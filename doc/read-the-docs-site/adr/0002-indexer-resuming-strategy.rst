.. _adr6:

ADR 6: Indexer resuming strategy
================================

Date: 2023-07-26

Authors
-------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Draft

Context
-------

When building a Marconi indexer, you need to provide the points from which you can resume from.
Typically, the latest point will be used to bootstrap the node-to-client chain-sync protocol.

However, the user will sometimes want to run *multiple* indexers in parallel.
In that scenario, the user will need to provide a list of common points across the different indexers as input to the node-to-client chain-sync protocol.

The initial implementation would have ``resumeFromStorage`` return *all* points that the indexer can resume from inside a list.
The issue is that most indexers *can* resume from *any* point starting from genesis up until the point they have indexed to.
The result is a ``resumeFromStorage`` that can return millions of points and a significant amount of time to run because the results need to be sorted in descending order.

Given this performance issue, we want to define an efficient resuming strategy which satifies the following general goals:

* fast resuming
* low hardware resource consumption (CPU and memory)
* does not require indexer to re-index data they have already indexed
* indexers are *always* in a consistent state. They *must* delete any indexed information in points that have been rollbacked even if the rollback happens when the indexers are stopped.

Decision
--------

* We will revert the return type of ``resumeFromStorage`` from ``StorablePoint h`` to ``[StorablePoint h]``.

* We will change the ``resumeFromStorage`` implementaton of existing indexers that can resume from *any* point in time so that they return a limited set of resumable points.
  More specifically, ``securityParam * onDiskBufferRatio + 1`` worth of points.
  In the current Marconi interface, it is actually just ``securityParam + 1`` as we assume that all rollbackable blocks *can* be fully stored on disk because of the disk flush.
  We assume that all the data contained in the indexer is consistent for all the point returned by an indexer.

* We will start a single node chain-sync client for all indexers instead of one node chain sync client per indexer.

* We will implement the following resuming logic.

  In order to start the chain-sync client, we need to provide to provide intersection points (or resuming points).
  Intersection points represent the points from which we want to start syncing from the local node.
  We will provide as intersection points the points returned from the following pseudo-code:

  .. code-block:: haskell

    -- | Query the resumable points of each indexer then select the resumable
    -- points of the indexer that has the lowest immutable point. If two indexer
    -- have the same lowest immutable point, select the one which has the lowest
    -- overall point (immutable or not).
    selectIntersectionPoints indexers = do
      resumingPoints <- mapM queryResumingPoints indexers
      head $ sortOn (\rps -> (maximum $ filter isImmutablePoint rps, maximum rps)) resumingPoints

  Next, we need a function which processes each ``ChainSyncEvent`` returned by the chain-sync client for each indexer.
  The pseudo-code is as follows:

  .. code-block:: haskell

    -- Preconditions:
    --   * The 'initialLastSyncPoints' param should be sorted in ascending order on SlotNo
    --   * Each element of the 'initialLastSyncPoints' param should satisfy the property '\(point, nextPoint) -> getBlockNo point == getBlockNo (succ point) + 1'.
    --     where `(point, nextPoint)` is any two consecutive elements in the list `initialLastSyncPoints`.
    applyChainSyncEvent initialLastSyncPoints previousChainSyncEventPoint event indexer = do
      case event of
        Resume point -> do
          maybePoint <- find (\p -> getSlotNo p == getSlotNo point) initialLastSyncPoints
          case maybePoint of
            Nothing ->
              pure () -- Do nothing
            Just _ ->
              update initialLastSyncPoints $ filter (\p -> getSlotNo p > getSlotNo point)
        RollBackward point -> do
          rewind point indexer
        RollForward block point -> do
          case initialLastSyncPoints of
            -- Processed all initial last sync points. The indexer is in sync.
            -- So we just index the block.
            [] -> do
              insert block indexer

            -- The lowest last sync point is the same as the current chain event point.
            -- We drain the event and remove the last sync point from the initial last sync points.
            (lowestLastSyncPoint:restOfLastSyncPoints) | point == lowestLastSyncPoint -> do
                update initialLastSyncPoints $ \_ -> restOfLastSyncPoints

            -- The lowest last sync point is not the same as the current chain event point, but they
            -- have the same slot number.
            -- We rewind to the previous point, insert the block and remove all points from the
            -- initial last sync point list.
            -- We drain the event and remove the last sync point from the initial last sync points.
            (lowestLastSyncPoint:_) | getSlotNo point == getSlotNo lowestLastSyncPoint -> do
                rewind previousChainSyncEventPoint indexer
                insert block indexer
                update initialLastSyncPoints $ \_ -> []

            -- The chain sync event slot number is higher than the lowest last sync point of the
            -- indexer. Should not happen though. We get in this case statement IIF we started the
            -- chain-sync client with a resuming point that is higher than this indexer's resuming
            -- point.
            (lowestLastSyncPoint:_) | getSlotNo point > getSlotNo lowestLastSyncPoint -> do
                error "The point of the new block is higher than the lowest last sync point of the indexer. That means a bug in the resuming point selection provided for the chain-sync client."

            -- The indexer is still more up-to-date than the ChainSyncClient event. Drain the event.
             _ -> do
                pure ()

  It is important to note that the implementation of ``applyChainSyncEvent`` highly depends on the implementation of ``selectIntersectionPoints``.

Argument
--------

The reason ``resumeFromStorage`` returns a list of resuming points where there is *at least* one immutable point is to ensure we can resume from an existing chain point when restarting Marconi.
Then, the reason why we include rollbackable points in the list of resuming points is simply to resume from a point close to the tip, thus prevent re-indexing of already indexed information.

The main drawback of the solution is that each indexer must keep track of the last sync points (although all solutions written thus far relied on this assumption).
We provide a way to create the table implicitly when using the SQLite adapter.
However, if the user wants to use a different db, then he will need to rewrite the lastSyncPOints logic in the database.
However, we assume that most user will want to use the provided adapters, and not write indexers in databases that are not supported yet.

In order to justify the decision, we will present various use case scenarios and show how the new implementation works.
We assume two indexers: ``A`` and ``B`` which have started indexing information, and then were **abruptly** stopped.
The use cases will show what will happen when resuming them.

We use the notation ``[x..y]`` to define the resumable interval.
Also note that we use the operator ``-`` for calculating the difference between two intervals.
For example, ``[1..3] - [2..4] == [1..1]`` and ``[1..3] - [5..10] == [1..3]``.
In Haskell, that would look something like:

  .. code-block:: haskell

    Set.fromList [1..3] `Set.difference` Set.fromList [2..4] == [1..1]
    Set.fromList [1..3] `Set.difference` Set.fromList [5..10] == [1..3]

``A`` has resumable interval outside of the rollbackable chain point interval
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

                   Rollbackable
                     |-----|
  1 2 3 4 5 6 7 8 9 10 11 12
    |---|                  |
      A                   Tip

``A``'s resumable interval provided for the chain-sync protocol is ``[4, 3, 2]``.
The chain-sync protocol is started at point ``4``, thus ``A`` starts syncing from point ``5`` with no lost data.

``A`` has a resumable interval overlapping the rollbackable chain point interval
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

             Rollbackable
              |------|
  1 2 3 4 5 6 7 8 9 10
            |--------|
                A   Tip

Supposing we have a ``securityParam`` of ``4``.
``A``'s resumable interval provided for the chain-sync protocol is ``[10, 9, 8, 7, 6]``.
Reminder that we return a maximum of ``securityParam + 1`` number of resumable points for any indexer.
The chain-sync protocol will try each of these points and identify the first one which is known by the local node.
As rollbacks can occur between points ``[7..10]`` after the indexer was stopped, the points ``[7..10]`` provided by the indexer *could* be invalid.
Thus, if any of those rollbackable points fail, we can at least guaranty that the chain-sync protocol will resume at point ``6``.
That is unless the node database was deleted and the node re-sync did not get past point ``6`` in a scenario such as:

::

  1 2 3 4 5 6 7 8 9 10 11 12 13
      |     |--------|
     Tip        A

In that case, we will just log a runtime error saying that the provided resuming points don't appear in the chain.
Then, the user will have to wait for the node to sync up.

``A`` and ``B`` are resuming at different points
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

                      Rollbackable
                        |-----|
  1 2 3 4 5 6 7 8 9 10 11 12 13
    |---|                     |
      B                      Tip
  |---|
    A

Given our resuming point selection, the resumable interval provided for the chain-sync protocol is ``[3, 2, 1]``.
``A`` will start syncing from point ``4``, while ``B`` will drain/ignore points ``3`` and ``4`` and start syncing from point ``5``.

Let's take a similar scenario:

::

                      Rollbackable
                        |-----|
  1 2 3 4 5 6 7 8 9 10 11 12 13
    |---|                     |
      B                      Tip
  |---------|
       A

Given our resuming point selection, the resumable interval provided for the chain-sync protocol is ``[4, 3, 2]``.
``B`` will start syncing from point ``5``, while ``A`` will drain/ignore points ``5`` and ``6`` and start syncing from point ``7``.

Alternative solutions
---------------------

Make every indexer restart from the oldest common point shared between indexers
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This was our initial implementation, where each indexer would return all possible points it can resume from, and we would identify the intersection between those points as the resuming points.
However, the drawbacks were:

* slow resuming because of the query of the resuming points of each indexer which also augmented
  memory usage
* in the case where indexer ``A`` is faster than ``B``, ``A`` would have to rollback to ``B``'s point, thus having to re-index already indexed information

Each indexer has it's own chain-sync client
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Alternatively, each indexer can execute it's own chain-sync client with it's own intersection points.
This approach would solve all major issues with resuming.
However, after some experimentation, we noticed some significant slowdown of syncing time compared to using a single chain-sync client.
We didn't investigate why that happens exactly.
In any case, we decided to not go forward with this solution in the foreseable future, but we might re-explore it eventually.
