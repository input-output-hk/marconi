.. _adr6:

ADR 6: Indexer resuming strategy
================================

Date: 2023-06-07

Authors
-------

koslambrou <konstantinos.lambrou@iohk.io>

Status
------

Proposed

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

* We will have each indexer start the node-to-client chain-sync protocol at different points in time.
  However, the ``Coordinator`` will make sure that all indexers are synced until a common point, and advance at the same speed (i.e. they can only request the next block once all the indexers have finished processing the current block).

* We will use the full set of points provided by ``resumableFromStorage`` of an indexer as resuming points for the node-to-client chain-sync protocol.
  These points need to ordered in descending order so that protocol can priotise the selection of the latest ones.

Argument
--------

In order to justify the decision, we will present various use case scenarios and show how the decisions satisfies them.
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
            |--------------|
  1 2 3 4 5 6 7 8 9 10 11 12
    |---|                  |
      A                   Tip

``A``'s resumable interval provided for the chain-sync protocol is ``[4, 3, 2]``.
The chain-sync protocol is started at point ``4``, thus ``A`` is rewinded to point ``4``.

``A`` has a resumable interval fully included in the rollbackable chain point interval
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

                 Rollbackable
                |-------------|
  1 2 3 4 5 6 7 8 9 10 11 12 13
                |----|        |
                   A         Tip

``A``'s resumable interval provided for the chain-sync protocol is ``[10, 9, 8]``.
The chain-sync protocol will try each of these points and identify the first one which is known by the local node.
As rollbacks can occur between points ``[8..13]`` after the indexer was stopped, the points ``[8..9]`` provided by the indexer *could* be invalid.
Thus, if any of those points fail, the chain-sync protocol will just start from genesis.

``A`` has a resumable interval overlapping the rollbackable chain point interval
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

             Rollbackable
                |----|
  1 2 3 4 5 6 7 8 9 10
            |--------|
                A   Tip

Supposing we have a ``securityParam`` of ``3``.
``A``'s resumable interval provided for the chain-sync protocol is ``[10, 9, 8, 7]``.
Reminder that we don't return ``6`` because ``A`` will return ``securityParam + 1`` number of points.
The chain-sync protocol will try each of these points and identify the first one which is known by the local node.
As rollbacks can occur between points ``[8..13]`` after the indexer was stopped, the points ``[8..10]`` provided by the indexer could be invalid.
Thus, if any of those rollbackable points fail, we can guaranty that the chain-sync protocol will start at point ``7``.
That is unless the node database was deleted and the node re-sync did not get past point ``7`` in a scenario such as:

::

  1 2 3 4 5 6 7 8 9 10 11 12 13
      |     |--------|
     Tip        A

``A`` and ``B`` are resuming at different points
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

                 Rollbackable
                |-------------|
  1 2 3 4 5 6 7 8 9 10 11 12 13
    |-|                       |
     B                       Tip
  |---------|
       A

``A`` and ``B``'s resumable interval provided for the chain-sync protocol is ``[6, 5, 4, 3, 2, 1]`` and ``[3, 2]`` respectively.
The coordinator will block syncing of ``A`` until ``B`` reaches the same point (point ``6``).
Then, both indexers can only process the next block once the other has finished processing the current block.

Alternative solutions
---------------------

Single shared node-to-client chain-sync protocol
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

This was our initial implementation.
We started a single node-to-client chain-sync protocol and then the ``Coordinator`` would pass the ``ChainSyncEvent`` to all indexers.
Once *all* indexers have finished processing the event, the ``Coordinator`` would fetch the next ``ChainSyncEvent`` and propagate it to all indexers.

The major issue with this solution is that, for multiple indexers, they don't always share the same resumable point.
Here's an example:

::

                 Rollbackable
                |-------------|
  1 2 3 4 5 6 7 8 9 10 11 12 13
      |     |--------|        |
      |         A            Tip
      B

Since they don't share any resumable points, all of the indexers are restarted from genesis (losing all data they previously indexed).

A possible extension would have been to start the chain-sync protocol from the lowest point (``3``), and more up-to-date indexers would ignore/drain already indexed chain sync events.
However, such a solution fails when the resumable point of ``A`` is a rollbackable point (such as ``10`` in our example).
``A`` would ignore/drain all ``ChainSyncEvent`` until point ``10``.
However, that point is rollbackable and could possibly be invalid (by having a different block header hash) when restarting the indexer.
For example, let's say ``A`` was stopped at point ``10``, then the node was rollbacked to point ``8`` , and then the node continued syncing until point ``13``.
In that scenario, resuming the indexer from point ``10`` would not yield an error, but it will put the indexer into an inconsistent state with regards to the data that it has indexed.

Of course, that problem would not occur if ``resumeFromStorage`` would only return the largest point that is outside the rollbackable interval.
That would imply that the indexer needs to be aware of the current node tip in order to derive latest immutable point.
However, we think that it should *not* be of concern to the user writing an indexer, and removing rollbackable points should be done outside the indexer.
The user should *not* worry about that.

Alternatively, we could implement an indexer which keeps tracks of the latest immutable chain point in order to know if a point is rollbackable or not.
Then, we could start the chain-sync protocol with the ``A``'s resumable interval by filtering out points *after* the immutable chain point.
Therefore, the last resumable point of ``A`` would be ``8``.
The main problem with this approach is that it adds an additionnal overhead for the user to run such an indexer.
Therefore, we decided to not go with this approach for the time being.
