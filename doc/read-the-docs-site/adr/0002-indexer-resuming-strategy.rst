.. _adr6:

ADR 6: Indexer resuming strategy
================================

Date: 2023-12-12

Authors
-------

berewt <nicolas.biri@iohk.io>

Status
------

Draft

Context
-------

When we start Marconi, we need to define a starting point to the node, and each
indexers must be able to restart from this point.
While we usually try to keep the indexers synchronised. We wanted to be able to
add new indexers without having to resynchronised the existing ones.

Initially, each indexer was sending a list of points that it can resume from,
but it was inefficient and led to a complex implementation.

We want to define an efficient resuming strategy which satifies the following general goals:

* fast resuming
* low hardware resource consumption (CPU and memory)
* does not require indexer to re-index data they have already indexed
* indexers are *always* in a consistent state. They *must* delete any indexed information in points that have been rollbacked even if the rollback happens when the indexers are stopped.

Decision
--------

* On restart, each indexer will restart from the last stable point it has
  indexed, removing any indexed volatile information.

* To decide where to (re)start the chain-sync protocol, we will take the minimum
  of all the last stablepoints provided by the different indexers and take the
  minimal (older) one.

* Using a preprocessor (``Resume``), each indexer will ignore events that are
  before its last stable synchronisation point.
  Once it reaches this last synchronisation point, the indexer will resume its
  synchronisation and process the newcoming events.

* To keep track of the stable point, it's the application responsability to call
  a dedicated function ``setLastStablePoint`` on the different indexers.

* It's the indexer responsability to ignore new stable points that are past its
  current indexing point, if any.

* We will start a single node chain-sync client for all indexers instead of one node chain sync client per indexer.

Argument
--------

Our different implementations of the resuming so far tried to restart at thhe
latest common synchronisation points of all the indexers, even if this point was
a volatile one.
The consequence of it was a complex logic to (1) find a valid sync point, (2)
deal with the potential rollbacks once the chainpoint was chosen.

Sticking to stable blocks drastically simplified the resuming logic, as we don't
have to handle rollback that happened while the indexer was offline.
The counterpart is that we potentially need to reindex volatile blocks, but we
decided that redoing this work isn't too expensive compared to the logic
simplification that it introduces.

Alternative solutions
---------------------

Take the last synchronisation point as a starting point for each indexer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Our previous implementation. The resuming logic was highly complex, hard to
maintain and explain, because we needed to handle potential rollbacks that would
happen while the indexer was offline.

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
