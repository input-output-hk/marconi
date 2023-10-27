Marconi Key concepts
====================

We introduce here the key concepts of Marconi, how they are related and how they
map to the architecture.

Indexer
-------

The key component of Marconi is an ``indexer``.
An indexer can be any type that implements the ``IsIndex`` typeclass.
The definition of ``IsIndex`` is (very close to) this one:

.. code-block:: haskell

   class (Monad m) => IsIndex m event indexer where

     index
       :: (Eq (Point event))
       => Timed (Point event) (Maybe event)
       -> indexer event
       -> m (indexer event)

    -- …

    rollback :: (Ord (Point event)) => Point event -> indexer event -> m (indexer event)

Let's discuss the type variables first:

- ``m`` is the monadic context of the indexer.
- ``event`` defines the input type of the indexer, we usually store these
  events, but an indexer can also decide to process them and store a
  different type.
- ``indexer`` maintains the indexer state and provide the necessary information
  to perform the indexing operations.

The two key operations that we have to deal with are ``index`` and ``rollback``.
In a blockchain context, we call ``index`` when a new block comes in order to
index the event that corresponds to that block. ``rollback`` is called when the
node emit a rollback: some of the indexed blocks aren't aligned with the
blockchain consensus and we need to unindex them.

``index`` takes a ``Timed (Point event) (Maybe event)`` and the indexer that
provides the indexing context. ``Point event`` is a type family that defines how
we identify the point in time of the block that produces the `event`.
In the context of Cardano, a ``Point event`` will almost always be a
``ChainPoint``: the hash of the block (to identify a block)
and the slot at which the block was issued.
The ``event`` is wrapped in a ``Maybe`` to express the possibility that a block
may not contain any relevant information for a given indexer. It still important
to index a block with no relevant event to keep track of the progress of an
indexer. We'll come back on this later.

``rollback`` takes a point in time. This is the point to which we must rollback
the indexer. It means that we must put the indexer back to the state it was when
we indexed the block at the given point.

``IsIndex`` provides other functions such as ``indexAll`` and
``indexAllDescending`` that allows indexing of a list of events.
A default implementation is provided for these functions, and they can be
overriden to provide a more efficient implementation.

It also provides a ``setLastStablePoint`` method that allows the indexer to keep
track of the last stable point progress and, if needed, to react accordingly.

Coordinator and Workers
-----------------------

A common indexing scenario is to run several indexers in parallel to index
different parts of a same event. Ideally we want to organise our indexers as
a tree, each node doing a part of the processing before propagating the
resulting events, and the leaves being indexers that store the relevant part of
these events.

For example, you may want to have one subtree that focuses on the block
information, while another focuses on the transaction bodies,
or any other split that sounds relevant for your business logic.

In Marconi, the indexers can be grouped thanks to a ``Coordinator``.
A coordinator is a special type of indexer that propagates incoming actions
(index, rollback) to a list of indexers, and handles the lifecycle of these
indexers.

As Haskell is strongly typed, and as the indexer that we want to coordinate
may have different types, we need a way to handle an heterogeneous list of
indexers.

There are many different ways to deal with heterogeneous lists in Haskell. For
the coordinator, we decided to wrap the indexer in a type that hides the
implementation details of an indexer, a ``Worker``.
The type of a worker (slightly simplified) is the following:

.. code-block:: haskell

   data Worker input point = forall indexer event n.
     ( IsIndex n event indexer
     , Point event ~ point
     ) =>
     Worker
     { workerName :: Text
     -- ^ use to identify the worker in logs
     , workerState :: MVar (indexer event)
     -- ^ the indexer controlled by this worker
     , transformInput :: Preprocessor (ExceptT IndexerError m) point input event
     -- ^ adapt the input event givent by the coordinator to the worker type
     , hoistError :: forall a. n a -> ExceptT IndexerError IO a
     -- ^ adapt the monadic stack of the indexer to the one of the worker
     }

You don't need to understand this type declaration in details. Here are the
important bits:

- We hide the indexer representation under the worker type. Any concrete indexer
  type will work as long as it implements the ``IsIndex`` typeclass.
- The indexer is put in an ``MVar`` to allow access to it from other threads.
  We'll come back to it later.
- We provide a preprocessor, which can be seen as a functon that transforms
  the input send by the coordinator into events.
- ``hoistError`` ensures that we know how to translate the base monad of the
  indexer into ``ExceptT IndexerError IO`` which is the base monad of a worker.

Once we have a list of workers, we can use it to create a coordinator using the
``mkCoordinator`` function.
This function inialises each worker, creating a dedicated thread for each worker
where the worker will wait for incoming action to perform and notify the
coordinator when the action is performed.

The coordinator monitors each of the threads and, if one of the worker encounter
an error, it will try to close all the other workers nicely.

``Coordinator`` itself implements ``IsIndex`` and thus we can itself be wrapped
in a worker.
Thanks to it, we can create a whole hierarchy of indexers that can control from
a main coordinator.


Preprocessor
------------

We saw in the coordinator and workers section that workers take a preprocessor.
As stated in this section, the preprocessor type can be viewed as a stateful
function that transform the action sent to an indexer.
It's type is isomorphic to:

.. code-block:: haskell

   StateT s m ([ProcessedInput point a] -> [ProcessedInput point b])

It's the first time we encounter ``ProcessedInput`` so it is worth going through
its definition:


.. code-block:: haskell

   data ProcessedInput point event
     = Rollback point
     | Index (Timed point (Maybe event))
     | IndexAllDescending (NonEmpty (Timed point (Maybe event)))
     | StableAt point
     | Stop

It is mostly a reified version (functions expressed as data) of most of the
``IsIndex`` functions plus a ``Stop`` construct that allows us to stop a worker.
So the goal of a preprocessor is to take a list of actions that must be sent to
a worker and to transform this list.
It can be either to filter out some actions or to add some actions to the list,
based on the internal state ``s`` of the preprocessor.

Transformers
------------

Indexer transformers (or, shorter, transformers) are used to alter the behaviour
of an existing indexer.
The name comes from the monad transformers concept, and it was chosen because
monad and indexer transformers have similarities, as both aims at adding
capabilities to a base implementation.

A transformer carries a state and an underlying indexer.
The typeclass instances of the transformer can then add extra logic to this
underlying indexer.

For example the `WithCatchup` in `marconi-core` will prepare batch of events
when the blocks we receive are far from the tip to enable batch insertion of
events.

Preprocessor vs Transformers
----------------------------

There's a lot of similarities between preprocessor and transformers.
There are two major differences:

- ``Preprocessor`` state is not exposed and can't be accessed from the outside
  while ``Transformers`` can expose their state.
  It makes preprocessors slightly less powerful.
- ``Preprocessor`` doesn't rely on typeclasses implementation. As a consequence,
  they are easier to write and easier to compose in the general case.
  Furthermore, they don't change the type of the indexers.

Despite these differences, preprocessors and transformers are closely related
and in most scenarios, you should be able to rewrite a preprocessor as a
transformer or the opposite.

Tracking synchronisation progress
---------------------------------

In Marconi, each indexer is independent.
You can reuse data from another indexer in your application, add or remove
indexers from one run to another, without compromising the other indexers.
A consequence is that each indexer must track its synchronisation progress.
When there's a call on ``index`` the indexer keeps track of its last
synchronisation point.
When there's a call to ``setLastStablePoint``, it is supposed to keep track
of the given point as well.

In many scenarios, it is useful to have access to the last sync point and last
stable point of an indexer.
If you need to query different indexers for example, you may want to query them
at the same point in time, to ensure that you get consistent results.
When you restart your indexers, you may want to access their last stable point
to know at which blocks you must restart your synchronisation.

To expose this information, we usually implement the ``IsSync`` typeclass.
Implementing this typeclass is required to put an indexer in a worker.

Queryable
---------

The whole point of an indexer is to expose information about the event
they index.
In Marconi, it's done through the ``Queryable`` typeclass, which has the
following deifinition:

.. code-block:: haskell

   class Queryable m event query indexer where
     query
       :: (Ord (Point event))
       => Point event
       -> query
       -> indexer event
       -> m (Result query)

To define a ``Queryable`` instance, you need to provide the context ``m`` in
which it operates, the ``event`` that the indexer must handle to be able to
answer the query, the query type and the indexer implementation that can answer
the query.

Then, you need to implement the ``query`` method, which takes a point,
a ``query`` and an indexer to provide a ``Result query``.
``Result`` is a type family that associates a query to its result.
The ``point`` needed by query defines both a point that the indexer must have
reached and the upper bound of the result we consider, if applicable.
When you need to do several queries to different indexers, passing the same
point to the different queries ensures that the results are consistent.

In many situations, we just want access to the freshest information of an
indexer. In these scenarios, one can use the ``queryLatest`` function.
``queryLatest`` requires the indexer to implement both ``Queryable`` and
``IsSync``. It will get for you the last sync point and pass it to the query.
