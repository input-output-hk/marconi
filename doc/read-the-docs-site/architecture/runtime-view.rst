Runtime view
============

There are two runtime perspectives in a Marconi application:

* user querying
* indexing

They will typically run on separate threads (or separate processes) in a Marconi application.
In the followings sections, we will describe the runtime behavior of each of those perspectives.

Indexing view
-------------

In this section, we'll show the indexing runtime view of a Marconi application.
Note that the indexing is mainly handled by the `marconi-core` and `marconi-cardano-core` packages.

.. uml::

    !include <C4/C4_Dynamic>
    title Marconi indexing runtime view

    Container_Ext(datasource, "Datasource", "", "Represents some external data source that Marconi will connect to")

    Container_Boundary(marconiContainer, "Marconi application") {
      Component(sourceReader, "Input source reader", "Haskell component", "Module that processes data from the external datasource and transforms it to a [ProcessedInput]")
      Component(preprocessor, "Preprocessor", "Haskell component", "Applies some preprocessing function to the [ProcessedInput]")
      Component(transformer, "Transformer", "Haskell component", "Applies some function to the incoming ProcessedInput and forwards them to the indexer")
      Component(indexer, "Indexer", "Haskell thread", "Indexes the relevant parts of the ProcessedInput to some storage")
      ContainerDb(marconiDb, "Marconi storage", "Storage backend of the indexer")
    }

    Rel(datasource, sourceReader, "sends some input data")
    Rel(sourceReader, preprocessor, "sends '[ProcessedInput]'")
    Rel(preprocessor, transformer, "sends preprocessed '[ProcessedInput]'")
    Rel(transformer, indexer, "forwards each 'ProcessedInput'")
    Rel(indexer, marconiDb, "stores events in the indexer storage")

The input source reader provides an interface to fetch data from any datasource: a local Cardano node, Ogmios, cardano-db-sync, etc.
However, the input data needs to be mapped to a ``ProcessedInput``.
The ``ProcessedInput`` is sent to the preprocessor which applies some preprocessing function on the inputs.
The preprocessed inputs are sent to the transformer which also transforms the inputs, and sends them to the indexer.
Finally, the indexer will index them to it's storage backend.

.. note::

  See the :ref:`core-building-blocks` page for a general understanding of concepts like ``Preprocessor``,  ``Transformer``, ``Indexer``, etc..

As an example of an input source reader, here is a runtime view of a Cardano node source reader.

.. uml::

    !include <C4/C4_Dynamic>
    title Cardano node source reader runtime view

    ContainerDb(blockchain, "Blockchain")
    Container_Ext(cardanoNode, "Local Cardano node")

    Container_Boundary(marconiContainer, "Marconi application") {
      Component(sourceReader, "Local node source reader", "Haskell component", "Module that processes data from the local node and transforms it to a [ProcessedInput]")
    }

    Rel(blockchain, cardanoNode, "sends blocks to")
    Rel(cardanoNode, sourceReader, "sends chain-sync event")

The local Cardano node constructs a local state of the Cardano blockchain by fetching blocks from its peers and storing it on disk.
Then, node clients can request for blocks by the using the node's chain-sync mini-protocol.
The chain-sync protocol essentially streams events to the client which have the following structure:

.. code-block:: haskell

  data ChainSyncEvent a
    = RollForward a C.ChainTip
    | RollBackward C.ChainPoint C.ChainTip
    deriving (Show, Functor, Generic)

where ``a`` can be anything including a block.

.. note::

   For an overview on the different Cardano node mini-protocols, see this `Cardano Docs page <https://docs.cardano.org/explore-cardano/cardano-network/networking-protocol/>`_. For a more in-depth explanation, read the `Ouroboros network specification <https://input-output-hk.github.io/ouroboros-network/pdfs/network-spec/network-spec.pdf>`_.

Now, let's assume that we want to run multiple indexers that process inputs at the same rate, and that a subset of indexers are queryable from the a client application.
Running multiple indexers that process inputs at the same rate are managed by a ``Coordinator``.
The runtime view would look like:

.. uml::

    !include <C4/C4_Dynamic>
    title Marconi indexing runtime view with multiple indexers

    Container_Ext(datasource, "Datasource")
    Container_Ext(dApp, "dApp")

    Container_Boundary(marconiContainer, "Marconi application") {
      Component(sourceReader, "Input source reader", "Haskell component", "Module that processes data from the external datasource and transforms it to a [ProcessedInput]")
      Component(coordinator, "Coordinator", "Haskell component", "Combines multiple indexers so that they can process inputs at the same rate")
      Component(indexerA, "Indexer A", "Haskell component")
      Component(indexerB, "Indexer B", "Haskell component")
      Component(indexerC, "Indexer C", "Haskell component")
      Component(queryAggregateD, "Read-only indexer D", "Haskell component", "Read-only indexer which doesn't store anything, but aggregates results of multiple indexers.")
    }

    Rel(datasource, sourceReader, "sends some input data")
    Rel(sourceReader, coordinator, "sends input")
    Rel(coordinator, indexerA, "forwards input")
    Rel(coordinator, indexerB, "forwards input")
    Rel(coordinator, indexerC, "forwards input")
    Rel(queryAggregateD, indexerA, "queries")
    Rel(queryAggregateD, indexerB, "queries")

    Rel(dApp, indexerC, "queries")
    Rel(dApp, queryAggregateD, "queries")

In this view, we show that the ``Coordinator`` can combine multiple indexers.
The ``Coordinator``'s role is to forward the incoming inputs to each of the indexer and make sure each indexer has processed the input before moving to the next one.

Then, we have the querying part.
Each indexer actually defines two things:

* how to index the given input
* how to query the indexed information

Therefore, dApps will simply interact directly with the indexers when needing to query some information.

.. note::

  In reality, the indexer will need to be wrapped in an ``MVar`` and the ``MVar`` is passed to the indexing thread and the query thread.
  The indexing thread will update the ``MVar``, while the query thread will read the ``MVar``.

However, some queries can only be responded by aggregating the query result of multiple indexers.
This is why we have ``QueryAggregate`` components which don't index anything, but simply aggregates the query result of multiple indexers by accessing their internal state.

User indexer querying view
--------------------------

In this section, we'll show the querying runtime view of a Marconi application which contains at least one indexer.
The Marconi indexer querying is typically done in a separate thread from the indexing itself.
We're assuming that the indexers have fully consumed the input stream and are ready to be queried.

.. note::

   You can start querying the indexer even if it did not *fully* consume the input stream.
   However, you will get out of date information until it has synced with the actual current tip of the local node.

.. uml::

    !include <C4/C4_Dynamic>
    title User querying runtime view

    Container_Ext(dApp, "dApp")

    Container_Boundary(marconiContainer, "Marconi application") {
      Component(indexer, "Indexer", "Haskell thread")
      Component(httpServer, "HTTP server", "Haskell thread")
      ContainerDb(marconiDb, "Marconi storage")

    }

    Rel(dApp, httpServer, "sends a HTTP request")
    Rel(httpServer, indexer, "queries indexer")
    Rel(indexer, marconiDb, "queries storage")
    Rel(marconiDb, indexer, "sends result to indexer")
    Rel(indexer, httpServer, "sends result to HTTP request handler")
    Rel(httpServer, dApp, "sends a HTTP response")

In this view, the dApp interacts with the HTTP server that wraps the query interface of the indexer (or indexers).

.. note::

  If there are multiple indexers, the HTTP server will need to redirect the request to the correct indexer.
  That redirection should be implemented by the Marconi application.

The indexer receives the request, queries it's backend storage, and sends the result to the HTTP server.
The HTTP server simply forwards the result to the dApp with an HTTP response.
