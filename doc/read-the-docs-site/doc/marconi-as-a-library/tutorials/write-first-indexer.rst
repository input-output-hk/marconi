.. _my-first-indexer:

Write your first indexer
========================

In this tutorial, we'll show you how to write the simplest possible indexer, and how to expose the query interface of your indexer from a JSON-RPC server.

In order to understand the tutorial, you need to understand:

* Cardano blockchain (specifically `cardano-node <https://github.com/input-output-hk/cardano-node>`_)
* Haskell (mainly the `cardano-api <https://github.com/input-output-hk/cardano-api>`_, `lens <https://hackage.haskell.org/package/lens>`_, `sqlite-simple <https://hackage.haskell.org/package/sqlite-simple>`_ libraries)
* how chain-indexing works in general

The main building blocks of an indexer are:

1) the event: the information you want to extract from a block and store it
2) the instanciation of the indexer: particuarly the specification of the storage engine
3) the instanciation of the worker: in order to be able to run the indexer in a separate thread so that we can run several indexers simultaneously
4) the query interface: specifies the queries supported by the indexer and how to run the query against some storage

For reference, the import list is:

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART import
   :end-before: BLOCKEND import

Now, let's start with the definition of the event.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART event
   :end-before: BLOCKEND event

Given this event, we can instanciate the storage.
Let's start with the one that requires the least amount of code: the ``ListIndexer``.
The ``ListIndexer`` storage stores each event inside a ``Data.List``, and it is instanciated with ``Core.mkListIndexer``.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART ListIndexer
   :end-before: BLOCKEND ListIndexer

Once we have instanciated the backend with event type (``BlockInfoEvent``), we can wrap the indexer inside a ``Core.WorkerIndexer`` which is used to spin up a concurrent thread when reading blocks from the local Cardano node using the chain-sync protocol.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART ListIndexer worker
   :end-before: BLOCKEND ListIndexer worker

We now have a complete indexer which can be run with ``Core.runIndexer``.
Next, let's start by defining a runtime environment type which will contain all necessary inputs to run the indexer:

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART env
   :end-before: BLOCKEND env

With that environment type, let's create the function which runs the indexer.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART runBlockInfoIndexer
   :end-before: BLOCKEND runBlockInfoIndexer

Putting the final pieces together, we get:

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART runBlockInfoListIndexer
   :end-before: BLOCKEND runBlockInfoListIndexer

In order to run that you need to start a Cardano node which will produce a UNIX
socket file. That socket file is what Marconi will use to communicate with the
node.

Instructions on how to run a Cardano node are detailed `here <https://github.com/input-output-hk/cardano-node>`_ along with the list of Cardano networks.
However, in the Marconi Github repository, you can enter a Nix shell which provides the
`cardano-node` executable by doing::

  $ nix develop

Then, run the command which starts a Cardano node for the preprod network (a testnet)::

  cardano-node run --topology config/cardano-node/preprod/topology.json \
      --socket-path ~/cardano-node/preprod/cardano-node.socket \
      --config config/cardano-node/preprod/config.json \
      --database-path ~/cardano-node/preprod/db

The topology and config files are copied in the `config` directory of the `Marconi`
Github repository.
The socket and database file paths should be defined somewhere in your system.

Finally, we can run `Marconi` using the socket file path with::

  cabal run marconi-doc -- ~/cardano-node/preprod/cardano-node.socket 1

.. note::
  For reference, the network IDs of public Cardano networks are:
     * 1: preprod
     * 2: preview
     * 3: sanchonet

You should get an output that looks like::

  [marconi-tutorial:Info:1] [2023-10-16 12:14:26.87 UTC] The starting point for the chain-sync protocol is ChainPointAtGenesis
  [marconi-tutorial:Info:9] [2023-10-16 12:14:26.88 UTC] Starting from ChainPointAtGenesis.
  [marconi-tutorial:Info:9] [2023-10-16 12:14:36.88 UTC] Synchronising (2.42%). Current synced point is ChainPoint(Slot 1011389, BlockHash 797ff6324ed7049e263014d0845596b1da60699c47f74b17432580a61222166c) and current node tip is ChainTip(Slot 41775270, BlockHash b77475adc22489a33a223f94cfb85255a14afd76d6ed830d68a2a791df220734, BlockNo 1491171). Processed 46258 blocks and 1 rollbacks in the last 10s (4626 blocks/s).
  [marconi-tutorial:Info:9] [2023-10-16 12:14:46.88 UTC] Synchronising (4.70%). Current synced point is ChainPoint(Slot 1962692, BlockHash 960127359b2703f4055994f42e24872f8713b8e5ed6fbcca2f76704ccea07f7a) and current node tip is ChainTip(Slot 41775270, BlockHash b77475adc22489a33a223f94cfb85255a14afd76d6ed830d68a2a791df220734, BlockNo 1491171). Processed 47393 blocks and 0 rollbacks in the last 10s (4739 blocks/s).
  [marconi-tutorial:Info:9] [2023-10-16 12:14:56.88 UTC] Synchronising (6.87%). Current synced point is ChainPoint(Slot 2871998, BlockHash 5accb16e4c9c4ee4f163720bed7fbc1883b4aeb5b30260cb051c280dac6a2e97) and current node tip is ChainTip(Slot 41775270, BlockHash b77475adc22489a33a223f94cfb85255a14afd76d6ed830d68a2a791df220734, BlockNo 1491171). Processed 45397 blocks and 0 rollbacks in the last 10s (4540 blocks/s).
  ...

SQLiteIndexer backend
---------------------

The previous code used the ``Core.ListIndexer`` which requires the least amount of code to write, but keeps all the events in memory.
Therefore, this backend should only be used for testing purposes.

For production purposes, especially on Cardano mainnet, the ``Core.SQLiteIndexer`` should be used.
In this part, we show the changes that need to be made in order to use it.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART SQLiteIndexer
   :end-before: BLOCKEND SQLiteIndexer

Finally, we can now define the main function to run it which reuses functions defined in the ``Core.ListIndexer`` part.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART runBlockInfoSqliteIndexer
   :end-before: BLOCKEND runBlockInfoSqliteIndexer

There you go.
You now have a complete end-to-end example on how to build Marconi indexers.
In the next part of the tutorial, we'll show you how to define the query interface of the indexer, along with exposing it on a JSON-RPC HTTP server.
