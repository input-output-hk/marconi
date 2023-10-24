How to query your first indexer
===============================

In the :ref:`previous tutorial <my-first-indexer>`, we showed how to build and run a Marconi indexer.
Now, we'll show how to query the indexed information.

In order to query the indexer, there are 2 parts:

1) Create an instance of the ``Core.Queryable`` typeclass with your event type
2) (Optional) Use that instance as the handler of your HTTP server in order to expose the query outside of Haskell.

Let's start with 1).

The first thing to do is to define the datatype which will represent the request and response of the query.
For our example, let's add the query capability of getting the ``SlotNo`` given a ``BlockNo``.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART query
   :end-before: BLOCKEND query

ListIndexer query
-----------------

Next, we need to define a ``Core.Queryable`` instance for that query datatype and for a target backend, the ``Core.ListIndexer``.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART ListIndexer query
   :end-before: BLOCKEND ListIndexer query

The ``Point BlockInfoEvent`` param is necessary in the scenario where there is more than one indexer and each run at different speeds.
Then, Marconi can use that parameter to make sure that the result are consistent accross the different query calls by using, for example, the lowest synced point across indexers.

So now, we have defined the supported queries on the indexer.
Now, let's expose this in a JSON-RPC HTTP server.
To do so, we use the `servant-server` and the `json-rpc` library, as well as `marconi-core-json-rpc` to help us define the `Servant` handlers.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART HTTP
   :end-before: BLOCKEND HTTP

With the implemented API, we can now use this in our new main function.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART runBlockInfoListIndexerHttp
   :end-before: BLOCKEND runBlockInfoListIndexerHttp

In short, the indexing runs in a different thread from the HTTP server.
They share a `MVar` which contains the indexer.
The indexing thread writes to that `MVar` and the query thread reads that `MVar`.

After running that function, we can finally query the indexer.
Here is an example:

::

  $ curl -d '{"jsonrpc": "2.0" , "method": "getBlockInfoFromBlockNoQuery" , "params": 500, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/api | jq
  {
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
      "_event": {
        "blockNo": 500
      },
      "_point": {
        "blockHash": "06741f76ba87e9c9d0e06f18ecaa844a1a97a528e6145f20607
        "slot": 95480,
        "tag": "ChainPoint"
      }
    }
  }

SQLiteIndexer query
-------------------

Now, let's define the necessary changes to use the ``Core.SQLiteIndexer`` backend instead of the ``Core.ListIndexer``.

First the definition of the ``Queryable`` instance.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART SQLiteIndexer query
   :end-before: BLOCKEND SQLiteIndexer query

Then, a similar main function to the ``Core.ListIndexer`` one.

.. literalinclude:: BasicApp.hs
   :language: haskell
   :start-after: BLOCKSTART runBlockInfoSqliteIndexerHttp
   :end-before: BLOCKEND runBlockInfoSqliteIndexerHttp
