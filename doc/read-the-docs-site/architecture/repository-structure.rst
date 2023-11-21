Repository restructure
======================

Let's start by describing the different packages and relations between them.
Depending on the chain-indexer the user is trying to build, only a subset of the packages might be necessary.

.. uml::

    !include <C4/C4_Component>
    title Packages in the Marconi repository.

    Component(marconiCardanoChainIndex, "marconi-cardano-chain-index", "executable", "runs an executable which provides a set of commonly used indexers which can configured based on the user's needs")

    Component(marconiCardanoIndexers, "marconi-cardano-indexers", "Haskell library", "defines a set of reusable indexers for the Cardano blockchain")

    Component(marconiCardanoCore, "marconi-cardano-core", "Haskell library", "Cardano-specific specialisation of `marconi-core`")

    Component(marconiCoreJsonRpc, "marconi-core-json-rpc", "Haskell library", "wraps an indexer's query interface defined with `marconi-core` with a JSON-RPC HTTP server")

    Component(marconiCore, "marconi-core", "Haskell library", "chain-agnostic core library for building chain-indexers")

    Rel(marconiCardanoCore, marconiCore, "extends")

    Rel(marconiCoreJsonRpc, marconiCore, "extends")

    Rel(marconiCardanoIndexers, marconiCardanoCore, "creates indexers with")

    Rel(marconiCardanoChainIndex, marconiCardanoIndexers, "instanciates indexers in")
    Rel(marconiCardanoChainIndex, marconiCoreJsonRpc, "provides JSON-RPC HTTP query interface with")

`marconi-core` is the core library which defines the main buildings blocks that a user must use in order to build a chain-indexer in a chain-agnostic way.
The library does not depend on any Cardano-related libraries, and provides capabilities such as:

* indexing chain events into some backend storage (supported storages are `FileIndexer`, `ListIndexer` and `SQLiteIndexer`)
* batching events for faster syncing
* restarting from previously indexed points
* defining a query interface on top of any indexer
* running several independent indexers which index events at the same rate

.. note::
However, the user should expect some significant work to be done in order to adapt it for some blockchain.

`marconi-cardano-core` is also a core library which reexports everything from `marconi-core`, but specialises the API types and functions to Cardano-specific representations.
Particularly, it uses the types and functions defined in the Cardano core libraries: `cardano-api <https://github.com/input-output-hk/cardano-api>`_, `ouroboros-network <https://github.com/input-output-hk/ouroboros-network>`_, `ouroboros-consensus <https://github.com/input-output-hk/ouroboros-consensus>`_ and `cardano-ledger <https://github.com/input-output-hk/cardano-ledger>`_.
However, at the moment, `marconi-cardano-core` mostly uses `cardano-api` as it's a library that integrates the most useful parts, from a client application's perspective, of `ouroboros-network <https://github.com/input-output-hk/ouroboros-network>`_, `ouroboros-consensus <https://github.com/input-output-hk/ouroboros-consensus>`_ and `cardano-ledger <https://github.com/input-output-hk/cardano-ledger>`_.
In addition, `marconi-cardano-core` provides a runner for indexers ingesting events from the `chain-sync https://docs.cardano.org/explore-cardano/cardano-network/networking-protocol/`_ protocol, as well as some additions to `cardano-api`
in `cardano-api-extended`.

`marconi-core-json-rpc` is a core library which extends the query interface of `marconi-core` of an indexer so that is can be queried through a JSON-RPC HTTP server.
It uses the `servant` Haskell library, and particularly the `json-rpc` Haskell sub-library of `marconi-core-json-rpc`, to define the handlers of the different HTTP requests.

The `marconi-cardano-indexers` Haskell library contains indexers that were build for other projects, like the Sidechain Bridge Backend.
However, they were generalized enough that they can be reused for different use cases.
The goal for this package is to contain enough indexers so would satisfy the majority of use cases, and which user can simply reuse if they fit their needs.

The `marconi-cardano-chain-index` is the executable we provide for end-users who do *not* wish to building their own chain-indexer from scratch.
The executable makes the indexers in `marconi-cardano-indexers` available for anyone to use.
The query interface of each indexer is also wrapped behing a JSON-RPC HTTP server using `marconi-core`.

Which package should the user choose?
-------------------------------------

Users should typically use the `marconi-chain-index` executable if they:

* want to get started as quickly as possible
* don't know Haskell

However, if the information indexed by `marconi-chain-index` is not adequate for the user's use case **and** the user is familiar with Haskell, then they should build their own indexer using the other components.

The following graph is a decision tree that could help the user make an informed decision:

.. uml::

  @startuml
  (*) --> "You want to build your own chain-indexer"
  if "Are you indexing information on the  Cardano blockchain?" then
    -->[yes] "Use `marconi-cardano-core`" as mcc
  else
    -->[no] "Use `marconi-core`"
  endif

  --> if "Do you need JSON-RPC HTTP querying capabilities?" then
    --> "use `marconi-core-json-rpc`" as mcjr
  endif

  mcc --> if "Do you need JSON-RPC HTTP querying capabilities?" then
    --> "Use `marconi-core-json-rpc`" as mcjr
  endif

  --> if "Do you want to reuse or extend some predefined indexers?" then
    --> "Use `marconi-cardano-indexers`"
  endif

  --> (*)
  mcjr --> (*)
  @enduml
