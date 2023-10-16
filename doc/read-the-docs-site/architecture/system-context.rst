Marconi scope and context
=========================

Marconi falls in the category of chain-indexers.
Their responsability is to index in a database information from a blockchain in order to achieve fast access queries.
Some common queries are:

  * the UTxOs for a given Cardano address
  * the `Datum` (or `ScriptData`) given a `DatumHash`

This indexed information will be typically used by dApps (like Plutus appplications), and rarely by end-users directly.
The general system diagram for Marconi (or most chain-indexers) looks like:

.. uml::

    !include <C4/C4_Context>
    title System diagram for Marconi

    System_Ext(dApp, "dApp", "Application which runs a dApp provider's off-chain code")

    System_Ext(cardanoNode, "cardano-node", "Runs a Cardano node. Stores blocks on disk and allows clients to communicate with it using the N2C mini-protocols.")

    Rel(dApp, cardanoNode, "communicates with", "UNIX socket")

    System(marconi, "Marconi", "Indexes blockchain information in a database for fast access.")

    Rel(marconi, cardanoNode, "requests blocks/transactions from", "HTTP API or Haskell functions")
    Rel(dApp, marconi, "queries indexed data stored in")

    SystemDb(db, "Database", "Storage of blockchain data")
    Rel(marconi, db, "stores indexed data in")

Marconi (or any client applications for that matter) can interact with a `cardano-node` using the NTC (Node-To-Client) mini-protocols.
Read the `Cardano Docs <https://docs.cardano.org/explore-cardano/cardano-network/networking-protocol/>`_  for a more in-depth explaination.

With regards to chain-indexers, the most important mini-protocol is the chain-sync protocol.

.. note::
  Although, we also use the state query protocol in order to query information from the node such as the `K` parameter or the `EraHistory` (useful for converting a given POSIX or UTC time to a slot number for example.)

The chain-sync protocol is used for gettings blocks for the local node which is synchronizing itself with other Cardano nodes in the network.
Therefore, any client can read all the blocks from the local node starting from genesis all the way to the tip of the local node.

.. note::
  Note that it is possible to start from a point other than genesis.

After Marconi starts indexing the blocks, the data can be queries using:

  * Haskell directly
  * some transport protocol (local UNIX socket, HTTP, etc.).
