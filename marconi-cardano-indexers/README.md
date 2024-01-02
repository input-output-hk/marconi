# marconi-cardano-indexers

This package contains a set of indexers for Cardano that can be reused in any
marconi-indexing project.

## Available indexers

- [`BlockInfo`](src/Marconi/Cardano/Indexers/BlockInfo.hs): get the starting
  slot, the creation time and the epoch number of a block.
- [`ChainTip`](src/Marconi/Cardano/Indexers/BlockInfo.hs) get the slot and block
  header hash of the chain tip of the node your synchronising against.
- [`Datum`](src/Marconi/Cardano/Indexers/Datum.hs): get all the datum on the
  chain with their hash.
- [`EpochNonce`](src/Marconi/Cardano/Indexers/EpochNonce.hs) get the epoch Nonce used
  at each epoch.
- [`EpochSDD`](src/Marconi/Cardano/Indexers/EpochSDD.hs) get the stake pool
  delegation at each epoch.
- [`MintTokenEvent`](src/Marconi/Cardano/Indexers/MintTokenEvent.hs) get all the
  minting and/or burning events.
- [`Spent`](src/Marconi/Cardano/Indexers/Spent.hs) get all the spent Tx outputs
  with the transaction they were spent in.
- [`Utxo`](src/Marconi/Cardano/Indexers/Spent.hs) get all the utxos that were
  created.

Note that both `EpochNonce` and `EpochSDD` require that you maintain a ledger
state (that can be obtained using an
[`ExtLedgerStateCoordinator`](src/Marconi/Cardano/Indexers/Spent.hs)), which is
quite memory and CPU expensive, especially on mainnet.
