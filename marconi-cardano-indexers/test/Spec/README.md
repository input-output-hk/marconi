# Writing Db-Sync Comparison Tests

These are instructions on how to write tests to the `Spec.Marconi.Cardano.DbSyncComparison.hs` test suite.

Db-Sync Comparison tests are tests which compare the behavior of `marconi` to the behavior of `cardano-db-sync`,
where `cardano-db-sync` is the one providing the standard expected behavior.

We have provided a framework which should make these kinds of tests easier to write and as lightweight as possible for CI.

## Sub-Chain Snapshots

We decided to avoid running `cardano-node` directly in CI in order to avoid the large resource overhead and complications which would arise from such a solution.
Therefore, our solution is to serialise parts of the block-chain (sub-chains) to disk. These are what we call _snapshots of the chain_, or just _snapshots_. The tests
run on these snapshots.

### Remark: snapshots are generated before-hand, and added to the environment by TODO.

For the purpose of the Db-Sync Comparison tests we have generated 9 snapshots, each containing the blocks from the first 10 epochs from each Cardano era. [This table](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0059/feature-table.md) was used as reference.

## How to add a Db-Sync Comparison test

For the same reason as above, we cannot run `cardano-db-sync` in CI, but we can store its outputs to specific queries in our repository as golden files.

This section will explain the steps required to add a new comparison test, for any new indexer `I` with an associated query `Q`.

1. Run `cardano-db-sync` locally, and research how queries similar to `Q` can be created for `cardano-db-sync`. You can find the instructions for doing this [here](https://input-output-rnd.slack.com/docs/T0N639Z4N/F06AXV0BUSZ).
2. You will have to write a module similar to `Test.Marconi.Cardano.DbSyncComparison.BlockInfoResult.hs`. Note that the expected result from Db-Sync should be serialised to JSON.
3. Add your tests to `Spec.Marconi.Cardano.DbSyncComparison.hs`.
