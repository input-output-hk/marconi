.. _adr8:

ADR 8: Marconi-Sidechain Testing Strategy
=========================================

Date: 11/07/2023

Authors
-------

james-iohk <james.browning@iohk.io>

Status
------

Draft

Context
-------

A general approach to functional testing Marconi-Sidechains covering all test levels.
The scope of this document is `marconi-sidechain`, not other Marconi applications such as `marconi-chain-index`.

Decision
--------

* We will design and implement tests at the following levels:
    * Unit tests are run locally on individual functions and modules.
    * Integration tests can be easily run locally and on commit using cardano-testnet for a private testnet (soon to use the node emulator).
    * End-to-end tests are run on a public network with a real cardano node. These can be cumbersome to setup and run due requiring a fully synced cardano-node and funded wallet, and are slow to execute due to block propagation. Therefore, we will only run automated end-to-end tests nightly.

* We will write tests in Haskell where possible so these tests easily integrate into existing infrastructure.
  Only use other tools if there are strong benefits.

* We will produce test reports for each run which will be hosted publicly in Allure format.
  In lieu of a Haskell adapter for Allure, `tasty-test-reporter` can be used.
  It is not important to retain previous reports triggered by nightly or for PR commits runs.
  Reports produced for a release should be hosted indefinitely.

UNIT TESTING:

* We will test indexers and filters with as many positive and negative combinations as possible using data-driven and property tests.

* We will not attempt to test the CLI or RPC interface at unit level because we already have some sanity tests being run on commit on private testnet for fast feedback.

* We will plan for and produce tests alongside implementation of new product features.

INTEGRATION TESTING:

* We will plan test coverage and also aim to produce them alongside implementation of new product features.
  It is acceptable to have separate tasks to produce the tests but them must be complete within the same PI cycle.

* We will produce sanity tests to be to be executed on PR commit and run on a private testnet (using cardano-testnet) which will be primed with a basic dataset not intended for performance evaluations or querying in a complex environment.
  These tests should only check the essential functionality of each feature.
  See https://github.com/input-output-hk/marconi/issues/93 for checklist of test cases.

END-TO-END TESTING:

* We will plan test coverage and also aim to produce them alongside implementation of new product features.
  It is acceptable to have separate tasks to produce the tests but them must be complete within the same PI cycle.

* We will sometimes perform exploratory testing on major features, such as new RPC methods or cli options.
  This can provide faster feedback in lieu of automation testing being produced.
  Should not be carried out by the engineer who implemented the feature (due to assumption bias).
    
* We will produce tests for each of the RPC API methods to stress the filters
  These tests will assert against known correct response content.
  These tests will ideally be run on mainnet because there is more interesting transaction data there and so are more likely to catch an edge-case.
  These tests should be run either after a sync test or on an instance of the process under test that is always running fully synced.
  See RPC-1 in https://github.com/input-output-hk/marconi/issues/93 for checklist of test cases.

* We will produce tests for the RPC API methods to cover a board set of response data comparing against a source of truth
  These tests will compare against results produced by `cardano-db-sync <https://github.com/input-output-hk/cardano-db-sync>`_
  These tests will ideally be run on mainnet
  see RPC-2 in https://github.com/input-output-hk/marconi/issues/93 for checklist of test cases.

* We will not have any end-to-end tests that query maroni's db because this does not always store relevant data and it does not resemble how the user should be querying.

* We will produce sync performance tests to be run on each public network: preview, preprod and mainnet. (See ADR 7: Marconi observability for details)

* We will produce RPC query performance tests using specific periods of interesting data from public networks. (See ADR 7: Marconi observability for details)

Argument
--------

Being the primary customer-facing application, Marconi-Sidechain will undergo regular updates and enhancements to meet evolving customer needs, which exposes the risk of regressions in its functionality and accepted performance.

Consequently, an ongoing process is necessary to validate the compatibility and performance of new features or modifications whilst verifying that legacy features remain working correctly.

Alternative solutions
---------------------
When testing marconi-chain-index we can take a similar approach but must also:
  - test starting the application with the CLI's disable flags:
    - end-to-end test to check absence of db file being created after run
    - property test to randomise combinations
  - test RPC for useful error responses when querying for disabled filters
