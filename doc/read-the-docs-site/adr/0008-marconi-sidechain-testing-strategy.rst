.. _adr8:

ADR 8: Marconi-Sidechain Testing Strategy
===============================

Date:

Authors
-------

james-iohk <james.browning@iohk.io>

Status
------

Draft

Context
-------

Being the primary customer-facing application, Marconi-Sidechain requires a comprehensive testing process to ensure its overall quality.
It will undergo regular updates and enhancements to meet evolving customer needs, which exposes the risk of regresions in its functionality and accepted performance.
Consequently, an ongoing process is necessary to validate the compatibility and performance of new features or modifications.

Decision
--------

* We will design and implement tests at the following levels: Unit, Integration and End-to-end.
  - Unit tests are run locally on individual functions and modules.
  - ? Integration tests are run locally on integration of modules ? (is there any useful distinction between integration and e2e tests in Marconi?)
  - End-to-end tests are run on private or public testnets with a real cardano node and network

FOR UNIT TESTING:

* We will test indexers and filters with as many positive and negative combinations as possible using data-driven and property tests.

* We will not attempt to test the CLI or RPC interface at unit level because we already have some sanity tests being run on commit on private testnet for fast feedback.

* We will plan for and produce tests alongside implementation of new product features.

FOR END-TO-END/INTEGRATION TESTING:

* We will write tests in Haskell where possible so these tests easily integrate into existing infrastructure.
  Only use other tools if there are strong benefits.

* We will plan test coverage and also aim to produce them alongside implementation of new product features.
  It is acceptable to have separate tasks to produce the tests but them must be complete within the same PI cycle.

* We will sometimes perform exploratory testing on major features, such as new RPC methods or cli options.
  This can provide faster feedback in lieu of automation testing being produced.
  Should not be carried out by the engineer who implemented the feature (due to assumption bias).

* We will produce a small number of sanity checks to be to be executed on PR commit and run on a private testnet (using cardano-testnet) which will be primed with a basic dataset not intended for performance evaluations or querying in a complex environment.
  These checks primarily focus on core functionality:
   
  CLI-1. that the application starts correctly using each of the the cli options (mandatory and optional)
    - Mandatory flags: node config path, node socket path, marconi db directory, magic id
    - Optional flags: address filter and asset id filter

  CLI-2. local validation correctly catches invalid input, such as incorrectness of address formats for all cli options that take an input and a useful error is shown. 

  CLI-3 query version has correct commit hash

  RPC-1. that input validation catches each error scenario with useful feedback
         ? use property tests to randomise invalid inputs ?:
    1. `getUtxosFromAddress``
      a. invalid format address (e.g. pubkeyhash instead of Bech32)
      b. unsupported address (e.g. stake address)
      c. address not included in the --address-to-index
      d. createdAfterSlotNo higher than current slot
      e. createAfterSlotNo and unspentBeforeSlotNo not a natural number
      f. unspentBeforeSlotNo value larger than createdAfterSlotNo
    
    2. `getBurnTokensEvents`
      a. invalid policyId hash
      b. invalid afterTx id format
      c. policyId not included in --match-asset-id filter
      d. policyId is included in --match-asset-id filter but the assetName is not
      e. afterTx value is not an existing burn txId (e.g. use one that only mints)
 
    3. `getNonceByEpoch`
      a. epochNo in future
      b. epochNo in byron era
      c. epochNo not a natural number
    
    4. `getActiveStakePoolDelegationByEpoch`
      a. epochNo not a natural number
      b. epochNo in future

    5. `getActiveStakePoolDelegationByEpoch`
      a. epochNo not a natural number
      b. epochNo in future
  
  RPC-2. sanity check each RPC method for a valid query to prove validation doesn't block correct use (response data is not important)
    - ? Could property test be used to randomise valid queries?

  RPC-3. check that only mandatory fields are required for each method

  RPC-4. ? `getCurrentSyncedBlock` test pre-fully-synced response ?
    
* We will produce tests for the RPC API query filters to be run on public testnets (including mainnet) because there is more interesting transaction data on these networks and so test the queries more broadly.
  These will be executed nightly on each of the POST /json-rpc methods
  These should be run either after a sync test or on an instance of the process under test that is always running fully synced.
    1. `getCurrentSyncedBlock`
      a. check for valid values in result object (e.g. presence and bytestring length)
    
    2. `getUtxosFromAddress`
      a. check at least one unspent and one spent utxo for correct key attributes
      b. use createdAfterSlotNo and unspentBeforeSlotNo together to find a specfic utxo
      c. query using more than one address included in the --address-to-index filter
      d. query address when no --address-to-index filter is used
  
    3. `getBurnTokensEvents`
      a. query using more than one policyId= and assetName pair included in the --match-asset-id filter
      b. query policyId when no --match-asset-id is used
      c. use slotNo and afterTx together to find a specific burn event
    
    4. `getNonceByEpoch`
      a. check for valid values in result object (e.g. known correct nonce)
    
    5. `getActiveStakePoolDelegationByEpoch`
      a. check for valid values in result object(e.g. check known delegation of a particular epoch remains consistent)
    
    6. query performance test using specific periods of interesting data from each network. (e.g. the Alonzo hype on mainnet around around block 6535793)
      a. (option) use a tool like artillery on each query method for a know subset of data. this allows us to:
        - easily configure simulation of load ramping and number of users
        - generate reports for each test run with useful measurements to compare over time

* We will not have any end-to-end tests that query maroni's db because this does not always store relevant data and it does not resemble how the user should be querying.

* We will produce sync performance tests to be run on each public network: preview, preprod and mainnet. (See ADR 7: Marconi observability for details)

Argument
--------

Alternative solutions
---------------------
When testing marconi-chain-index we can take a similar aproach but must also:
  - test starting the application with the CLI's disable flags:
    - end-to-end test to check absence of db file being created after run
    - property test to randomise combinations
  - test RPC for useful error responses when querying for disabled filters
