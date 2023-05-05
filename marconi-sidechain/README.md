# marconi-sidechain

`marconi-sidechain` is a lightweight chain follower application for the Sidechain project to index and query specific information from the Cardano blockchain.
The interface for querying the indexed information uses [JSON-RPC](https://www.jsonrpc.org/specification) over HTTP.

See the [architecture documentation](./doc/ARCHITECTURE.adoc) for more information on how this application was build.

## Prerequisites

If using `Nix`:

* [Nix](https://nixos.org/download.html) (`>=2.5.1`)
  * Enable IOHK's binary cache or else you will build the world! Refer to [this section](../CONTRIBUTING.adoc#how-to-get-a-shell-environment-with-tools) on how to achieve this.

If *not* using `Nix`:

* [GHC](https://www.haskell.org/downloads/) (`==8.10.7`)
* [Cabal](https://www.haskell.org/cabal/download.html) (`>=3.4.0.0`)
* [cardano-node](https://github.com/input-output-hk/cardano-node/releases/tag/1.35.4) (`==1.35.4`) running on preview testnet, pre-production testnet or mainnet

## How to build from source

### Cabal build

TBD

### Cabal+Nix build

To build `marconi-sidechain` from the source files, use the following commands:

```sh
git clone git@github.com:input-output-hk/marconi.git
nix develop
cabal clean && cabal update # Optional, but makes sure you start clean
cabal build marconi-sidechain
```

The above process will build the executable in your local environment at this location:

```sh
cabal exec -- which marconi-sidechain
```

Or you can run the executable directly with:

```sh
cabal run marconi-sidechain:exe:marconi-sidechain -- --help
```

### Nix build

The `marconi-sidechain` executable is available as a nix flake.

If inside the `marconi` repository, you can run from the top-level:

```
$ nix build .#marconi-sidechain
```

Or you may run from anywhere:

```
$ nix build github:input-output-hk/marconi#marconi-sidechain
```

Both commands will produce a `result` directory containing the executable
`result/bin/marconi-sidechain`.

## Command line summary

Run `marconi-sidechain`, `$(cabal exec -- which marconi-sidechain) --help` or `cabal run marconi-sidechain:exe:marconi-sidechain -- --help` for a general synopsis of the command line options depending on your installation method.

See [this automatically generated golden file](./test/Spec/Golden/Cli/marconi-sidechain___help.help) for the up-to-date help command output.

## How to run

We are assuming that:

* you have a local running cardano-node instance.
* you've set the following environment variables:
  * `CARDANO_NODE_SOCKET_PATH`: socket path of your local cardano-node instance
  * `MARCONI_DB_DIRECTORY`: directory in which to create the various SQLite database files

The most minimal way to run the executable is as follows:

```sh
$(cabal exec -- which marconi-sidechain) \
    --testnet-magic 1 \
    --socket-path "$CARDANO_NODE_SOCKET_PATH" \
    --db-dir "$MARCONI_DB_DIRECTORY" \
```

This command will do two things:

* from the last chainpoint (if none, from genesis), fetch blocks from the local node, extract required data and index them in the database.
* run a JSON-RPC server which will listen for any queries on the indexed data.

Using the `--addresses-to-index`, you can instruct `marconi-sidechain` to index target addresses.
By default, all addresses are indexed in the database.

Some example addresses from pre-production-testnet are:

```
addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc \
addr_test1vp8cprhse9pnnv7f4l3n6pj0afq2hjm6f7r2205dz0583egagfjah \
addr_test1wpzvcmq8yuqnnzerzv0u862hmc4tc8xlm74wtsqmh56tgpc3pvx0f \
addr_test1wrn2wfykuhswv4km08w0zcl5apmnqha0j24fa287vueknasq6t4hc \
addr_test1wr9gquc23wc7h8k4chyaad268mjft7t0c08wqertwms70sc0fvx8w \
```

## Querying JSON-RPC server

There is single endpoint from which the client can send requests using a `POST` request: `/json-rpc` (or `http:localhost:3000/json-rpc`)

The body of HTTP request must contain a JSON of the following format:

```json
{ "jsonrpc": "2.0"
, "method": "<METHOD>"
, "params": "<PARAMETERS>"
, "id": 0
}
```

The `id` field should be a random ID representing your request. The response will have that same ID.

### JSON-RPC API method examples

All of the following example are actual results from the Cardano pre-production testnet.

#### echo

Healthcheck method to test that the JSON-RPC server is responding.

```sh
$ curl -d '{"jsonrpc": "2.0", "method": "echo", "params": "", "id": 0}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 0,
  "jsonrpc": "2.0",
  "result": []
}
```

#### getTargetAddresses

Retrieves user provided addresses.

Assuming the user started the `marconi-sidechain` executable with the address `addr_test1qz0ru2w9suwv8mcskg8r9ws3zvguekkkx6kpcnn058pe2ql2ym0y64huzhpu0wl8ewzdxya0hj0z5ejyt3g98lpu8xxs8faq0m` as the address to index.

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getTargetAddresses" , "params": "", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result": ["addr_test1qz0ru2w9suwv8mcskg8r9ws3zvguekkkx6kpcnn058pe2ql2ym0y64huzhpu0wl8ewzdxya0hj0z5ejyt3g98lpu8xxs8faq0m"]
}
```

#### getCurrentSyncedBlock (PARTIALLY IMPLEMENTED)

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getCurrentSyncedBlock" , "params": "", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result": {
      "blockHeaderHash": "6161616161616161616161616161616161616161616161616161616161616161",
      "slotNo": 1
  }
}
```

#### getUtxosFromAddress (PARTIALLY IMPLEMENTED)

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getUtxosFromAddress" , "params": { "address": "addr_test1vz09v9yfxguvlp0zsnrpa3tdtm7el8xufp3m5lsm7qxzclgmzkket", "unspentBeforeSlotNo": 100000000 }, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result":
    [
        {
          "address": "addr_test1vz09v9yfxguvlp0zsnrpa3tdtm7el8xufp3m5lsm7qxzclgmzkket",
          "blockHeaderHash": "6b1c0c2ccd1fec376235c6580a667b67be92028e183dc46236eb551f1c40d621",
          "datum": null,
          "datumHash": null,
          "slotNo": 86480,
          "txId": "a00696a0c2d70c381a265a845e43c55e1d00f96b27c06defc015dc92eb206240",
          "txIx": 0
        }
    ]
}
```

#### getTxsBurningAssetId

Example yet to come.

#### getStakePoolDelegationByEpoch

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getStakePoolDelegationByEpoch" , "params": 6, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result":
    [
        {
            "blockHeaderHash": "578f3cb70f4153e1622db792fea9005c80ff80f83df028210c7a914fb780a6f6",
            "blockNo": 64903,
            "epochNo": 6,
            "lovelace": 100000000000000,
            "poolId": "pool1z22x50lqsrwent6en0llzzs9e577rx7n3mv9kfw7udwa2rf42fa",
            "slotNo": 1382422
        },
        {
            "blockHeaderHash": "578f3cb70f4153e1622db792fea9005c80ff80f83df028210c7a914fb780a6f6",
            "blockNo": 64903,
            "epochNo": 6,
            "lovelace": 100000000000000,
            "poolId": "pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6",
            "slotNo": 1382422
        },
        {
            "blockHeaderHash": "578f3cb70f4153e1622db792fea9005c80ff80f83df028210c7a914fb780a6f6",
            "blockNo": 64903,
            "epochNo": 6,
            "lovelace": 100000000000000,
            "poolId": "pool174mw7e20768e8vj4fn8y6p536n8rkzswsapwtwn354dckpjqzr8",
            "slotNo": 1382422
        }
    ]
}
```

#### getNonceByEpoch

```sh
$ curl -d '{"jsonrpc": "2.0" , "method": "getNonceByEpoch" , "params": 4, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:3000/json-rpc | jq
{
  "id": 1,
  "jsonrpc": "2.0",
  "result":
    {
        "blockHeaderHash": "fdd5eb1b1e9fc278a08aef2f6c0fe9b576efd76966cc552d8c5a59271dc01604",
        "blockNo": 21645,
        "epochNo": 4,
        "nonce": "ce4a80f49c44c21d7114d93fe5f992a2f9de6bad4a03a5df7e7403004ebe16fc",
        "slotNo": 518400
    }
}
```

### Other documentation

See [test-json-rpc.http](./examples/test-json-rpc.http) for additional example usages.

See [API](./doc/API.adoc) for the full API documentation.
