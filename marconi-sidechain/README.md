# marconi-sidechain

`marconi-sidechain` is a lightweight chain follower application for the Sidechain project to index and query specific information from the Cardano blockchain.

## Purpose

The purpose of ``marconi-sidechain`` is to encapsulate a subset of the indexers provided in [Marconi Chain Index](../marconi-chain-index) into a cohesive set which are required by the Sidechain team and which provides an interface to allow non-Haskell applications to query the indexed information.

## Interface

The interface for marconi-sidechain uses [JSON-RPC](http://www.simple-is-better.org/rpc/#differences-between-1-0-and-2-0) over HTTP built on top of [Marconi Chain Index](../marconi-chain-index/README.md).

```
             Running on a single machine                    Internet
+----------------------------------------------------+
|                                                    |                  +-------------+
|   +----------------+                +-----------+  |                  |             |
|   |                | node-to-client |           |  |     JSON-RPC     |  sidechain  |
|   |  cardano-node  +----------------+  marconi  +--+------------------+ application |
|   |                |      IPC       | sidechain |  |       HTTP       |             |
|   +----------------+                +----+------+  |                  +------------ +
|                                          |         |
|                                          |         |
|                                      +---+----+    |
|                                      | SQLite |    |
|                                      +--------+    |
|                                                    |
+----------------------------------------------------+
```

## Prerequisites

* [GHC](https://www.haskell.org/downloads/) (`==8.10.7`)
* [Cabal](https://www.haskell.org/cabal/download.html) (`>=3.4.0.0`)
* [Nix](https://nixos.org/download.html) (`>=2.5.1`)
  * Enable [IOHK's binary cache](https://iohk.zendesk.com/hc/en-us/articles/900000673963-Installing-Nix-on-Linux-distribution-and-setting-up-IOHK-binaries) or else you will build the world!
* [cardano-node](https://github.com/input-output-hk/cardano-node/releases/tag/1.35.4) (`==1.35.4`) running on preview testnet, pre-production testnet or mainnet

## How to build from source

### Cabal build

TODO

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

### JSON-RPC API Methods

#### echo

Healthcheck method to test that the JSON-RPC server is responding.

**Example**:

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

**JSON Schema request body**

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "jsonrpc": {
      "type": "string"
    },
    "method": {
      "type": "string"
    },
    "params": {
      "type": "string"
    },
    "id": {
      "type": "integer"
    }
  },
  "required": [
    "jsonrpc",
    "method",
    "params",
    "id"
  ]
}
```

**JSON Schema response**

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "id": {
      "type": "integer"
    },
    "jsonrpc": {
      "type": "string"
    },
    "result": {
      "type": "array",
      "items": [
        {
          "type": "string"
        }
      ]
    }
  },
  "required": [
    "id",
    "jsonrpc",
    "result"
  ]
}
```

**Example**:

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

Retrieves the block information from which the indexers are synced at.

It queries the UTXO indexer and doesn't return the last indexed chainpoint, but the one before.
The reason is that we want to use this query to find a sync point that is common to all the indexers
that are under the same coordinator.
Unfortunately, while the coordinator ensures that all the indexer move at the same speed,
it can't monitor if the last submitted block was indexed by all the indexers or not.

As a consequence, if the last chainpoint of the utxo indexer can, at most,
be ahead of one block compared to other indexers.
Taking the chainpoint before ensure that we have consistent infomation across all the indexers.

**JSON Schema request body**

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "jsonrpc": {
      "type": "string"
    },
    "method": {
      "type": "string"
    },
    "params": {
      "type": "string"
    },
    "id": {
      "type": "integer"
    }
  },
  "required": [
    "jsonrpc",
    "method",
    "params",
    "id"
  ]
}
```

**JSON Schema response**

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "id": {
      "type": "integer"
    },
    "jsonrpc": {
      "type": "string"
    },
    "result": {
      "type": "object",
      "properties": {
        "blockNo": {
          "_comment": "Not available yet",
          "type": "integer",
          "minimum": 0
        },
        "blockTimestamp": {
          "_comment": "Not available yet",
          "type": "string",
          "minimum": 0,
          "description": "timestamp in seconds"
        },
        "blockHeaderHash": {
          "type": "string",
          "pattern": "^[0-9a-f]{64}$"
        },
        "slotNo": {
          "type": "integer",
          "minimum": 0
        },
        "epochNo": {
          "_comment": "Not available yet",
          "type": "integer",
          "minimum": 0
        }
      },
      "required": [
        "blockNo",
        "blockTimestamp"
      ]
    }
  },
  "required": [
    "id",
    "jsonrpc",
    "result"
  ]
}
```

**Example**:

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

Retrieves UTXOs of a given address until a given point in time (measured in slots).

**JSON Schema request body**:

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "jsonrpc": {
      "type": "string"
    },
    "method": {
      "type": "string"
    },
    "params": {
      "type": "object",
      "properties": {
        "address": {
          "type": "string",
          "_comment": "Address encoded in the Bech32 format"
        },
        "createdAfterSlotNo": {
          "type": "integer",
          "minimum": 0,
          "description": "Filter out UTxO that were created during or before that slot."
        },
        "unspentBeforeSlotNo": {
          "type": "integer",
          "minimum": 0,
          "description": "Show only UTxOs that existed at this slot. Said another way, only outputs that were created during or before that slot and were unspent during that slot will be returned."
        }
      },
      "required": [
        "address",
        "unspentBeforeSlotNo"
      ]
    },
    "id": {
      "type": "integer"
    }
  },
  "required": [
    "jsonrpc",
    "method",
    "params",
    "id"
  ]
}
```

**JSON Schema response**:

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "id": {
      "type": "integer"
    },
    "jsonrpc": {
      "type": "string"
    },
    "result": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "blockHeaderHash": {
            "type": "string",
            "pattern": "^[0-9a-f]{64}$"
          },
          "slotNo": {
            "type": "integer",
            "minimum": 0
          },
          "blockNo": {
            "_comment: Not available yet",
            "type": "integer",
            "minimum": 0
          },
          "txIndexInBlock": {
            "_comment: Not available yet",
            "type": "integer",
            "minimum": 0
          },
          "address": {
            "type": "string"
          },
          "datum": {
            "type": "string",
            "description": "HEX string of the CBOR encoded datum"
          },
          "datumHash": {
            "type": "string",
            "pattern": "^[0-9a-f]{64}$"
          },
          "txId": {
            "type": "string"
          },
          "txIx": {
            "type": "integer",
            "minimum": 0
          },
          "spentBy": {
            "_comment: Not available yet",
            "type": "object",
            "properties": {
              "slotNo": {
                "type": "integer",
                "minimum": 0
              },
              "txId": {
                "type": "string",
                "pattern": "^[0-9a-f]{64}$"
              }
            },
            "required": [
              "slotNo",
              "txId"
            ]
          },
          "txInputs": {
            "_comment: Not available yet",
            "type": "array",
            "description": "List of inputs that were used in the transaction that created this UTxO",
            "items": {
              "type": "object",
              "properties": {
                "txId": {
                  "type": "string",
                  "pattern": "^[0-9a-f]{64}$"
                },
                "txIx": {
                  "type": "integer",
                  "minimum": 0
                }
              },
              "required": [
                "txId",
                "txIx"
              ]
            }
          },
        },
        "required": [
          "address",
          "blockHeaderHash",
          "blockNo",
          "datum",
          "datumHash",
          "slotNo",
          "txId",
          "txInputs"
          "txIx",
        ]
      }
    }
  },
  "required": [
    "id",
    "jsonrpc",
    "result"
  ]
}
```

**Example**:

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

#### getTxsBurningAssetId (NOT IMPLEMENTED YET)

Retrieves transactions that include a minting policy for minting/burning tokens until a given point in time (measured in slots).

**JSON Schema request body**:

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "jsonrpc": {
      "type": "string"
    },
    "method": {
      "type": "string"
    },
    "params": {
      "type": "object",
      "properties": {
        "policyId": {
          "type": "string",
          "pattern": "^[0-9a-f]{64}$",
          "description": "Hash of the minting policy"
        },
        "assetName": {
          "type": "string",
          "pattern": "^([0-9a-f]{2})+$"
        },
        "slotNo": {
          "type": "integer",
          "minimum": 0,
          "description": "Return the state of the chain at this slot. Effectively it filters out transactions that occured during or after this slot."
        },
        "afterTx": {
          "type": "string",
          "pattern": "^[0-9a-f]{64}$",
          "description": "Filters out transaction that occurred before this transaction. The specific transaction must be part of the indexed transactions."
        }
      },
      "required": [
        "policyId",
        "assetName",
        "slotNo",
      ]
    },
    "id": {
      "type": "integer"
    }
  },
  "required": [
    "jsonrpc",
    "method",
    "params",
    "id"
  ]
}
```

**JSON Schema response**:

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "id": {
      "type": "integer"
    },
    "jsonrpc": {
      "type": "string"
    },
    "result": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "blockHeaderHash": {
            "type": "string",
            "pattern": "^[0-9a-f]{64}$"
          },
          "slotNo": {
            "type": "integer",
            "minimum": 0
          },
          "blockNo": {
            "_comment: Not available yet",
            "type": "integer",
            "minimum": 0
          },
          "txId": {
            "type": "string"
          },
          "redeemer": {
            "type": "string",
            "pattern": "^([0-9a-f]{2})+$"
          },
          "redeemerHash": {
            "type": "string",
            "pattern": "^[0-9a-f]{56}$"
          },
          "burnAmount": {
            "type": "integer"
            "minimum": 0
          }
        },
        "required": [
          "blockHeaderHash",
          "slotNo",
          "blockNo",
          "txId",
          "redeemer",
          "redeemerHash",
          "burnAmount"
        ]
      }
    }
  },
  "required": [
    "id",
    "jsonrpc",
    "result"
  ]
}
```

**Example**:

Yet to come.

#### getStakePoolDelegationByEpoch

Retrieves the stake pool delegation per epoch.

**JSON Schema request body**:

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "jsonrpc": {
      "type": "string"
    },
    "method": {
      "type": "string"
    },
    "params": {
      "type": "integer",
      "minimum": 0,
      "description": "Epoch number"
    },
    "id": {
      "type": "integer"
    }
  },
  "required": [
    "jsonrpc",
    "method",
    "params",
    "id"
  ]
}
```

**JSON Schema response**:

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "id": {
      "type": "integer"
    },
    "jsonrpc": {
      "type": "string"
    },
    "result": {
      "type": "array",
      "items": {
        "type": "object",
        "properties": {
          "blockHeaderHash": {
            "pattern": "^[0-9a-f]{64}$",
            "type": "string"
          },
          "blockNo": {
            "minimum": 0,
            "type": "integer"
          },
          "slotNo": {
            "minimum": 0,
            "type": "integer"
          },
          "epochNo": {
            "minimum": 0,
            "type": "integer"
          },
          "poolId": {
            "type": "string"
          },
          "lovelace": {
            "minimum": 0,
            "type": "integer"
          }
        },
        "required": [
          "blockHeaderHash",
          "blockNo",
          "slotNo",
          "epochNo",
          "poolId",
          "lovelace"
        ]
      }
    }
  },
  "required": [
    "id",
    "jsonrpc",
    "result"
  ]
}
```

**Example**:

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

Retrieves transactions that include a minting policy for minting/burning tokens.

**JSON Schema request body**:

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "jsonrpc": {
      "type": "string"
    },
    "method": {
      "type": "string"
    },
    "params": {
      "type": "integer",
      "minimum": 0,
      "description": "Epoch number"
    },
    "id": {
      "type": "integer"
    }
  },
  "required": [
    "jsonrpc",
    "method",
    "params",
    "id"
  ]
}
```

**JSON Schema response**:

```JSON
{
  "$schema": "http://json-schema.org/draft-04/schema#",
  "type": "object",
  "properties": {
    "id": {
      "type": "integer"
    },
    "jsonrpc": {
      "type": "string"
    },
    "result": {
      "type": "object",
      "properties": {
        "blockHeaderHash": {
          "pattern": "^[0-9a-f]{64}$",
          "type": "string"
        },
        "blockNo": {
          "minimum": 0,
          "type": "integer"
        },
        "epochNo": {
          "minimum": 0,
          "type": "integer"
        },
        "slotNo": {
          "minimum": 0,
          "type": "integer"
        },
        "nonce": {
          "pattern": "^[0-9a-f]{64}$",
          "type": "string"
        }
      },
      "required": [
        "blockHeaderHash",
        "blockNo",
        "epochNo",
        "nonce",
        "slotNo"
      ]
    }
  },
  "required": [
    "id",
    "jsonrpc",
    "result"
  ]
}
```

**Example**:

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

[test-json-rpc.http](./examples/test-json-rpc.http) contains additional example usage.
