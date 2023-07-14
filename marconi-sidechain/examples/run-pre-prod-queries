#!/usr/bin/env sh

echo "marconi-sidechain --help"
cabal run marconi-sidechain:exe:marconi-sidechain -- --help
echo ""

echo "============================================="
echo "== unknownMethod"
echo "============================================="
echo ""

echo "curl --silent -d '{"jsonrpc": "2.0" , "method": "unknownMethod" , "params": "", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0" , "method": "unknownMethod" , "params": "", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq


echo "============================================="
echo "== getTargetAddresses"
echo "============================================="
echo ""

echo "curl --silent -d '{"jsonrpc": "2.0" , "method": "getTargetAddresses" , "params": "", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0" , "method": "getTargetAddresses" , "params": "", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq


echo "============================================="
echo "== getCurrentSyncedBlock"
echo "============================================="
echo ""

echo "curl --silent -d '{"jsonrpc": "2.0" , "method": "getCurrentSyncedBlock" , "params": "", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0" , "method": "getCurrentSyncedBlock" , "params": "", "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""


echo "============================================="
echo "== getUtxosFromAddress"
echo "============================================="
echo ""


# The cardano-db-sync query to get addresses with the most utxos:
# select address, count(*) as c from tx_out, tx, block where tx_out.tx_id = tx.id and tx.block_id = block.id group by address order by c desc limit 1;

echo "curl --silent -d '{"jsonrpc": "2.0" , "method": "getUtxosFromAddress" , "params": { "address": "addr_test1vz09v9yfxguvlp0zsnrpa3tdtm7el8xufp3m5lsm7qxzclgmzkket" }, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0" , "method": "getUtxosFromAddress" , "params": { "address": "addr_test1vz09v9yfxguvlp0zsnrpa3tdtm7el8xufp3m5lsm7qxzclgmzkket", "unspentBeforeSlotNo": 30000000 }, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""

echo "curl --silent -d '{"jsonrpc": "2.0" , "method": "getUtxosFromAddress" , "params": { "address": "addr_test1vz09v9yfxguvlp0zsnrpa3tdtm7el8xufp3m5lsm7qxzclgmzkket", "createdAfterSlotNo": 16052985, "unspentBeforeSlotNo": 18364224}, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0" , "method": "getUtxosFromAddress" , "params": { "address": "addr_test1vz09v9yfxguvlp0zsnrpa3tdtm7el8xufp3m5lsm7qxzclgmzkket", "createdAfterSlotNo": 16052985, "unspentBeforeSlotNo": 18364224}, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""


echo "============================================="
echo "== getBurnTokenEvents"
echo "============================================="
echo ""

# The cardano-db-sync query to get burn events:
# select multi_asset.policy, multi_asset.name, block.slot_no, tx.hash from tx, ma_tx_mint, multi_asset, redeemer, block where tx.id = ma_tx_mint.tx_id and multi_asset.id = ma_tx_mint.ident and redeemer.tx_id = tx.id and tx.block_id = block.id limit 1;

echo "curl --silent -d '{"jsonrpc": "2.0", "method": "getBurnTokenEvents", "params": {"policyId": "e2bab64ca481afc5a695b7db22fd0a7df4bf930158dfa652fb337999", "assetName": "53554d4d495441574152445344656669", "slotNo": 11639233}, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0", "method": "getBurnTokenEvents", "params": {"policyId": "e2bab64ca481afc5a695b7db22fd0a7df4bf930158dfa652fb337999", "assetName": "53554d4d495441574152445344656669", "slotNo": 11639233}, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""

echo "curl --silent -d '{"jsonrpc": "2.0", "method": "getBurnTokenEvents", "params": {"policyId": "e222ae950cef1915dcb9db8840dc9c3df3785f9a10eca30dfb84ad40"}, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0", "method": "getBurnTokenEvents", "params": {"policyId": "e222ae950cef1915dcb9db8840dc9c3df3785f9a10eca30dfb84ad40"}, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""

echo "curl --silent -d '{"jsonrpc": "2.0", "method": "getBurnTokenEvents", "params": {"policyId": "e222ae950cef1915dcb9db8840dc9c3df3785f9a10eca30dfb84ad40", "slotNo": 10944386}, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0", "method": "getBurnTokenEvents", "params": {"policyId": "e222ae950cef1915dcb9db8840dc9c3df3785f9a10eca30dfb84ad40", "slotNo": 10944386}, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""

# This one doesn't work yet, as afterTx is ignored for now.
echo "curl --silent -d '{"jsonrpc": "2.0", "method": "getBurnTokenEvents", "params": {"policyId": "e222ae950cef1915dcb9db8840dc9c3df3785f9a10eca30dfb84ad40", "afterTx": "0a872c4fcf87f041caab5d5ecaeae19fd0e26de14241167c8dee7d1b26b5b4f7"}, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0", "method": "getBurnTokenEvents", "params": {"policyId": "e222ae950cef1915dcb9db8840dc9c3df3785f9a10eca30dfb84ad40", "afterTx": "0a872c4fcf87f041caab5d5ecaeae19fd0e26de14241167c8dee7d1b26b5b4f7"}, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""


echo "============================================="
echo "== getNonceByEpoch"
echo "============================================="
echo ""

echo "curl --silent -d '{"jsonrpc": "2.0" , "method": "getNonceByEpoch" , "params": 3, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0" , "method": "getNonceByEpoch" , "params": 3, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""

echo "curl --silent -d '{"jsonrpc": "2.0" , "method": "getNonceByEpoch" , "params": 4, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0" , "method": "getNonceByEpoch" , "params": 4, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""


echo "============================================="
echo "== getActiveStakePoolDelegationByEpoch"
echo "============================================="
echo ""

echo "curl --silent -d '{"jsonrpc": "2.0" , "method": "getActiveStakePoolDelegationByEpoch" , "params": 7, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0" , "method": "getActiveStakePoolDelegationByEpoch" , "params": 7, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""

echo "curl --silent -d '{"jsonrpc": "2.0" , "method": "getActiveStakePoolDelegationByEpoch" , "params": 31, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq"
curl --silent -d '{"jsonrpc": "2.0" , "method": "getActiveStakePoolDelegationByEpoch" , "params": 31, "id": 1}' -H 'Content-Type: application/json' -X POST http://localhost:8080/json-rpc | jq
echo ""

