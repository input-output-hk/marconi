-- | Helpers for testing the various JsonRpc endpoints.
module Test.Marconi.Cardano.ChainIndex.Api.JsonRpc where

import Cardano.Api qualified as C
import Control.Lens ((^.))
import Data.List qualified as List
import Data.List.NonEmpty qualified as NEList
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.MintBurnToken (
  BurnTokenEventResult (BurnTokenEventResult),
  GetBurnTokenEventsResult (GetBurnTokenEventsResult),
 )
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Types (
  AddressUtxoResult (txId, txIx, value),
  GetUtxosFromAddressResult (unAddressUtxosResult),
 )
import Marconi.Cardano.ChainIndex.Api.JsonRpc.Endpoint.Utxo.Wrappers (ValueWrapper (unValueWrapper))
import Marconi.Cardano.Indexers.MintTokenEvent qualified as MintTokenEvent
import Marconi.Cardano.Indexers.Utxo qualified as Utxo

{- | Create uniform actual/expected results from the GetUtxosFromAddressResult query, for comparison
 - with equality. This is to give better counterexample reporting with '==='.
   It is the caller's job to ensure the query in fact did use the provided address, since that is not in the result.
   Utxos are considered equal here if they are associated with the same address (assumed),
   have the same @C.'TxIn'@ and the same 'value'.
-}
uniformGetUtxosFromAddressResult
  :: C.AddressAny
  -> GetUtxosFromAddressResult
  -> [Utxo.UtxoEvent]
  -> ([(C.TxIn, C.Value)], [(C.TxIn, C.Value)])
uniformGetUtxosFromAddressResult target result inputs = (sortUniqueOnTxIn actual, sortUniqueOnTxIn expected)
  where
    sortUniqueOnTxIn = List.sortOn fst . List.nub
    actual = map (\x -> (C.TxIn (txId x) (txIx x), unValueWrapper $ value x)) $ unAddressUtxosResult result
    blockUtxosToExpected :: Utxo.UtxoEvent -> [(C.TxIn, C.Value)]
    blockUtxosToExpected = map (\x -> (x ^. Utxo.txIn, x ^. Utxo.value)) . NEList.filter (\x -> x ^. Utxo.address == target)
    expected = concatMap blockUtxosToExpected inputs

{- | Create uniform actual/expected results from GetBurnTokenEventsResult and compare on
fields of BurnTokenEventResult except slotNo, blockHeaderHash and isStable.
-}
uniformGetBurnTokenEventsResult
  :: GetBurnTokenEventsResult
  -> [MintTokenEvent.MintTokenEvent]
  -> ( [(C.BlockNo, C.TxId, C.AssetName, C.Quantity, Maybe MintTokenEvent.MintAssetRedeemer)]
     , [(C.BlockNo, C.TxId, C.AssetName, C.Quantity, Maybe MintTokenEvent.MintAssetRedeemer)]
     )
uniformGetBurnTokenEventsResult (GetBurnTokenEventsResult result) inputs = (List.sort actual, List.sort expected)
  where
    actual = map mintTokenToUniform inputs
    expected = map resultToUniform result
    mintTokenToUniform
      :: MintTokenEvent.MintTokenEvent
      -> (C.BlockNo, C.TxId, C.AssetName, C.Quantity, Maybe MintTokenEvent.MintAssetRedeemer)
    mintTokenToUniform e =
      ( e ^. MintTokenEvent.mintTokenEventLocation . MintTokenEvent.mintTokenEventBlockNo
      , e ^. MintTokenEvent.mintTokenEventLocation . MintTokenEvent.mintTokenEventTxId
      , e ^. MintTokenEvent.mintTokenEventAsset . MintTokenEvent.mintAssetAssetName
      , -- NOTE: The query handler converts negative values to positive ones, since it considers burn events only.
        -e ^. MintTokenEvent.mintTokenEventAsset . MintTokenEvent.mintAssetQuantity
      , e ^. MintTokenEvent.mintTokenEventAsset . MintTokenEvent.mintAssetRedeemer
      )
    resultToUniform
      :: BurnTokenEventResult
      -> (C.BlockNo, C.TxId, C.AssetName, C.Quantity, Maybe MintTokenEvent.MintAssetRedeemer)
    resultToUniform (BurnTokenEventResult _ _ blockNo tid rh r assetName burnAmount _) =
      (blockNo, tid, assetName, burnAmount, MintTokenEvent.MintAssetRedeemer <$> r <*> rh)
