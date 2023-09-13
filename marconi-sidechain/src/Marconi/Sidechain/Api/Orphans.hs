{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.Sidechain.Api.Orphans where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.BaseTypes qualified as Ledger
import Control.Lens ((&), (.~), (?~))
import Data.OpenApi (
  NamedSchema (NamedSchema),
  OpenApiType (OpenApiObject),
  ToSchema (declareNamedSchema),
  declareSchemaRef,
 )
import Data.OpenApi.Lens (properties, required, type_)
import Data.Proxy (Proxy (Proxy))
import Marconi.ChainIndex.Legacy.Indexers.Utxo (Utxo)
import Marconi.ChainIndex.Legacy.Orphans ()
import Marconi.ChainIndex.Legacy.Types (TxIndexInBlock (TxIndexInBlock))

deriving newtype instance ToSchema C.BlockNo
deriving newtype instance ToSchema C.EpochNo
deriving newtype instance ToSchema C.Lovelace
deriving newtype instance ToSchema C.SlotNo
deriving newtype instance ToSchema C.TxIx

deriving via String instance ToSchema C.AddressAny
deriving via String instance ToSchema C.AssetName
deriving via String instance ToSchema C.PolicyId
deriving via String instance ToSchema C.TxId
deriving via String instance ToSchema (C.Hash C.BlockHeader)
deriving via String instance ToSchema (C.Hash C.ScriptData)
deriving via String instance ToSchema (C.Hash C.StakePoolKey)

deriving via String instance ToSchema C.Value -- FIXME
deriving via String instance ToSchema C.ScriptData -- FIXME
deriving via String instance ToSchema C.Quantity -- FIXME
deriving via String instance ToSchema Ledger.Nonce -- FIXME

instance ToSchema C.ScriptInAnyLang where
  declareNamedSchema _ = do
    stringSchema <- declareSchemaRef $ Proxy @String
    scriptSchema <- declareSchemaRef $ Proxy @(Maybe C.ScriptInAnyLang)
    return $
      NamedSchema (Just "ScriptInAnyLang") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("scriptLanguage", stringSchema)
               , ("script", scriptSchema)
               ]
          & required
            .~ [ "scriptLanguage"
               , "script"
               ]

deriving newtype instance ToSchema TxIndexInBlock

instance ToSchema Utxo where
  declareNamedSchema _ = do
    addressSchema <- declareSchemaRef $ Proxy @C.AddressAny
    txIdSchema <- declareSchemaRef $ Proxy @C.TxId
    txIxSchema <- declareSchemaRef $ Proxy @C.TxIx
    datumHashSchema <- declareSchemaRef $ Proxy @String -- @(Maybe (C.Hash C.ScriptData))
    valueSchema <- declareSchemaRef $ Proxy @String -- @C.Value
    inlineScriptSchema <- declareSchemaRef $ Proxy @(Maybe C.ScriptInAnyLang)
    inlineScriptHashSchema <- declareSchemaRef $ Proxy @String -- @(Maybe C.ScriptHash)
    txIndexInBlockSchema <- declareSchemaRef $ Proxy @TxIndexInBlock
    return $
      NamedSchema (Just "Utxo") $
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("address", addressSchema)
               , ("txId", txIdSchema)
               , ("txIx", txIxSchema)
               , ("datumHash", datumHashSchema)
               , ("value", valueSchema)
               , ("inlineScript", inlineScriptSchema) -- Uses ToJSON instance of cardano-api which serialises using the 'C.HasTextEnvelope' typeclass.
               , ("inlineScriptHash", inlineScriptHashSchema)
               , ("txIndexInBlock", txIndexInBlockSchema)
               ]
          & required
            .~ [ "address"
               , "txId"
               , "txIx"
               , "datumHash"
               , "value"
               , "inlineScript"
               , "inlineScriptHash"
               , "txIndexInBlock"
               ]
