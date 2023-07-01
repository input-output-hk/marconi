{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.ChainIndex.Orphans where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Shelley.API qualified as Ledger
import Codec.CBOR.Read qualified as CBOR
import Codec.Serialise (Serialise (decode, encode))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Coerce (coerce)
import Data.Proxy (Proxy (Proxy))
import Data.SOP.Strict (K (K), NP (Nil, (:*)), fn, type (:.:) (Comp))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.FromRow (FromRow (fromRow))
import Database.SQLite.Simple.Ok qualified as SQL
import Database.SQLite.Simple.ToField (ToField (toField))
import Database.SQLite.Simple.ToField qualified as SQL
import Database.SQLite.Simple.ToRow (ToRow (toRow))
import Marconi.ChainIndex.Types (SecurityParam (SecurityParam))
import Ouroboros.Consensus.Byron.Ledger qualified as O
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.HardFork.Combinator qualified as O
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O
import Prettyprinter (Pretty (pretty), (<+>))

-- * ChainPoint

instance Pretty C.ChainTip where
  pretty C.ChainTipAtGenesis = "ChainTipAtGenesis"
  pretty (C.ChainTip sn ha bn) = "ChainTip(" <> pretty sn <> "," <+> pretty ha <> "," <+> pretty bn <> ")"

instance Pretty C.ChainPoint where
  pretty C.ChainPointAtGenesis = "ChainPointAtGenesis"
  pretty (C.ChainPoint sn ha) = "ChainPoint(" <> pretty sn <> "," <+> pretty ha <> ")"

instance SQL.FromRow C.ChainPoint where
  fromRow = C.ChainPoint <$> SQL.field <*> SQL.field

instance ToRow C.ChainPoint where
  toRow C.ChainPointAtGenesis = [SQL.SQLNull]
  toRow (C.ChainPoint sn bh) = [toField sn, toField bh]

-- * C.Hash C.BlockHeader

instance Pretty (C.Hash C.BlockHeader) where
  pretty hash = "BlockHash" <+> pretty (C.serialiseToRawBytesHexText hash)

instance SQL.ToField (C.Hash C.BlockHeader) where
  toField f = SQL.toField $ C.serialiseToRawBytes f

instance SQL.FromField (C.Hash C.BlockHeader) where
  fromField f = do
    bs <- SQL.fromField f
    case C.deserialiseFromRawBytes (C.proxyToAsType Proxy) bs of
      Left _ -> SQL.returnError SQL.ConversionFailed f "Cannot deserialise C.Hash C.BlockHeader"
      Right x -> pure x

-- * Sometime we need to get a count or test if a value exist.

instance ToRow Integer where
  toRow = SQL.toRow

instance FromRow Integer where
  fromRow = SQL.field

-- * C.SlotNo

instance Pretty C.SlotNo where
  pretty (C.SlotNo n) = "Slot" <+> pretty n

deriving newtype instance SQL.ToField C.SlotNo
deriving newtype instance SQL.FromField C.SlotNo

instance ToRow C.SlotNo where
  toRow (C.SlotNo bn) = [toField bn]

instance SQL.FromRow C.SlotNo where
  fromRow = C.SlotNo <$> SQL.field

-- * C.BlockNo

instance Pretty C.BlockNo where
  pretty (C.BlockNo bn) = "BlockNo" <+> pretty bn

instance SQL.FromRow C.BlockNo where
  fromRow = C.BlockNo <$> SQL.field

instance ToRow C.BlockNo where
  toRow (C.BlockNo bn) = [toField bn]

-- * C.AddressAny

instance SQL.FromField C.AddressAny where
  fromField f =
    SQL.fromField f >>= \b ->
      either
        (const cantDeserialise)
        pure
        $ C.deserialiseFromRawBytes
          C.AsAddressAny
          b
    where
      cantDeserialise = SQL.returnError SQL.ConversionFailed f "Cannot deserialise address."

instance SQL.ToField C.AddressAny where
  toField = SQL.SQLBlob . C.serialiseToRawBytes

instance FromJSON C.AddressAny where
  parseJSON (Aeson.String v) =
    maybe
      mempty
      pure
      $ C.deserialiseAddress C.AsAddressAny v
  parseJSON _ = mempty

instance ToJSON C.AddressAny where
  toJSON = Aeson.String . C.serialiseAddress

-- * C.Hash C.ScriptData

instance SQL.FromField (C.Hash C.ScriptData) where
  fromField f =
    SQL.fromField f
      >>= either
        (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise C.Hash C.ScriptData.")
        pure
        . C.deserialiseFromRawBytes (C.AsHash C.AsScriptData)

instance SQL.ToField (C.Hash C.ScriptData) where
  toField = SQL.SQLBlob . C.serialiseToRawBytes

-- * C.ScriptData

instance Serialise C.ScriptData where
  encode = CBOR.toCBOR
  decode = CBOR.fromCBOR

instance SQL.FromField C.ScriptData where
  fromField f =
    SQL.fromField f
      >>= either (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise C.ScriptData.") pure
        . C.deserialiseFromCBOR C.AsScriptData

instance SQL.ToField C.ScriptData where
  toField = SQL.SQLBlob . C.serialiseToCBOR

instance FromJSON C.ScriptData where
  parseJSON =
    either (fail . show) (pure . C.getScriptData)
      . C.scriptDataFromJson C.ScriptDataJsonDetailedSchema

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left v) = Left $ f v
mapLeft _ (Right v) = Right v

instance ToJSON C.ScriptData where
  toJSON = C.scriptDataToJson C.ScriptDataJsonDetailedSchema . C.unsafeHashableScriptData

-- * C.TxIn

instance SQL.ToRow C.TxIn where
  toRow (C.TxIn txid txix) = SQL.toRow (txid, txix)

instance SQL.FromRow C.TxIn where
  fromRow = C.TxIn <$> SQL.field <*> SQL.field

instance SQL.FromField C.TxId where
  fromField f =
    SQL.fromField f
      >>= either
        (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise TxId.")
        pure
        . C.deserialiseFromRawBytes (C.proxyToAsType Proxy)

instance SQL.ToField C.TxId where
  toField = SQL.SQLBlob . C.serialiseToRawBytes

instance SQL.FromField C.TxIx where
  fromField = fmap C.TxIx . SQL.fromField

instance SQL.ToField C.TxIx where
  toField (C.TxIx i) = SQL.SQLInteger $ fromIntegral i

-- * C.Value

instance SQL.ToField C.Value where
  toField = SQL.SQLBlob . toStrict . Aeson.encode

instance SQL.FromField C.Value where
  fromField f =
    SQL.fromField f
      >>= either
        (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise value.")
        pure
        . Aeson.eitherDecode

-- * C.ScriptInAnyLang

instance SQL.ToField C.ScriptInAnyLang where
  toField = SQL.SQLBlob . toStrict . Aeson.encode

instance SQL.FromField C.ScriptInAnyLang where
  fromField f =
    SQL.fromField f
      >>= either
        (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise value.")
        pure
        . Aeson.eitherDecode

-- * C.ScriptHash

instance SQL.ToField C.ScriptHash where
  toField = SQL.SQLBlob . C.serialiseToRawBytesHex

instance SQL.FromField C.ScriptHash where
  fromField f =
    SQL.fromField f
      >>= either
        (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise scriptDataHash.")
        pure
        . C.deserialiseFromRawBytesHex (C.proxyToAsType Proxy)

-- * O.LedgerState (O.CardanoBlock O.StandardCrypto)

instance SQL.ToField (O.LedgerState (O.CardanoBlock O.StandardCrypto)) where
  toField = SQL.SQLBlob . CBOR.toStrictByteString . encodeLedgerState

instance SQL.FromField (O.LedgerState (O.CardanoBlock O.StandardCrypto)) where
  fromField f =
    SQL.fromField f
      >>= either
        (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise ExtLedgerState.")
        (pure . snd)
        . CBOR.deserialiseFromBytes decodeLedgerState

-- * Ledger.Nonce

instance SQL.ToField Ledger.Nonce where
  toField = SQL.SQLBlob . CBOR.toStrictByteString . CBOR.toCBOR

instance SQL.FromField Ledger.Nonce where
  fromField f =
    SQL.fromField f
      >>= either
        (const $ SQL.returnError SQL.ConversionFailed f "Cannot deserialise Ledger.Nonce.")
        (pure . snd)
        . CBOR.deserialiseFromBytes CBOR.fromCBOR

-- * ToField/FromField

deriving newtype instance SQL.ToField C.BlockNo
deriving newtype instance SQL.FromField C.BlockNo

deriving newtype instance SQL.ToField C.AssetName
deriving newtype instance SQL.FromField C.AssetName

deriving newtype instance SQL.ToField C.Quantity
deriving newtype instance SQL.FromField C.Quantity

instance SQL.ToField C.EpochNo where
  toField (C.EpochNo word64) = SQL.toField word64
instance SQL.FromField C.EpochNo where
  fromField f = C.EpochNo <$> SQL.fromField f

instance SQL.ToField C.Lovelace where
  toField = SQL.toField @Integer . coerce
instance SQL.FromField C.Lovelace where
  fromField = coerce . SQL.fromField @Integer

instance SQL.FromField C.PoolId where
  fromField f = do
    bs <- SQL.fromField f
    case C.deserialiseFromRawBytes (C.AsHash C.AsStakePoolKey) bs of
      Right h -> pure h
      Left _ -> SQL.returnError SQL.ConversionFailed f " PoolId"

instance SQL.ToField C.PoolId where
  toField = SQL.toField . C.serialiseToRawBytes

instance SQL.ToField C.PolicyId where -- C.PolicyId is a newtype over C.ScriptHash but no ToField available for it.
  toField = SQL.toField . C.serialiseToRawBytes
instance SQL.FromField C.PolicyId where
  fromField = fromFieldViaRawBytes C.AsPolicyId

-- | Helper to deserialize via SerialiseAsRawBytes instance
fromFieldViaRawBytes :: (C.SerialiseAsRawBytes a) => C.AsType a -> SQL.Field -> SQL.Ok a
fromFieldViaRawBytes as f = either (const err) pure . C.deserialiseFromRawBytes as =<< SQL.fromField f
  where
    err = SQL.returnError SQL.ConversionFailed f "can't deserialise via SerialiseAsRawBytes"

encodeLedgerState :: O.LedgerState (O.CardanoBlock O.StandardCrypto) -> CBOR.Encoding
encodeLedgerState (O.HardForkLedgerState st) =
  O.encodeTelescope
    (byron :* shelley :* allegra :* mary :* alonzo :* babbage :* conway :* Nil)
    st
  where
    byron = fn (K . O.encodeByronLedgerState)
    shelley = fn (K . O.encodeShelleyLedgerState)
    allegra = fn (K . O.encodeShelleyLedgerState)
    mary = fn (K . O.encodeShelleyLedgerState)
    alonzo = fn (K . O.encodeShelleyLedgerState)
    babbage = fn (K . O.encodeShelleyLedgerState)
    conway = fn (K . O.encodeShelleyLedgerState)

decodeLedgerState :: forall s. CBOR.Decoder s (O.LedgerState (O.CardanoBlock O.StandardCrypto))
decodeLedgerState =
  O.HardForkLedgerState
    <$> O.decodeTelescope (byron :* shelley :* allegra :* mary :* alonzo :* babbage :* conway :* Nil)
  where
    byron = Comp O.decodeByronLedgerState
    shelley = Comp O.decodeShelleyLedgerState
    allegra = Comp O.decodeShelleyLedgerState
    mary = Comp O.decodeShelleyLedgerState
    alonzo = Comp O.decodeShelleyLedgerState
    babbage = Comp O.decodeShelleyLedgerState
    conway = Comp O.decodeShelleyLedgerState

-- * SecurityParam

deriving newtype instance SQL.ToField SecurityParam
deriving newtype instance SQL.FromField SecurityParam
