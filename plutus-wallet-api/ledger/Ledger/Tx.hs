{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Tx(
    -- * Transactions
    Tx(..),
    inputs,
    outputs,
    txOutRefs,
    unspentOutputsTx,
    spentOutputs,
    updateUtxo,
    validValuesTx,
    signatures,
    addSignature,
    -- ** Hashing transactions
    preHash,
    hashTx,
    -- ** Stripped transactions
    TxStripped(..),
    strip,
    -- * Transaction outputs
    TxOutType(..),
    TxOut(..),
    TxOutRef(..),
    isPubKeyOut,
    isPayToScriptOut,
    outAddress,
    outValue,
    outType,
    txOutPubKey,
    txOutData,
    pubKeyTxOut,
    scriptTxOut,
    scriptTxOut',
    -- * Transaction inputs
    TxInType(..),
    TxIn(..),
    inRef,
    inType,
    inScripts,
    inPubKey,
    validRange,
    pubKeyTxIn,
    scriptTxIn,
    -- * Addresses
    Address(..),
    pubKeyAddress,
    scriptAddress,
    inAddress
    ) where

import qualified Codec.CBOR.Write          as Write
import           Codec.Serialise.Class     (Serialise, encode)
import           Control.Lens
import           Crypto.Hash               (Digest, SHA256, hash)
import           Data.Aeson                (FromJSON, FromJSONKey (..), ToJSON, ToJSONKey (..))
import           Data.Aeson.Extras         (encodeSerialise)
import qualified Data.ByteArray            as BA
import qualified Data.ByteString.Char8     as BS8
import qualified Data.ByteString.Lazy      as BSL
import           Data.Hashable             (Hashable, hashWithSalt)
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Maybe                (isJust)
import qualified Data.Set                  as Set
import           Data.Text.Prettyprint.Doc
import           GHC.Generics              (Generic)
import           Schema                    (ToSchema)

import           Language.PlutusTx.Lattice

import           Ledger.Ada
import           Ledger.Crypto
import           Ledger.Orphans            ()
import           Ledger.Scripts
import           Ledger.Slot
import           Ledger.TxId
import           Ledger.Value
import qualified Ledger.Value              as V
import qualified LedgerBytes               as LB

{- Note [Serialisation and hashing]

We use cryptonite for generating hashes, which requires us to serialise values
to a strict ByteString (to implement `Data.ByteArray.ByteArrayAccess`).

Binary serialisation could be achieved via

1. The `binary` package
2. The `cbor` package

(1) is used in the cardano-sl repository, and (2) is used in the
`language-plutus-core` project in this repository.

In this module we use (2) because of the precedent. This means however that we
may generate different hashes for the same transactions compared to cardano-sl.
This might become a problem if/when we want to support "imports" of some real
blockchain state into the emulator.

However, it should be easy to change the serialisation mechanism later on,
especially because we only need one direction (to binary).

-}

-- | A payment address using a hash as the id.
newtype Address = Address { getAddress :: Digest SHA256 }
    deriving stock (Eq, Ord, Show, Generic)

instance Pretty Address where
    pretty = pretty . encodeSerialise . getAddress

instance Hashable Address where
    hashWithSalt s (Address digest) = hashWithSalt s $ BA.unpack digest

deriving newtype instance Serialise Address
deriving anyclass instance ToJSON Address
deriving anyclass instance FromJSON Address
deriving anyclass instance ToJSONKey Address
deriving anyclass instance FromJSONKey Address

-- | A transaction, including witnesses for its inputs.
data Tx = Tx {
    txInputs     :: Set.Set TxIn,
    -- ^ The inputs to this transaction.
    txOutputs    :: [TxOut],
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    txForge      :: !Value,
    -- ^ The 'Value' forged by this transaction.
    txFee        :: !Ada,
    -- ^ The fee for this transaction.
    txValidRange :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    txSignatures :: Map PubKey Signature
    -- ^ Signatures of this transaction
    } deriving stock (Show, Eq, Generic)
      deriving anyclass (ToJSON, FromJSON, Serialise)

instance Pretty Tx where
    pretty t@Tx{txInputs, txOutputs, txForge, txFee, txValidRange, txSignatures} =
        let renderOutput TxOut{txOutType, txOutValue} =
                hang 2 $ vsep ["-" <+> pretty txOutValue <+> "locked by", pretty txOutType]
            renderInput TxIn{txInRef,txInType} =
                let rest =
                        case txInType of
                            ConsumeScriptAddress _ redeemer ->
                                [pretty redeemer]
                            ConsumePublicKeyAddress pk ->
                                [pretty pk]
                in hang 2 $ vsep $ "-" <+> pretty txInRef : rest
            lines' =
                [ hang 2 (vsep ("inputs:" : fmap renderInput (Set.toList txInputs)))
                , hang 2 (vsep ("outputs:" : fmap renderOutput txOutputs))
                , "forge:" <+> pretty txForge
                , "fee:" <+> pretty txFee
                , hang 2 (vsep ("signatures:": fmap (pretty . fst) (Map.toList txSignatures)))
                , "validity range:" <+> viaShow txValidRange
                ]
            txid = hashTx t
        in nest 2 $ vsep ["Tx" <+> pretty txid <> colon, braces (vsep lines')]

instance Semigroup Tx where
    tx1 <> tx2 = Tx {
        txInputs = txInputs tx1 <> txInputs tx2,
        txOutputs = txOutputs tx1 <> txOutputs tx2,
        txForge = txForge tx1 <> txForge tx2,
        txFee = txFee tx1 <> txFee tx2,
        txValidRange = txValidRange tx1 /\ txValidRange tx2,
        txSignatures = txSignatures tx1 <> txSignatures tx2
        }

instance Monoid Tx where
    mempty = Tx mempty mempty mempty mempty top mempty

-- | The inputs of a transaction.
inputs :: Lens' Tx (Set.Set TxIn)
inputs = lens g s where
    g = txInputs
    s tx i = tx { txInputs = i }

-- | The outputs of a transaction.
outputs :: Lens' Tx [TxOut]
outputs = lens g s where
    g = txOutputs
    s tx o = tx { txOutputs = o }

-- | The validity range of a transaction.
validRange :: Lens' Tx SlotRange
validRange = lens g s where
    g = txValidRange
    s tx o = tx { txValidRange = o }

signatures :: Lens' Tx (Map PubKey Signature)
signatures = lens g s where
    g = txSignatures
    s tx sig = tx { txSignatures = sig }

instance BA.ByteArrayAccess Tx where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | Check that all values in a transaction are non-negative.
validValuesTx :: Tx -> Bool
validValuesTx Tx{..}
  = all (nonNegative . txOutValue) txOutputs && nonNegative txForge  && txFee >= 0 where
    nonNegative i = V.geq i mempty

-- | A transaction without witnesses for its inputs.
data TxStripped = TxStripped {
    txStrippedInputs  :: Set.Set TxOutRef,
    -- ^ The inputs to this transaction, as transaction output references only.
    txStrippedOutputs :: [TxOut],
    -- ^ The outputs of this transation.
    txStrippedForge   :: !Value,
    -- ^ The 'Value' forged by this transaction.
    txStrippedFee     :: !Ada
    -- ^ The fee for this transaction.
    } deriving (Show, Eq)

instance BA.ByteArrayAccess TxStripped where
    length = BA.length . BS8.pack . show
    withByteArray = BA.withByteArray . BS8.pack . show

strip :: Tx -> TxStripped
strip Tx{..} = TxStripped i txOutputs txForge txFee where
    i = Set.map txInRef txInputs

-- | Hash a stripped transaction once.
preHash :: TxStripped -> Digest SHA256
preHash = hash

-- | Double hash of a transaction, excluding its witnesses.
hashTx :: Tx -> TxId
hashTx = TxId . hash . preHash . strip

-- | A reference to a transaction output. This is a
-- pair of a transaction reference, and an index indicating which of the outputs
-- of that transaction we are referring to.
data TxOutRef = TxOutRef {
    txOutRefId  :: TxId,
    txOutRefIdx :: Integer -- ^ Index into the referenced transaction's outputs
    } deriving (Show, Eq, Ord, Generic)

instance Pretty TxOutRef where
    pretty TxOutRef{txOutRefId, txOutRefIdx} = pretty txOutRefId <> "!" <> pretty txOutRefIdx

deriving instance Serialise TxOutRef
deriving instance ToJSON TxOutRef
deriving instance FromJSON TxOutRef
deriving instance ToSchema TxOutRef
deriving instance ToJSONKey TxOutRef
deriving instance FromJSONKey TxOutRef

-- | A list of a transaction's outputs paired with a 'TxOutRef's referring to them.
txOutRefs :: Tx -> [(TxOut, TxOutRef)]
txOutRefs t = mkOut <$> zip [0..] (txOutputs t) where
    mkOut (i, o) = (o, TxOutRef txId i)
    txId = hashTx t

-- | The type of a transaction input.
data TxInType =
      ConsumeScriptAddress !ValidatorScript !RedeemerScript -- ^ A transaction input that consumes a script address with the given validator and redeemer pair.
    | ConsumePublicKeyAddress !PubKey -- ^ A transaction input that consumes a public key address.
    deriving (Show, Eq, Ord, Generic, Serialise, ToJSON, FromJSON)

-- | A transaction input, consisting of a transaction output reference and an input type.
data TxIn = TxIn {
    txInRef  :: !TxOutRef,
    txInType :: !TxInType
    } deriving (Show, Eq, Ord, Generic)

deriving instance Serialise TxIn
deriving instance ToJSON TxIn
deriving instance FromJSON TxIn

-- | The 'TxOutRef' spent by a transaction input.
inRef :: Lens TxIn TxIn TxOutRef TxOutRef
inRef = lens txInRef s where
    s txi r = txi { txInRef = r }

-- | The type of a transaction input.
inType :: Lens' TxIn TxInType
inType = lens txInType s where
    s txi t = txi { txInType = t }

-- | Validator and redeemer scripts of a transaction input that spends a
--   "pay to script" output.
inScripts :: TxIn -> Maybe (ValidatorScript, RedeemerScript)
inScripts TxIn{ txInType = t } = case t of
    ConsumeScriptAddress v r  -> Just (v, r)
    ConsumePublicKeyAddress _ -> Nothing

-- | Signature of a transaction input that spends a "pay to public key" output.
inPubKey :: TxIn -> Maybe PubKey
inPubKey TxIn{ txInType = t } = case t of
    ConsumeScriptAddress _ _  -> Nothing
    ConsumePublicKeyAddress p -> Just p

-- | The address of the output spent by a 'TxIn'.
inAddress :: TxIn -> BSL.ByteString
inAddress TxIn{ txInType = t } = case t of
    ConsumeScriptAddress v _ ->
        BSL.fromStrict . BA.convert . getAddress . scriptAddress $ v
    ConsumePublicKeyAddress pk  ->
        LB.bytes (getPubKey pk)

-- | A transaction input that spends a "pay to public key" output, given the witness.
pubKeyTxIn :: PubKey -> TxOutRef -> TxIn
pubKeyTxIn pubK r = TxIn r (ConsumePublicKeyAddress pubK)

-- | A transaction input that spends a "pay to script" output, given witnesses.
scriptTxIn :: TxOutRef -> ValidatorScript -> RedeemerScript -> TxIn
scriptTxIn r v = TxIn r . ConsumeScriptAddress v

instance BA.ByteArrayAccess TxIn where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | The type of a transaction output.
data TxOutType =
    PayToScript !DataScript -- ^ A pay-to-script output with the given data script.
    | PayToPubKey !PubKey -- ^ A pay-to-pubkey output.
    deriving (Show, Eq, Ord, Generic, Serialise, ToJSON, FromJSON, ToJSONKey)

instance Pretty TxOutType where
    pretty = \case
        PayToScript (DataScript ds) -> "PayToScript:" <+> pretty ds
        PayToPubKey pk -> "PayToPubKey:" <+> pretty pk

-- | A transaction output, consisting of a target address, a value, and an output type.
data TxOut = TxOut {
    txOutAddress :: !Address,
    txOutValue   :: !Value,
    txOutType    :: !TxOutType
    }
    deriving (Show, Eq, Generic)

deriving instance Serialise TxOut
deriving instance ToJSON TxOut
deriving instance FromJSON TxOut

instance BA.ByteArrayAccess TxOut where
    length        = BA.length . Write.toStrictByteString . encode
    withByteArray = BA.withByteArray . Write.toStrictByteString . encode

-- | The data script attached to a 'TxOut', if there is one.
txOutData :: TxOut -> Maybe DataScript
txOutData TxOut{txOutType = t} = case  t of
    PayToScript s -> Just s
    PayToPubKey _ -> Nothing

-- | The public key attached to a 'TxOut', if there is one.
txOutPubKey :: TxOut -> Maybe PubKey
txOutPubKey TxOut{txOutType = t} = case  t of
    PayToPubKey k -> Just k
    _             -> Nothing

-- | The address of a transaction output.
outAddress :: Lens TxOut TxOut Address Address
outAddress = lens txOutAddress s where
    s tx a = tx { txOutAddress = a }

-- | The value of a transaction output.
-- | TODO: Compute address again
outValue :: Lens' TxOut Value
outValue = lens txOutValue s where
    s tx v = tx { txOutValue = v }

-- | The output type of a transaction output.
-- | TODO: Compute address again
outType :: Lens' TxOut TxOutType
outType = lens txOutType s where
    s tx d = tx { txOutType = d }

-- | Whether the output is a pay-to-pubkey output.
isPubKeyOut :: TxOut -> Bool
isPubKeyOut = isJust . txOutPubKey

-- | Whether the output is a pay-to-script output.
isPayToScriptOut :: TxOut -> Bool
isPayToScriptOut = isJust . txOutData

-- | The address that should be targeted by a transaction output locked by the given public key.
pubKeyAddress :: PubKey -> Address
pubKeyAddress pk = Address $ hash h where
    h :: Digest SHA256 = hash $ Write.toStrictByteString e
    e = encode pk

-- | The address that should be used by a transaction output locked by the given validator script.
scriptAddress :: ValidatorScript -> Address
scriptAddress vl = Address $ hash h where
    h :: Digest SHA256 = hash $ Write.toStrictByteString e
    e = encode vl

-- | Create a transaction output locked by a validator script hash
--   with the given data script attached.
scriptTxOut' :: Value -> Address -> DataScript -> TxOut
scriptTxOut' v a ds = TxOut a v tp where
    tp = PayToScript ds

-- | Create a transaction output locked by a validator script and with the given data script attached.
scriptTxOut :: Value -> ValidatorScript -> DataScript -> TxOut
scriptTxOut v vs = scriptTxOut' v (scriptAddress vs)

-- | Create a transaction output locked by a public key.
pubKeyTxOut :: Value -> PubKey -> TxOut
pubKeyTxOut v pk = TxOut a v tp where
    a = pubKeyAddress pk
    tp = PayToPubKey pk

-- | The unspent outputs of a transaction.
unspentOutputsTx :: Tx -> Map TxOutRef TxOut
unspentOutputsTx t = Map.fromList $ fmap f $ zip [0..] $ txOutputs t where
    f (idx, o) = (TxOutRef (hashTx t) idx, o)

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> Set.Set TxOutRef
spentOutputs = Set.map txInRef . txInputs

-- | Update a map of unspent transaction outputs and signatures based on the inputs
--   and outputs of a transaction.
updateUtxo :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxo t unspent = (unspent `Map.difference` lift' (spentOutputs t)) `Map.union` outs where
    lift' = Map.fromSet (const ())
    outs = unspentOutputsTx t

-- | Sign the transaction with a 'PrivateKey' and add the signature to the
--   transaction's list of signatures.
addSignature :: PrivateKey -> Tx -> Tx
addSignature privK tx = tx & signatures . at pubK ?~ sig where
    sig = signTx (hashTx tx) privK
    pubK = toPublicKey privK
