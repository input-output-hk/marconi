{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module PSGenerator.Common where

import           Control.Applicative                       (empty, (<|>))
import           Control.Monad.Reader                      (MonadReader)
import           Data.Proxy                                (Proxy (Proxy))
import           Language.PureScript.Bridge                (BridgePart, Language (Haskell), PSType, SumType,
                                                            TypeInfo (TypeInfo), doCheck, equal, functor, genericShow,
                                                            haskType, isTuple, mkSumType, order, psTypeParameters,
                                                            typeModule, typeName, (^==))
import           Language.PureScript.Bridge.Builder        (BridgeData)
import           Language.PureScript.Bridge.PSTypes        (psArray, psInt, psString)
import           Language.PureScript.Bridge.TypeParameters (A)
import           Ledger                                    (Address, Datum, DatumHash, MonetaryPolicy, PubKey,
                                                            PubKeyHash, Redeemer, Signature, Tx, TxId, TxIn, TxInType,
                                                            TxOut, TxOutRef, TxOutTx, TxOutType, UtxoIndex, Validator)
import           Ledger.Ada                                (Ada)
import           Ledger.Index                              (ValidationError)
import           Ledger.Interval                           (Extended, Interval, LowerBound, UpperBound)
import           Ledger.Scripts                            (ScriptError)
import           Ledger.Slot                               (Slot)
import           Ledger.Value                              (CurrencySymbol, TokenName, Value)
import           Wallet.API                                (WalletAPIError)
import qualified Wallet.Emulator.Wallet                    as EM
import           Wallet.Rollup.Types                       (AnnotatedTx, BeneficialOwner, DereferencedInput, SequenceId,
                                                            TxKey)

psJson :: PSType
psJson = TypeInfo "" "Data.RawJson" "RawJson" []

psNonEmpty :: MonadReader BridgeData m => m PSType
psNonEmpty =
    TypeInfo "" "Data.Json.JsonNonEmptyList" "JsonNonEmptyList" <$>
    psTypeParameters

psJsonEither :: MonadReader BridgeData m => m PSType
psJsonEither =
    TypeInfo "" "Data.Json.JsonEither" "JsonEither" <$> psTypeParameters

psJsonMap :: MonadReader BridgeData m => m PSType
psJsonMap = TypeInfo "" "Data.Json.JsonMap" "JsonMap" <$> psTypeParameters

psUnit :: PSType
psUnit = TypeInfo "" "Data.Unit" "Unit" []

-- Note: Haskell has multi-section Tuples, whereas PureScript just uses nested pairs.
psJsonTuple :: MonadReader BridgeData m => m PSType
psJsonTuple = expand <$> psTypeParameters
  where
    expand []       = psUnit
    expand [x]      = x
    expand p@[_, _] = TypeInfo "" "Data.Json.JsonTuple" "JsonTuple" p
    expand (x:ys)   = TypeInfo "" "Data.Json.JsonTuple" "JsonTuple" [x, expand ys]

psJsonUUID :: PSType
psJsonUUID = TypeInfo "" "Data.Json.JsonUUID" "JsonUUID" []

uuidBridge :: BridgePart
uuidBridge = do
    typeName ^== "UUID"
    typeModule ^== "Data.UUID" <|> typeModule ^== "Data.UUID.Types.Internal"
    pure psJsonUUID

mapBridge :: BridgePart
mapBridge = do
    typeName ^== "Map"
    typeModule ^== "Data.Map.Internal"
    psJsonMap

aesonValueBridge :: BridgePart
aesonValueBridge = do
    typeName ^== "Value"
    typeModule ^== "Data.Aeson.Types.Internal"
    pure psJson

eitherBridge :: BridgePart
eitherBridge = do
    typeName ^== "Either"
    psJsonEither

tupleBridge :: BridgePart
tupleBridge = do
    doCheck haskType isTuple
    psJsonTuple

aesonBridge :: BridgePart
aesonBridge =
    mapBridge <|> eitherBridge <|> tupleBridge <|> aesonValueBridge <|>
    uuidBridge

------------------------------------------------------------
setBridge :: BridgePart
setBridge = do
    typeName ^== "Set"
    typeModule ^== "Data.Set" <|> typeModule ^== "Data.Set.Internal"
    psArray

nonEmptyBridge :: BridgePart
nonEmptyBridge = do
    typeName ^== "NonEmpty"
    typeModule ^== "GHC.Base"
    psNonEmpty

containersBridge :: BridgePart
containersBridge = nonEmptyBridge <|> setBridge

------------------------------------------------------------
integerBridge :: BridgePart
integerBridge = do
    typeName ^== "Integer"
    pure psInt

digestBridge :: BridgePart
digestBridge = do
    typeName ^== "Digest"
    typeModule ^== "Crypto.Hash.Types"
    pure psString

byteStringBridge :: BridgePart
byteStringBridge = do
    typeName ^== "ByteString"
    typeModule ^== "Data.ByteString.Lazy.Internal"
    pure psString

scientificBridge :: BridgePart
scientificBridge = do
    typeName ^== "Scientific"
    typeModule ^== "Data.Scientific"
    pure psInt

miscBridge :: BridgePart
miscBridge =
    byteStringBridge <|> integerBridge <|> scientificBridge <|> digestBridge

------------------------------------------------------------
psAssocMap :: MonadReader BridgeData m => m PSType
psAssocMap =
    TypeInfo "plutus-playground-client" "Language.PlutusTx.AssocMap" "Map" <$>
    psTypeParameters

dataBridge :: BridgePart
dataBridge = do
    typeName ^== "Data"
    typeModule ^== "Language.PlutusTx.Data"
    pure psString

assocMapBridge :: BridgePart
assocMapBridge = do
    typeName ^== "Map"
    typeModule ^== "Language.PlutusTx.AssocMap"
    psAssocMap

languageBridge :: BridgePart
languageBridge = dataBridge <|> assocMapBridge

------------------------------------------------------------
scriptBridge :: BridgePart
scriptBridge = do
    typeName ^== "Script"
    typeModule ^== "Ledger.Scripts"
    pure psString

validatorHashBridge :: BridgePart
validatorHashBridge = do
    typeName ^== "ValidatorHash"
    typeModule ^== "Ledger.Scripts"
    pure psString

mpsHashBridge :: BridgePart
mpsHashBridge = do
    typeName ^== "MonetaryPolicyHash"
    typeModule ^== "Ledger.Scripts"
    pure psString

ledgerBytesBridge :: BridgePart
ledgerBytesBridge = do
    typeName ^== "LedgerBytes"
    typeModule ^== "LedgerBytes"
    pure psString

ledgerBridge :: BridgePart
ledgerBridge =
    scriptBridge <|> validatorHashBridge <|> mpsHashBridge <|> ledgerBytesBridge

------------------------------------------------------------
headersBridge :: BridgePart
headersBridge = do
    typeModule ^== "Servant.API.ResponseHeaders"
    typeName ^== "Headers"
    -- Headers should have two parameters, the list of headers and the return type.
    psTypeParameters >>= \case
        [_, returnType] -> pure returnType
        _ -> empty

headerBridge :: BridgePart
headerBridge = do
    typeModule ^== "Servant.API.Header"
    typeName ^== "Header'"
    empty

servantBridge :: BridgePart
servantBridge = headersBridge <|> headerBridge

------------------------------------------------------------
ledgerTypes :: [SumType 'Haskell]
ledgerTypes =
    [ (equal <*> (genericShow <*> mkSumType)) (Proxy @Slot)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @Ada)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @Tx)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @TxId)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @TxIn)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @TxOut)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @TxOutTx)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @TxOutRef)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @UtxoIndex)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @Value)
    , (functor <*> (equal <*> (genericShow <*> mkSumType)))
          (Proxy @(Extended A))
    , (functor <*> (equal <*> (genericShow <*> mkSumType)))
          (Proxy @(Interval A))
    , (functor <*> (equal <*> (genericShow <*> mkSumType)))
          (Proxy @(LowerBound A))
    , (functor <*> (equal <*> (genericShow <*> mkSumType)))
          (Proxy @(UpperBound A))
    , (genericShow <*> (order <*> mkSumType)) (Proxy @CurrencySymbol)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @MonetaryPolicy)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @Redeemer)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @Signature)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @TokenName)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @TxInType)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @Validator)
    , (genericShow <*> mkSumType) (Proxy @ScriptError)
    , (genericShow <*> mkSumType) (Proxy @ValidationError)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @Address)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @Datum)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @DatumHash)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @PubKey)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @PubKeyHash)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @TxOutType)
    ]

walletTypes :: [SumType 'Haskell]
walletTypes =
    [ (equal <*> (genericShow <*> mkSumType)) (Proxy @AnnotatedTx)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @DereferencedInput)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @EM.Wallet)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @WalletAPIError)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @BeneficialOwner)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @SequenceId)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @TxKey)
    ]
