{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Test.Gen.Marconi.Cardano.Core.Types (
  nonEmptySubset,
  genBlockHeader,
  genHashBlockHeader,
  genBlockNo,
  genChainSyncEvents,
  genChainPoints,
  genChainPoint,
  genChainPoint',
  CGen.genExecutionUnits,
  genSlotNo,
  genTxBodyContentWithTxInsCollateral,
  genTxBodyContentForPlutusScripts,
  genTxBodyWithTxIns,
  genTxIndex,
  genWitnessAndHashInEra,
  genTxOutTxContext,
  genShelleyTxOutTxContext,
  CGen.genAddressInEra,
  CGen.genTxOutValue,
  genSimpleScriptData,
  genSimpleHashableScriptData,
  genProtocolParametersForPlutusScripts,
  CGen.genHashScriptData,
  CGen.genAssetId,
  CGen.genPolicyId,
  CGen.genQuantity,
  CGen.genEpochNo,
  genPoolId,
) where

import Cardano.Api qualified as C
import Cardano.Api.Extended.Streaming (ChainSyncEvent (RollBackward, RollForward))
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash.Class qualified as CRYPTO
import Cardano.Ledger.Keys (KeyHash (KeyHash))
import Control.Monad.State (StateT, evalStateT, lift, modify)
import Control.Monad.State.Lazy (MonadState (get))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.Coerce (coerce)
import Data.List.NonEmpty as NE (NonEmpty ((:|)), cons, fromList, init, toList)
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import Hedgehog (Gen, MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCostModelParams)
import Test.Gen.Cardano.Api.Typed qualified as CGen

nonEmptySubset :: (MonadGen m, Ord a) => Set a -> m (Set a)
nonEmptySubset s = do
  e <- Gen.element (Set.toList s)
  sub <- Gen.subset s
  pure $ Set.singleton e <> sub

genSlotNo :: (Hedgehog.MonadGen m) => m C.SlotNo
genSlotNo = C.SlotNo <$> Gen.word64 (Range.linear 10 1000)

genBlockNo :: (Hedgehog.MonadGen m) => m C.BlockNo
genBlockNo = C.BlockNo <$> Gen.word64 (Range.linear 100 1000)

validByteSizeLength :: Int
validByteSizeLength = 32

{- | Generate an (almost) sound chain of 'ChainSyncEvent'.
 "almost" because the 'ChainTip' of the events is always 'ChainTipAtGenesis'.
-}
genChainSyncEvents
  :: (Hedgehog.MonadGen m)
  => (a -> C.ChainPoint)
  -- ^ extract the chainpoint from the event
  -> (a -> m a)
  -- ^ generator for an event based on the last event
  -> a
  -- ^ the initial event
  -> Word64
  -- ^ minimal number of generated events
  -> Word64
  -- ^ maximal number of generated events
  -> m [ChainSyncEvent a]
genChainSyncEvents getChainPoint f start lo hi = do
  nbOfEvents <- Gen.word64 $ Range.linear lo hi
  reverse . toList
    <$> evalStateT (go nbOfEvents (RollForward start C.ChainTipAtGenesis :| [])) (start :| [])
  where
    go n xs
      | n <= 0 = pure xs
      | otherwise = do
          next <- genChainSyncEvent getChainPoint f
          go (n - 1) (cons next xs)

genChainSyncEvent
  :: (Hedgehog.MonadGen m)
  => (a -> C.ChainPoint)
  -> (a -> m a)
  -> StateT (NonEmpty a) m (ChainSyncEvent a)
genChainSyncEvent getChainPoint f = do
  s@(x :| xs) <- get
  let genNext =
        if null xs
          then genRollForward x
          else Gen.frequency [(5, genRollBackward s), (95, genRollForward x)]

  created <- lift genNext
  case created of
    RollForward y _ ->
      modify (cons y)
    RollBackward y _ ->
      modify (fromList . dropWhile ((y >) . getChainPoint) . toList)
  pure created
  where
    genRollBackward xs = RollBackward <$> (getChainPoint <$> Gen.element (NE.init xs)) <*> pure C.ChainTipAtGenesis
    genRollForward x = RollForward <$> f x <*> pure C.ChainTipAtGenesis

genBlockHeader
  :: (Hedgehog.MonadGen m)
  => m C.BlockNo
  -> m C.SlotNo
  -> m C.BlockHeader
genBlockHeader genB genS = do
  bs <- Gen.bytes (Range.singleton validByteSizeLength)
  sn <- genS
  bn <- genB
  let (hsh :: C.Hash C.BlockHeader) =
        fromJust $ either (const Nothing) Just $ C.deserialiseFromRawBytes (C.proxyToAsType Proxy) bs
  pure (C.BlockHeader sn hsh bn)

genHashBlockHeader :: (MonadGen m) => m (C.Hash C.BlockHeader)
genHashBlockHeader = C.HeaderHash . BSS.toShort <$> Gen.bytes (Range.singleton 32)

genChainPoints :: (MonadGen m) => Word64 -> Word64 -> m [C.ChainPoint]
genChainPoints b e = do
  maxSlots <- Gen.word64 (Range.linear b e)
  mapM (\s -> C.ChainPoint (C.SlotNo s) <$> genHashBlockHeader) [1 .. maxSlots]

genChainPoint'
  :: (Hedgehog.MonadGen m)
  => m C.BlockNo
  -> m C.SlotNo
  -> m C.ChainPoint
genChainPoint' genB genS = do
  (C.BlockHeader sn hsh _) <- genBlockHeader genB genS
  pure $ C.ChainPoint sn hsh

genChainPoint :: (Hedgehog.MonadGen m) => m C.ChainPoint
genChainPoint =
  Gen.frequency
    [ (95, genChainPoint' genBlockNo genSlotNo)
    , (5, pure C.ChainPointAtGenesis)
    ]

genTxIndex :: Gen C.TxIx
genTxIndex = C.TxIx . fromIntegral <$> Gen.word16 Range.constantBounded

genTxBodyWithTxIns
  :: (C.IsCardanoEra era)
  => C.CardanoEra era
  -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))]
  -> C.TxInsCollateral era
  -> Gen (C.TxBody era)
genTxBodyWithTxIns era txIns txInsCollateral = do
  txBodyContent <- genTxBodyContentWithTxInsCollateral era txIns txInsCollateral
  case C.createAndValidateTransactionBody txBodyContent of
    Left err -> fail $ C.displayError err
    Right txBody -> pure txBody

genTxBodyContentWithTxInsCollateral
  :: C.CardanoEra era
  -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))]
  -> C.TxInsCollateral era
  -> Gen (C.TxBodyContent C.BuildTx era)
genTxBodyContentWithTxInsCollateral era txIns txInsCollateral = do
  sbe <- case C.cardanoEraStyle era of
    C.LegacyByronEra -> fail "Byron era not supported"
    C.ShelleyBasedEra e -> pure e
  txbody <- CGen.genTxBodyContent era
  initialPP <- CGen.genProtocolParameters C.BabbageEra
  let modifiedPP =
        initialPP
          { C.protocolParamUTxOCostPerWord = Just 1
          , C.protocolParamUTxOCostPerByte = Just 1
          , C.protocolParamMinUTxOValue = Just 1
          , C.protocolParamDecentralization = Just 0.1
          , C.protocolParamPrices = Just $ C.ExecutionUnitPrices 1 1
          , C.protocolParamMaxTxExUnits = Just $ C.ExecutionUnits 1 1
          , C.protocolParamMaxBlockExUnits = Just $ C.ExecutionUnits 1 1
          , C.protocolParamMaxValueSize = Just 1
          , C.protocolParamCollateralPercent = Just 1
          , C.protocolParamMaxCollateralInputs = Just 1
          }
  ledgerPP <-
    either (fail . C.displayError) pure $
      C.convertToLedgerProtocolParameters sbe modifiedPP
  let txProtocolParams = C.BuildTxWith $ Just ledgerPP
  pure $
    txbody
      { C.txIns
      , C.txInsCollateral
      , C.txProtocolParams
      }

genTxBodyContentForPlutusScripts :: Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentForPlutusScripts = do
  txIns <-
    map (,C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))
      <$> Gen.list (Range.constant 1 10) CGen.genTxIn
  txInsCollateral <-
    C.TxInsCollateral C.CollateralInBabbageEra <$> Gen.list (Range.linear 1 10) CGen.genTxIn
  let txInsReference = C.TxInsReferenceNone
  txOuts <- Gen.list (Range.constant 1 10) (genTxOutTxContext C.BabbageEra)
  let txTotalCollateral = C.TxTotalCollateralNone
  let txReturnCollateral = C.TxReturnCollateralNone
  txFee <- CGen.genTxFee C.BabbageEra
  let txValidityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
  let txMetadata = C.TxMetadataNone
  let txAuxScripts = C.TxAuxScriptsNone
  let txExtraKeyWits = C.TxExtraKeyWitnessesNone
  basicPP <- genProtocolParametersForPlutusScripts
  ledgerPP <-
    either (fail . C.displayError) pure $
      C.convertToLedgerProtocolParameters C.ShelleyBasedEraBabbage basicPP
  let txProtocolParams = C.BuildTxWith $ Just ledgerPP
  let txWithdrawals = C.TxWithdrawalsNone
  let txCertificates = C.TxCertificatesNone
  let txUpdateProposal = C.TxUpdateProposalNone
  let txMintValue = C.TxMintNone
  let txScriptValidity = C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid
  let txProposalProcedures = Nothing
  let txVotingProcedures = Nothing

  pure $
    C.TxBodyContent
      { C.txIns
      , C.txInsCollateral
      , C.txInsReference
      , C.txOuts
      , C.txTotalCollateral
      , C.txReturnCollateral
      , C.txFee
      , C.txValidityRange
      , C.txMetadata
      , C.txAuxScripts
      , C.txExtraKeyWits
      , C.txProtocolParams
      , C.txWithdrawals
      , C.txCertificates
      , C.txUpdateProposal
      , C.txMintValue
      , C.txScriptValidity
      , C.txProposalProcedures
      , C.txVotingProcedures
      }

genWitnessAndHashInEra :: C.CardanoEra era -> Gen (C.Witness C.WitCtxTxIn era, C.ScriptHash)
genWitnessAndHashInEra era = do
  C.ScriptInEra scriptLanguageInEra script <- CGen.genScriptInEra era
  witness :: C.Witness C.WitCtxTxIn era1 <-
    C.ScriptWitness C.ScriptWitnessForSpending <$> case script of
      C.PlutusScript version plutusScript -> do
        scriptData <- CGen.genHashableScriptData
        executionUnits <- CGen.genExecutionUnits
        pure $
          C.PlutusScriptWitness
            scriptLanguageInEra
            version
            (C.PScript plutusScript)
            (C.ScriptDatumForTxIn scriptData)
            scriptData
            executionUnits
      C.SimpleScript simpleScript ->
        pure $ C.SimpleScriptWitness scriptLanguageInEra (C.SScript simpleScript)
  pure (witness, C.hashScript script)

{- | Generate a @C.'TxOut'@ in the given era. It can contain Shelley or Byron addresses.
For a version that only gives Shelley addresses, see 'genShelleyTxOutTxContext'.
-}
genTxOutTxContext :: C.CardanoEra era -> Gen (C.TxOut C.CtxTx era)
genTxOutTxContext era = genTxOutTxContextWithAddress era (CGen.genAddressInEra era)

{- | Generate a @C.'TxOut'@ in the given era. It will contain only Shelley addresses.
For a version that also might give Byron addresses, see 'genTxOutTxContext'.
-}
genShelleyTxOutTxContext
  :: (C.IsShelleyBasedEra era) => C.CardanoEra era -> Gen (C.TxOut C.CtxTx era)
genShelleyTxOutTxContext era = genTxOutTxContextWithAddress era (genShelleyAddressInEra era)

genTxOutTxContextWithAddress
  :: C.CardanoEra era -> Gen (C.AddressInEra era) -> Gen (C.TxOut C.CtxTx era)
genTxOutTxContextWithAddress era addrGen =
  C.TxOut
    <$> addrGen
    <*> CGen.genTxOutValue era
    <*> genSimpleTxOutDatumHashTxContext era
    <*> constantReferenceScript era

-- | Generate a Shelley address in the given era.
genShelleyAddressInEra :: (C.IsShelleyBasedEra era) => C.CardanoEra era -> Gen (C.AddressInEra era)
genShelleyAddressInEra _ = C.shelleyAddressInEra <$> CGen.genAddressShelley

-- Copied from cardano-api, but removed the recursive construction because it is time consuming,
-- about a factor of 20 when compared to this simple generator.
genSimpleScriptData :: Gen C.ScriptData
genSimpleScriptData =
  Gen.choice
    [ C.ScriptDataNumber <$> genInteger
    , C.ScriptDataBytes <$> genByteString
    , C.ScriptDataConstructor <$> genInteger <*> pure []
    , pure $ C.ScriptDataList []
    , pure $ C.ScriptDataMap []
    ]
  where
    genInteger :: Gen Integer
    genInteger =
      Gen.integral
        ( Range.linear
            0
            (fromIntegral (maxBound :: Word64) :: Integer)
        )

    genByteString :: Gen ByteString
    genByteString =
      BS.pack
        <$> Gen.list
          (Range.linear 0 64)
          (Gen.word8 Range.constantBounded)

genSimpleHashableScriptData :: Gen C.HashableScriptData
genSimpleHashableScriptData = do
  sd <- genSimpleScriptData
  case C.deserialiseFromCBOR C.AsHashableScriptData $ C.serialiseToCBOR sd of
    Left e -> error $ "genHashableScriptData: " <> show e
    Right r -> return r

constantReferenceScript :: C.CardanoEra era -> Gen (C.ReferenceScript era)
constantReferenceScript era =
  case C.refInsScriptsAndInlineDatsSupportedInEra era of
    Nothing -> return C.ReferenceScriptNone
    Just supp ->
      pure $
        C.ReferenceScript supp $
          C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) $
            C.PlutusScript C.PlutusScriptV1 $
              C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

genSimpleTxOutDatumHashTxContext :: C.CardanoEra era -> Gen (C.TxOutDatum C.CtxTx era)
genSimpleTxOutDatumHashTxContext era = case era of
  C.ByronEra -> pure C.TxOutDatumNone
  C.ShelleyEra -> pure C.TxOutDatumNone
  C.AllegraEra -> pure C.TxOutDatumNone
  C.MaryEra -> pure C.TxOutDatumNone
  C.AlonzoEra ->
    Gen.choice
      [ pure C.TxOutDatumNone
      , C.TxOutDatumHash C.ScriptDataInAlonzoEra <$> CGen.genHashScriptData
      , C.TxOutDatumInTx C.ScriptDataInAlonzoEra <$> CGen.genHashableScriptData
      ]
  C.BabbageEra ->
    Gen.choice
      [ pure C.TxOutDatumNone
      , C.TxOutDatumHash C.ScriptDataInBabbageEra <$> CGen.genHashScriptData
      , C.TxOutDatumInTx C.ScriptDataInBabbageEra <$> CGen.genHashableScriptData
      , C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> CGen.genHashableScriptData
      ]
  C.ConwayEra ->
    Gen.choice
      [ pure C.TxOutDatumNone
      , C.TxOutDatumHash C.ScriptDataInConwayEra <$> CGen.genHashScriptData
      , C.TxOutDatumInTx C.ScriptDataInConwayEra <$> CGen.genHashableScriptData
      , C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInConwayEra <$> CGen.genHashableScriptData
      ]

genProtocolParametersForPlutusScripts :: Gen C.ProtocolParameters
genProtocolParametersForPlutusScripts =
  C.ProtocolParameters
    <$> ((,) <$> CGen.genNat <*> CGen.genNat)
    <*> Gen.maybe CGen.genRational
    <*> CGen.genMaybePraosNonce
    <*> CGen.genNat
    <*> CGen.genNat
    <*> CGen.genNat
    <*> CGen.genLovelace
    <*> CGen.genLovelace
    <*> Gen.maybe CGen.genLovelace
    <*> CGen.genLovelace
    <*> CGen.genLovelace
    <*> CGen.genLovelace
    <*> CGen.genEpochNo
    <*> CGen.genNat
    <*> CGen.genRationalInt64
    <*> CGen.genRational
    <*> CGen.genRational
    <*> pure Nothing -- Obsolete from babbage onwards
    <*> pure
      ( Map.fromList
          [
            ( C.AnyPlutusScriptVersion C.PlutusScriptV1
            , C.CostModel $
                Map.elems $
                  fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams
            )
          ,
            ( C.AnyPlutusScriptVersion C.PlutusScriptV2
            , C.CostModel $
                Map.elems $
                  fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams
            )
          ]
      )
    <*> (Just <$> CGen.genExecutionUnitPrices)
    <*> (Just <$> CGen.genExecutionUnits)
    <*> (Just <$> CGen.genExecutionUnits)
    <*> (Just <$> CGen.genNat)
    <*> (Just <$> CGen.genNat)
    <*> (Just <$> CGen.genNat)
    <*> (Just <$> CGen.genLovelace)

genPoolId :: Gen (C.Hash C.StakePoolKey)
genPoolId = C.StakePoolKeyHash . KeyHash . mkDummyHash <$> Gen.int (Range.linear 0 10)
  where
    mkDummyHash :: forall h a. (CRYPTO.HashAlgorithm h) => Int -> CRYPTO.Hash h a
    mkDummyHash = coerce . CRYPTO.hashWithSerialiser @h CBOR.toCBOR
