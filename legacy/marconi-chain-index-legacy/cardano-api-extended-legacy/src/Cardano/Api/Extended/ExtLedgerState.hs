{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Extended.ExtLedgerState where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.Trans.Except (runExcept)
import Data.ByteArray (ByteArrayAccess)
import Data.ByteArray qualified as ByteArray
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Short qualified as BSS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Ouroboros.Consensus.Block qualified as O
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.HardFork.Combinator qualified as HFC
import Ouroboros.Consensus.Ledger.Abstract qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Node qualified as O
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()

data ExtLedgerStateError
  = LedgerStateError C.LedgerStateError
  | ApplyBlockError (O.ExtValidationError (O.CardanoBlock O.StandardCrypto))
  deriving (Show)

mkExtLedgerConfig
  :: C.GenesisConfig
  -> O.ExtLedgerCfg (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
mkExtLedgerConfig genesisConfig =
  O.ExtLedgerCfg $
    O.pInfoConfig $
      fst $
        C.mkProtocolInfoCardano genesisConfig

mkInitExtLedgerState
  :: C.GenesisConfig
  -> O.ExtLedgerState (HFC.HardForkBlock (O.CardanoEras O.StandardCrypto))
mkInitExtLedgerState genesisConfig = O.pInfoInitLedger $ fst protocolInfo
  where
    protocolInfo = C.mkProtocolInfoCardano genesisConfig

applyBlockExtLedgerState
  :: O.ExtLedgerCfg (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
  -> C.ValidationMode
  -> C.BlockInMode C.CardanoMode
  -> O.ExtLedgerState (O.CardanoBlock O.StandardCrypto)
  -> Either ExtLedgerStateError (O.ExtLedgerState (O.CardanoBlock O.StandardCrypto))
applyBlockExtLedgerState hfLedgerConfig validationMode bim oldLedgerState = do
  let block = C.toConsensusBlock bim
  case validationMode of
    C.FullValidation ->
      either (Left . ApplyBlockError) (Right . O.lrResult) $
        runExcept $
          O.tickThenApplyLedgerResult
            hfLedgerConfig
            (C.toConsensusBlock bim)
            oldLedgerState
    C.QuickValidation ->
      if O.blockPrevHash block == O.ledgerTipHash (O.ledgerState oldLedgerState)
        then
          Right $
            O.lrResult $
              O.tickThenReapplyLedgerResult
                hfLedgerConfig
                block
                oldLedgerState
        else
          Left $
            LedgerStateError $
              C.ApplyBlockHashMismatch $
                mconcat
                  [ "Ledger state hash mismatch. Ledger head is slot "
                  , C.textShow $
                      O.unSlotNo $
                        O.fromWithOrigin
                          (O.SlotNo 0)
                          (O.ledgerTipSlot $ O.ledgerState oldLedgerState)
                  , " hash "
                  , renderByteArray $
                      unChainHash $
                        O.ledgerTipHash $
                          O.ledgerState oldLedgerState
                  , " but block previous hash is "
                  , renderByteArray (unChainHash $ O.blockPrevHash block)
                  , " and block current hash is "
                  , renderByteArray $
                      BSS.fromShort $
                        HFC.getOneEraHash $
                          O.blockHash block
                  , "."
                  ]

renderByteArray :: (ByteArrayAccess bin) => bin -> Text
renderByteArray = Text.decodeUtf8 . Base16.encode . ByteArray.convert

unChainHash :: O.ChainHash (O.CardanoBlock era) -> ByteString
unChainHash ch =
  case ch of
    O.GenesisHash -> "genesis"
    O.BlockHash bh -> BSS.fromShort (HFC.getOneEraHash bh)
