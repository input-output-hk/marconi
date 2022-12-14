{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Spec.Escrow.Endpoints where

import Data.Text (unpack)

import Control.Lens (_1, has, only)
import Control.Monad (void)

import Ledger (PaymentPubKeyHash, pNetworkId)
import Ledger.Constraints qualified as Constraints
import Ledger.Interval (from)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts (TypedValidator)
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.Scripts qualified as Ledger
import Plutus.V1.Ledger.Api (Datum (Datum))

import Plutus.Contract
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..), check, foldMap)

import Prelude (Semigroup (..))

import Plutus.Contracts.Escrow

type EscrowTestSchema = Endpoint "badrefund-escrow" PaymentPubKeyHash .\/ EscrowSchema

-- | 'badRefund' with an endpoint.
badRefundEp ::
    forall w s.
    ( HasEndpoint "badrefund-escrow" PaymentPubKeyHash s
    )
    => EscrowParams Datum
    -> Promise w s EscrowError ()
badRefundEp escrow = endpoint @"badrefund-escrow" $ \pk -> badRefund (typedValidator escrow) pk

-- Submit a transaction attempting to take the refund belonging to the given pk.
badRefund ::
    forall w s.
    TypedValidator Escrow
    -> PaymentPubKeyHash
    -> Contract w s EscrowError ()
badRefund inst pk = do
    networkId <- pNetworkId <$> getParams
    unspentOutputs <- utxosAt (Scripts.validatorCardanoAddress networkId inst)
    current <- snd <$> currentNodeClientTimeRange
    let pkh = Ledger.datumHash $ Datum $ PlutusTx.toBuiltinData pk
        flt _ ciTxOut = has (Tx.decoratedTxOutScriptDatum . _1 . only pkh) ciTxOut
        tx' = Constraints.collectFromTheScriptFilter flt unspentOutputs Refund
           <> Constraints.mustValidateIn (from (current - 1))
    utx <- mkTxConstraints ( Constraints.typedValidatorLookups inst
                          <> Constraints.unspentOutputs unspentOutputs
                           ) tx'
    handleError (\err -> logError $ "Caught error: " ++ unpack err) $
      adjustUnbalancedTx utx >>= void . submitUnbalancedTx
