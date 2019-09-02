{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS -fplugin-opt Language.PlutusTx.Plugin:debug-context #-}
module Language.PlutusTx.Coordination.Contracts.Future(
    -- $future
      Future(..)
    , FutureOwners(..)
    , mkOwners
    , FutureError(..)
    , FutureSchema
    , FutureSetup(..)
    , Role(..)
    , futureContract
    , futureStateMachine
    , validatorScript
    , initialiseFuture
    , initialMargin
    , futureAddress
    , tokenFor
    , initialState
    ) where

import           Control.Lens                   ((&), (.~), prism', review, makeClassyPrisms)
import           Control.Monad                  (void)
import           Control.Monad.Error.Lens       (throwing, throwing_)
import           Data.Foldable                  (toList)
import           GHC.Generics                   (Generic)
import           Language.Plutus.Contract
import qualified Language.Plutus.Contract.Tx    as Tx
import qualified Language.Plutus.Contract.Typed.Tx as Typed
import           Language.Plutus.Contract.Util  (loopM)
import qualified Language.PlutusTx              as PlutusTx
import           Language.PlutusTx              (Data)
import           Language.PlutusTx.Prelude
import           Language.PlutusTx.StateMachine (StateMachine (..))
import qualified Language.PlutusTx.StateMachine as SM
import           Ledger                         (PubKey, Slot (..), ValidatorScript, Value, Address, DataScript, ValidatorHash)
import qualified Ledger
import qualified Ledger.AddressMap              as AM
import qualified Ledger.Interval                as Interval
import qualified Ledger.Typed.Scripts           as Scripts
import           Ledger.Validation              (OracleValue (..), PendingTx, PendingTx' (..),
                                                 PendingTxOut (pendingTxOutValue))
import qualified Ledger.Validation              as Validation
import           Ledger.Value                   as Value

import qualified Language.PlutusTx.Coordination.Contracts.Currency as Currency
import Language.PlutusTx.Coordination.Contracts.Escrow (EscrowParams(..), AsEscrowError(..), EscrowError, RefundSuccess)
import qualified Language.PlutusTx.Coordination.Contracts.Escrow as Escrow
import qualified Language.PlutusTx.Coordination.Contracts.TokenAccount as TokenAccount
import Language.PlutusTx.Coordination.Contracts.TokenAccount (AccountOwner(..))

import qualified Prelude as Haskell

-- $future
-- A futures contract in Plutus. This example illustrates a number of concepts.
--   1. Maintaining a margin (a kind of deposit) during the duration of the contract to protect against breach of contract (see note [Futures in Plutus])
--   2. Using oracle values to obtain current pricing information (see note [Oracles] in Language.PlutusTx.Coordination.Contracts)
--   3. Writing contracts as state machines
--   4. Using tokens to represent claims on future cash flows

data FutureError =
    TokenSetupFailed ContractError
    -- ^ Something went wrong during the setup of the two tokens
    | MarginAdjustmentFailed ContractError
    -- ^ Something went wrong during a margin payment
    | EscrowFailed EscrowError
    -- ^ The escrow that initialises the future contract failed
    | EscrowRefunded RefundSuccess
    -- ^ The other party didn't make their payment in time so the contract never
    --   started.
    | FutureUtxoNotFound
    -- ^ The unspent output of the future was not found in the UTXO set.
    | FutureUtxoNotUnique
    -- ^ There was more than one unspent output at the address
    | FutureUtxoNoData
    -- ^ The data script of the future's unspent output was not found
    | FutureUtxoFromDataFailed Data
    -- ^ The data script of the future's unspent output could not be 
    --   decoded to a value of 'FutureState'
    | FutureTerminated
    -- ^ The contract was finished even though we didn't expect it to be
    | MarginRequirementsNotViolated
    -- ^ A call to "settle-early" failed because the margin requirements
    --   were not violated
    deriving Show

makeClassyPrisms ''FutureError

type FutureSchema =
    BlockchainActions
        .\/ Endpoint "initialise-future" (FutureSetup, Role)
        .\/ Endpoint "join-future" (FutureOwners, FutureSetup, Role)
        .\/ Endpoint "increase-margin" (Value, Role)
        .\/ Endpoint "settle-early" (OracleValue Value)
        .\/ Endpoint "settle-future" (OracleValue Value)

instance AsEscrowError FutureError where
    _EscrowError = prism' EscrowFailed (\case { EscrowFailed e -> Just e; _ -> Nothing})

futureContract :: Future -> Contract FutureSchema FutureError ()
futureContract ft = do
    owners <- join ft <|> initialiseFuture ft
    void $ loopM (const $ selectEither (increaseMargin ft owners) (settleFuture ft owners <|> settleEarly ft owners)) ()
    

{- note [Futures in Plutus]

A futures contract ("future") is an agreement to change ownership of an
asset at a certain time (the delivery time) for an agreed price (the forward
price). The time of the transfer, and the price, are fixed at the beginning of
the contract.

A future can be settled either by actually exchanging the asset for the price
(physical settlement) or by exchanging the difference between the forward price
and the spot (current) price. 

In Plutus we could do physical settlement for assets that exist on the 
blockchain, that is, for tokens and currencies (everything that's a 'Value'). But
the contract implemented here is for cash settlement.

The agreement involves two parties, a buyer (long position) and a seller (short
position). At the delivery time the actual price of the asset (spot price) is
quite likely different from the forward price. If the spot price is higher than
the forward price, then the seller transfers the difference to the buyer. If
the spot price is lower than the forward price, then the buyer transfers money
to the seller. In either case there is a risk that the payer does not meet their
obligation (by simply not paying). To protect against this risk, the contract
includes a kind of deposit called "margin".

Each party deposits an initial margin. If the price moves against the seller,
then the seller has to top up their margin periodically (in our case, once each
block). Likewise, if it moves against the buyer then the buyer has to top up
their margin. If either party fails to make a margin payment then the contract
will be settled early.

The current value of the underlying asset is determined by an oracle. See note
[Oracles] in Language.PlutusTx.Coordination.Contracts. Also note that we
wouldn't need oracles if this was a contract with physical settlement,

The contract has three phases: Initialisation, runtime, and settlement. In the
first phase both parties deposit their initial margins into an escrow contract.
The second phase is when the contract is "live". In this phase the contract
is a state machine whose state is the 'MarginnAccounts' with the current margins.
The transition from the second to the third phase happens either after the
settlement date, or if the sport price has moved so far that one of the margin
accounts is underfunded. The runtime and settlement phases are modeled as a state
machine, with 'FutureState' and 'FutureAction' types.

-}


-- | Basic data of a futures contract. `Future` contains all values that do not
--   change during the lifetime of the contract.
--
data Future =
    Future
        { ftDeliveryDate  :: Slot
        , ftUnits         :: Integer
        , ftUnitPrice     :: Value
        , ftInitialMargin :: Value
        , ftPriceOracle   :: PubKey
        , ftMarginPenalty :: Value
        -- ^ How much a participant loses if they fail to make the required
        --   margin payments.
        } deriving Generic

-- | The tokens that represent ownership of the two sides of the future
data FutureOwners =
    FutureOwners
        { ftoLong  :: AccountOwner
        -- ^ The owner of the "long" account (represented by a token)
        , ftoLongAccount :: ValidatorHash
        -- ^ Address of the 'TokenAccount' validator script for 'ftoLong'
        , ftoShort :: AccountOwner
        -- ^ The owner of the "short" account (represented by a token)
        , ftoShortAccount :: ValidatorHash
        -- ^ Address of the 'TokenAccount' validator script for 'ftoShort'
        } deriving (Haskell.Show, Generic)

mkOwners
    :: AccountOwner
    -> AccountOwner
    -> FutureOwners
mkOwners long short =
    FutureOwners
        { ftoLong = long
        , ftoLongAccount = TokenAccount.validatorHash long
        , ftoShort = short
        , ftoShortAccount = TokenAccount.validatorHash short
        }

{-# INLINABLE tokenFor #-}
tokenFor :: Role -> FutureOwners -> Value
tokenFor = \case
    Long -> \case FutureOwners{ftoLong=AccountOwner(sym,tn)} -> Value.singleton sym tn 1
    Short -> \case FutureOwners{ftoShort=AccountOwner(sym,tn)} -> Value.singleton sym tn 1

data MarginAccounts =
    MarginAccounts
        { ftsShortMargin :: Value
        , ftsLongMargin  :: Value
        } deriving (Haskell.Eq, Generic)

instance Eq MarginAccounts where
    l == r = ftsShortMargin l == ftsShortMargin r && ftsLongMargin l == ftsLongMargin r

data Role = Long | Short
    deriving (Generic, Show)

instance Eq Role where
    Long == Long = True
    Short == Short = True
    _ == _ = False

{-# INLINABLE adjustMargin #-}
adjustMargin :: Role -> Value -> MarginAccounts -> MarginAccounts
adjustMargin role value accounts =
    case role of
        Long  -> accounts { ftsLongMargin = ftsLongMargin accounts + value }
        Short -> accounts { ftsShortMargin = ftsShortMargin accounts + value }

{-# INLINABLE totalMargin #-}
totalMargin :: MarginAccounts -> Value
totalMargin MarginAccounts{ftsShortMargin, ftsLongMargin} =
    ftsShortMargin + ftsLongMargin

data FutureState = 
    Running MarginAccounts
    | Finished
    deriving (Haskell.Eq)

instance Eq FutureState where
    Running ma == Running ma' = ma == ma'
    Finished   == Finished    = True
    _ == _ = False

data FutureAction =
    AdjustMargin Role Value
    | Settle (OracleValue Value)
    -- ^ Close the contract at the delivery date by making the agreed payment
    --   and returning the margin deposits to their owners
    | SettleEarly (OracleValue Value)
    -- ^ Close the contract early after a margin payment has been missed. 
    --   The value of both margin accounts will be paid to the role that
    --   *didn't* violate the margin requirement

{-# INLINABLE futureStateMachine #-}
futureStateMachine
    :: Future
    -> FutureOwners
    -> StateMachine FutureState FutureAction
futureStateMachine ft fos =
    StateMachine
        { smTransition = futureTransition
        , smCheck      = futureCheck ft fos
        , smFinal      = \case { Finished -> True; _ -> False }
        }

{-# INLINABLE mkValidator #-}
mkValidator :: Future -> FutureOwners -> Scripts.ValidatorType (SM.StateMachine FutureState FutureAction)
mkValidator future ftos = SM.mkValidator (futureStateMachine future ftos)

scriptInstance :: Future -> FutureOwners -> Scripts.ScriptInstance (SM.StateMachine FutureState FutureAction)
scriptInstance future ftos = 
    let val = $$(PlutusTx.compile [|| validatorParam ||])
            `PlutusTx.applyCode`
                PlutusTx.liftCode future
                `PlutusTx.applyCode`
                    PlutusTx.liftCode ftos
        validatorParam f g = SM.mkValidator (futureStateMachine f g)
        wrap = Scripts.wrapValidator @FutureState @FutureAction
    
    in Scripts.Validator @(SM.StateMachine FutureState FutureAction)
        val
        $$(PlutusTx.compile [|| wrap ||])
        
machineInstance
    :: Future
    -> FutureOwners
    -> SM.StateMachineInstance FutureState FutureAction
machineInstance future ftos = 
    let machine = futureStateMachine future ftos
        script  = scriptInstance future ftos
    in SM.StateMachineInstance machine script

validatorScript :: Future -> FutureOwners -> ValidatorScript
validatorScript ft fos = Ledger.mkValidatorScript $
    $$(PlutusTx.compile [|| validatorParam ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode ft
            `PlutusTx.applyCode`
                PlutusTx.liftCode fos
    where validatorParam f g = Scripts.wrapValidator (futureCheck f g)
    
{-# INLINABLE verifyOracle #-}
verifyOracle :: Future -> OracleValue a -> (Slot, a)
verifyOracle Future{ftPriceOracle} OracleValue{ovSignature, ovSlot, ovValue} =
    if ovSignature == ftPriceOracle then (ovSlot, ovValue) else error ()

{-# INLINABLE futureTransition #-}
futureTransition :: FutureState -> FutureAction -> Maybe FutureState
futureTransition (Running accounts) (AdjustMargin role value) = Just (Running (adjustMargin role value accounts))
futureTransition (Running accounts) (Settle ov)               = Just Finished
futureTransition (Running accounts) (SettleEarly ov)          = Just Finished
futureTransition _ _                                          = Nothing

{-# INLINABLE outputsWith #-}
-- | The outputs of the 'PendingTx' that carry a non-zero amount of the currency
--   defined by the 'CurrencySymbol' and the 'TokenName'.
outputsWith :: PendingTx -> CurrencySymbol -> TokenName -> [PendingTxOut]
outputsWith PendingTx{pendingTxOutputs} symbol token =
    let expValue = Value.singleton symbol token 1 in
    filter (\output -> expValue `leq` pendingTxOutValue output) pendingTxOutputs

{-# INLINABLE paidTo #-}
-- | The total 'Value' paid by the pending transaction to outputs
--   whose value also includes a non-zero amount of the currency
--   & token
paidTo :: PendingTx -> (CurrencySymbol, TokenName) -> Value
paidTo ptx (symbol, token) =
    foldMap pendingTxOutValue (outputsWith ptx symbol token)

data Payouts =
    Payouts
        { payoutsShort :: Value
        , payoutsLong  :: Value
        }

payoutsTx :: Payouts -> FutureOwners -> UnbalancedTx
payoutsTx 
    Payouts{payoutsShort, payoutsLong}
    FutureOwners{ftoShort, ftoLong} =
        TokenAccount.payTx ftoShort payoutsShort
        Haskell.<> TokenAccount.payTx ftoLong payoutsLong

{-# INLINABLE payouts #-}
-- | Compute the payouts for each role given the future data,
--   margin accounts, and current (spot) price
payouts :: Future -> MarginAccounts -> Value -> Payouts
payouts Future{ftUnits, ftUnitPrice} MarginAccounts{ftsShortMargin, ftsLongMargin} spotPrice =
    let delta = scale ftUnits (spotPrice - ftUnitPrice)
    in Payouts
        { payoutsShort = ftsShortMargin - delta
        , payoutsLong  = ftsLongMargin + delta
        }

{-# INLINABLE futureCheck #-}
futureCheck :: Future -> FutureOwners -> FutureState -> FutureAction -> PendingTx -> Bool
futureCheck future owners state action ptx =
    let
        (ownHash, _, _) = Validation.ownHashes ptx
        vl = Validation.valueLockedBy ptx ownHash
    in
    case (state, action) of
        (Running accounts, AdjustMargin _ topUp) ->
                vl == (topUp + totalMargin accounts)
        (Running accounts, SettleEarly ov) ->
            let
                Future{ftDeliveryDate} = future
                MarginAccounts{ftsShortMargin, ftsLongMargin} = accounts
                FutureOwners{ftoLongAccount, ftoShortAccount} = owners
                (oracleDate, spotPrice) = verifyOracle future ov
                reqMargin   = requiredMargin future spotPrice
                violatingRole
                    | ftsShortMargin `lt` reqMargin = Short
                    | ftsLongMargin  `lt` reqMargin = Long
                    | True = traceH "Margin requirements not violated" (error ())
                payoutValid = case violatingRole of
                        Short -> Validation.valueLockedBy ptx ftoLongAccount `geq` totalMargin accounts
                        Long  -> Validation.valueLockedBy ptx ftoShortAccount `geq` totalMargin accounts
                slotValid = oracleDate < ftDeliveryDate
            in payoutValid && slotValid
        (Running accounts, Settle ov) ->
            let
                Future{ftDeliveryDate} = future
                FutureOwners{ftoLongAccount, ftoShortAccount} = owners
                (oracleDate, spotPrice) = verifyOracle future ov
                Payouts{payoutsShort, payoutsLong} = payouts future accounts spotPrice
                payoutValid = 
                    Validation.valueLockedBy ptx ftoLongAccount `geq` payoutsLong
                    && Validation.valueLockedBy ptx ftoShortAccount `geq` payoutsShort
                slotvalid   = oracleDate == ftDeliveryDate
                                && ftDeliveryDate `Interval.before` pendingTxValidRange ptx
            in slotvalid && payoutValid
        _ -> False

-- | Compute the required margin from the current price of the
--   underlying asset.
{-# INLINABLE requiredMargin #-}
requiredMargin :: Future -> Value -> Value
requiredMargin Future{ftUnits, ftUnitPrice, ftMarginPenalty} spotPrice =
    let
        delta  = scale ftUnits (spotPrice - ftUnitPrice)
    in
        ftMarginPenalty + delta

{-# INLINABLE initialMargin #-}
initialMargin :: Future -> Value
initialMargin ft@Future{ftUnitPrice, ftMarginPenalty} =
    ftMarginPenalty + ftUnitPrice

{-# INLINABLE initialState #-}
-- | The initial state of the 'Future' contract
initialState :: Future -> FutureState
initialState ft =
    let im = initialMargin ft in
    Running (MarginAccounts{ftsShortMargin=im, ftsLongMargin=im})

futureAddress :: Future -> FutureOwners -> Address
futureAddress ft fo = Ledger.scriptAddress (validatorScript ft fo)

-- | The data needed to initialise the futures contract.
data FutureSetup =
    FutureSetup
        { shortPK :: PubKey
        -- ^ Initial owner of the short token
        , longPK :: PubKey
        -- ^ Initial owner of the long token
        , contractStart :: Slot
        -- ^ Start of the futures contract itself. By this time the setup code 
        --   has to be finished, otherwise the contract is void.
        } deriving (Haskell.Show)

-- | Initialise the contract by
--   * Generating the tokens for long and short
--   * Setting up an escrow contract for the initial margins
--   * Paying the initial margin for the given role
initialiseFuture
    :: ( HasEndpoint "initialise-future" (FutureSetup, Role) s
       , HasBlockchainActions s
       , AsFutureError e
       )
    => Future
    -> Contract s e FutureOwners
initialiseFuture future = do
    (s, ownRole) <- endpoint @"initialise-future" @(FutureSetup, Role)
    ftos <- setupTokens
    let escr = escrowParams future ftos s
        escrowPayment = Escrow.payRedeemRefund escr (initialMargin future <> tokenFor Long ftos <> tokenFor Short ftos)
    e <- withContractError (review _EscrowFailed) escrowPayment
    either (throwing _EscrowRefunded) (\_ -> pure ftos) e

settleFuture
    :: ( HasEndpoint "settle-future" (OracleValue Value) s
       , HasBlockchainActions s
       , AsFutureError e
       )
    => Future
    -> FutureOwners
    -> Contract s e ()
settleFuture future@Future{ftDeliveryDate} ftos = do
    ov <- endpoint @"settle-future"
    let address = futureAddress future ftos
    unspentOutputs <- utxoAt address
    accounts <- currentBalances future ftos
    let tx = payoutsTx (payouts future accounts (ovValue ov)) ftos
             Haskell.<> Tx.collectFromScript unspentOutputs (validatorScript future ftos) (Ledger.RedeemerScript $ PlutusTx.toData $ Settle ov)
                & validityRange .~ Interval.from (succ ftDeliveryDate)
    void $ writeTx tx

settleEarly
    :: ( HasEndpoint "settle-early" (OracleValue Value) s
       , HasBlockchainActions s
       , AsFutureError e
       )
    => Future
    -> FutureOwners
    -> Contract s e ()
settleEarly future@Future{ftDeliveryDate} ftos = do
    ov <- endpoint @"settle-early"
    let address = futureAddress future ftos
        spotPrice = ovValue ov
        minMargin = requiredMargin future spotPrice
    unspentOutputs <- utxoAt address
    MarginAccounts{ftsShortMargin, ftsLongMargin} <- currentBalances future ftos
    
    violatingRole <- 
        if ftsShortMargin `lt` minMargin then pure Short
        else if ftsLongMargin `lt` minMargin then pure Long
        else throwing_ _MarginRequirementsNotViolated
    
    let payment = case violatingRole of
                    Long -> Payouts{payoutsLong=mempty, payoutsShort = ftsShortMargin + ftsLongMargin }
                    Short -> Payouts{payoutsLong=ftsShortMargin+ftsLongMargin, payoutsShort = mempty }
        tx = payoutsTx payment ftos
             Haskell.<> Typed.collectFromScript unspentOutputs (scriptInstance future ftos) (SettleEarly ov)
    void $ writeTx tx

-- | Determine the current 'MarginAccounts' by looking at the unspent output
--   of the contract.
currentBalances
    :: ( HasUtxoAt s
       , AsFutureError e)
    => Future
    -> FutureOwners
    -> Contract s e MarginAccounts
currentBalances future ftos = do
    let address = futureAddress future ftos
    unspentOutputs <- utxoAt address
    theOutput <- case toList (AM.outRefMap unspentOutputs) of
                    []  -> throwing_ _FutureUtxoNotFound
                    [x] -> pure x
                    _   -> throwing_ _FutureUtxoNotUnique
    Ledger.DataScript theData <- maybe (throwing_ _FutureUtxoNoData) pure (Ledger.txOutTxData theOutput)
    theState <- maybe (throwing _FutureUtxoFromDataFailed theData) pure (PlutusTx.fromData theData)
    case theState of
        Running accounts -> pure accounts
        Finished         -> throwing_ _FutureTerminated

increaseMargin
    :: ( HasEndpoint "increase-margin" (Value, Role) s
       , HasUtxoAt s
       , HasWriteTx s
       , AsFutureError e
       )
    => Future
    -> FutureOwners
    -> Contract s e ()
increaseMargin future ftos = do
    (value, role) <- endpoint @"increase-margin"
    let address = futureAddress future ftos
    unspentOutputs <- utxoAt address
    currentState <- currentBalances future ftos
    let newMarginAccounts = adjustMargin role value currentState
        dataScript = Ledger.DataScript (PlutusTx.toData $ Running newMarginAccounts)
        tx =
            Tx.collectFromScript unspentOutputs (validatorScript future ftos) (Ledger.RedeemerScript $ PlutusTx.toData $ AdjustMargin role value)
            Haskell.<> payToScript (totalMargin newMarginAccounts) address dataScript
    void (withContractError (review _MarginAdjustmentFailed) (writeTxSuccess tx))

join
    :: ( HasEndpoint "join-future" (FutureOwners, FutureSetup, Role) s
       , HasBlockchainActions s
       , AsFutureError e
       )
    => Future
    -> Contract s e FutureOwners
join ft = do
    (owners, stp, ownRole) <- endpoint @"join-future" @(FutureOwners, FutureSetup, Role)
    let escr = escrowParams ft owners stp
        payment = Escrow.pay escr (initialMargin ft)
    void (withContractError (review _EscrowFailed) payment)
    pure owners

setupTokens
    :: ( HasWriteTx s
       , HasWatchAddress s
       , HasOwnPubKey s
       , AsFutureError e
       )
    => Contract s e FutureOwners
setupTokens = do
    pk <- ownPubKey
    cur <- withContractError (review _TokenSetupFailed) (Currency.forgeContract pk [("long", 1), ("short", 1)])
    let tokenSym = Currency.currencySymbol cur
    pure $ mkOwners (AccountOwner (tokenSym, "long")) (AccountOwner (tokenSym, "short"))

-- | The escrow contract that initialises the future. Both parties have to pay
--   their initial margin to this contract in order to unlock their tokens.
escrowParams :: Future -> FutureOwners -> FutureSetup -> EscrowParams DataScript
escrowParams future ftos FutureSetup{longPK, shortPK, contractStart} = 
    let
        address = Ledger.validatorHash (validatorScript future ftos)
        dataScript  = Ledger.DataScript $ PlutusTx.toData $ initialState future
        targets = 
            [ Escrow.payToScriptTarget address 
                dataScript
                (scale 2 (initialMargin future))
            , Escrow.payToPubKeyTarget longPK (tokenFor Long ftos) 
            , Escrow.payToPubKeyTarget shortPK (tokenFor Short ftos) 
            ]
    in EscrowParams
        { escrowDeadline = contractStart
        , escrowTargets = targets
        }

PlutusTx.makeLift ''Future
PlutusTx.makeLift ''FutureOwners
PlutusTx.makeLift ''MarginAccounts
PlutusTx.makeIsData ''MarginAccounts
PlutusTx.makeLift ''Role
PlutusTx.makeIsData ''Role
PlutusTx.makeLift ''FutureState
PlutusTx.makeIsData ''FutureState
PlutusTx.makeLift ''FutureAction
PlutusTx.makeIsData ''FutureAction
