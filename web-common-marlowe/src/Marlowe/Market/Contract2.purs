module Marlowe.Market.Contract2
  ( contractTemplate
  , metaData
  , extendedContract
  ) where

import Prelude
import Data.BigInteger (BigInteger)
import Data.Tuple.Nested (type (/\), (/\))
import Examples.Metadata as Metadata
import Marlowe.Extended (Action(..), Case(..), Contract(..), Payee(..), Timeout(..), Value(..))
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.Extended.Template (ContractTemplate)
import Marlowe.Semantics (Bound(..), ChoiceId(..), Party(..), Token(..), ChoiceName)

contractTemplate :: ContractTemplate
contractTemplate = { metaData, extendedContract }

metaData :: MetaData
metaData = Metadata.escrowWithCollateral

ada :: Token
ada = Token "" ""

buyer :: Party
buyer = Role "Buyer"

seller :: Party
seller = Role "Seller"

burnAddress :: Party
burnAddress = PK "0000000000000000000000000000000000000000000000000000000000000000"

price :: Value
price = ConstantParam "Price"

collateral :: Value
collateral = ConstantParam "Collateral amount"

sellerCollateralTimeout :: Timeout
sellerCollateralTimeout = SlotParam "Collateral deposit by seller timeout"

buyerCollateralTimeout :: Timeout
buyerCollateralTimeout = SlotParam "Deposit of collateral by buyer timeout"

depositTimeout :: Timeout
depositTimeout = SlotParam "Deposit of price by buyer timeout"

disputeTimeout :: Timeout
disputeTimeout = SlotParam "Dispute by buyer timeout"

answerTimeout :: Timeout
answerTimeout = SlotParam "Seller's response timeout"

depositCollateral :: Party -> Timeout -> Contract -> Contract -> Contract
depositCollateral party timeout timeoutContinuation continuation =
  When [ Case (Deposit party party ada collateral) continuation ]
    timeout
    timeoutContinuation

burnCollaterals :: Contract -> Contract
burnCollaterals continuation =
  Pay seller (Party burnAddress) ada collateral
    $ Pay buyer (Party burnAddress) ada collateral
    $ continuation

deposit :: Timeout -> Contract -> Contract -> Contract
deposit timeout timeoutContinuation continuation =
  When [ Case (Deposit seller buyer ada price) continuation ]
    timeout
    timeoutContinuation

choice :: ChoiceName -> Party -> BigInteger -> Contract -> Case
choice choiceName chooser choiceValue continuation =
  Case
    ( Choice (ChoiceId choiceName chooser)
        [ Bound choiceValue choiceValue ]
    )
    continuation

choices :: Timeout -> Party -> Contract -> Array (BigInteger /\ ChoiceName /\ Contract) -> Contract
choices timeout chooser timeoutContinuation list =
  When
    ( do
        (choiceValue /\ choiceName /\ continuation) <- list
        pure $ choice choiceName chooser choiceValue continuation
    )
    timeout
    timeoutContinuation

sellerToBuyer :: Contract -> Contract
sellerToBuyer = Pay seller (Account buyer) ada price

extendedContract :: Contract
extendedContract =
  depositCollateral seller sellerCollateralTimeout Close
    $ depositCollateral buyer buyerCollateralTimeout Close
    $ deposit depositTimeout Close
    $ choices disputeTimeout buyer Close
        [ (zero /\ "Everything is alright" /\ Close)
        , ( one /\ "Report problem"
              /\ ( sellerToBuyer
                    $ choices answerTimeout seller Close
                        [ (one /\ "Confirm problem" /\ Close)
                        , (zero /\ "Dispute problem" /\ burnCollaterals Close)
                        ]
                )
          )
        ]
