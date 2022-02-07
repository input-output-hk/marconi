-- File auto generated by purescript-bridge! --
module Plutus.Contract.Error where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.RawJson (RawJson)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Ledger.Constraints.OffChain (MkTxError)
import Plutus.Contract.Checkpoint (CheckpointError)
import Plutus.Contract.Effects (ChainIndexResponse)
import Type.Proxy (Proxy(Proxy))
import Wallet.Emulator.Error (WalletAPIError)
import Wallet.Types (EndpointDescription, EndpointValue)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

newtype AssertionError = GenericAssertion { unAssertionError :: String }

derive instance Eq AssertionError

instance Show AssertionError where
  show a = genericShow a

instance EncodeJson AssertionError where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { unAssertionError: E.value :: _ String }
    )

instance DecodeJson AssertionError where
  decodeJson = defer \_ -> D.decode $ (GenericAssertion <$> D.record "GenericAssertion" { unAssertionError: D.value :: _ String })

derive instance Generic AssertionError _

derive instance Newtype AssertionError _

--------------------------------------------------------------------------------

_GenericAssertion :: Iso' AssertionError { unAssertionError :: String }
_GenericAssertion = _Newtype

--------------------------------------------------------------------------------

data ContractError
  = WalletContractError WalletAPIError
  | ChainIndexContractError String ChainIndexResponse
  | EmulatorAssertionContractError AssertionError
  | ConstraintResolutionContractError MkTxError
  | ResumableContractError MatchingError
  | CCheckpointContractError CheckpointError
  | EndpointDecodeContractError
      { eeEndpointDescription :: EndpointDescription
      , eeEndpointValue :: EndpointValue RawJson
      , eeErrorMessage :: String
      }
  | OtherContractError String

derive instance Eq ContractError

instance Show ContractError where
  show a = genericShow a

instance EncodeJson ContractError where
  encodeJson = defer \_ -> case _ of
    WalletContractError a -> E.encodeTagged "WalletContractError" a E.value
    ChainIndexContractError a b -> E.encodeTagged "ChainIndexContractError" (a /\ b) (E.tuple (E.value >/\< E.value))
    EmulatorAssertionContractError a -> E.encodeTagged "EmulatorAssertionContractError" a E.value
    ConstraintResolutionContractError a -> E.encodeTagged "ConstraintResolutionContractError" a E.value
    ResumableContractError a -> E.encodeTagged "ResumableContractError" a E.value
    CCheckpointContractError a -> E.encodeTagged "CCheckpointContractError" a E.value
    EndpointDecodeContractError { eeEndpointDescription, eeEndpointValue, eeErrorMessage } -> encodeJson
      { tag: "EndpointDecodeContractError"
      , eeEndpointDescription: flip E.encode eeEndpointDescription E.value
      , eeEndpointValue: flip E.encode eeEndpointValue E.value
      , eeErrorMessage: flip E.encode eeErrorMessage E.value
      }
    OtherContractError a -> E.encodeTagged "OtherContractError" a E.value

instance DecodeJson ContractError where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "ContractError"
    $ Map.fromFoldable
        [ "WalletContractError" /\ D.content (WalletContractError <$> D.value)
        , "ChainIndexContractError" /\ D.content (D.tuple $ ChainIndexContractError </$\> D.value </*\> D.value)
        , "EmulatorAssertionContractError" /\ D.content (EmulatorAssertionContractError <$> D.value)
        , "ConstraintResolutionContractError" /\ D.content (ConstraintResolutionContractError <$> D.value)
        , "ResumableContractError" /\ D.content (ResumableContractError <$> D.value)
        , "CCheckpointContractError" /\ D.content (CCheckpointContractError <$> D.value)
        , "EndpointDecodeContractError" /\
            ( EndpointDecodeContractError <$> D.object "EndpointDecodeContractError"
                { eeEndpointDescription: D.value :: _ EndpointDescription
                , eeEndpointValue: D.value :: _ (EndpointValue RawJson)
                , eeErrorMessage: D.value :: _ String
                }
            )
        , "OtherContractError" /\ D.content (OtherContractError <$> D.value)
        ]

derive instance Generic ContractError _

--------------------------------------------------------------------------------

_WalletContractError :: Prism' ContractError WalletAPIError
_WalletContractError = prism' WalletContractError case _ of
  (WalletContractError a) -> Just a
  _ -> Nothing

_ChainIndexContractError :: Prism' ContractError { a :: String, b :: ChainIndexResponse }
_ChainIndexContractError = prism' (\{ a, b } -> (ChainIndexContractError a b)) case _ of
  (ChainIndexContractError a b) -> Just { a, b }
  _ -> Nothing

_EmulatorAssertionContractError :: Prism' ContractError AssertionError
_EmulatorAssertionContractError = prism' EmulatorAssertionContractError case _ of
  (EmulatorAssertionContractError a) -> Just a
  _ -> Nothing

_ConstraintResolutionContractError :: Prism' ContractError MkTxError
_ConstraintResolutionContractError = prism' ConstraintResolutionContractError case _ of
  (ConstraintResolutionContractError a) -> Just a
  _ -> Nothing

_ResumableContractError :: Prism' ContractError MatchingError
_ResumableContractError = prism' ResumableContractError case _ of
  (ResumableContractError a) -> Just a
  _ -> Nothing

_CCheckpointContractError :: Prism' ContractError CheckpointError
_CCheckpointContractError = prism' CCheckpointContractError case _ of
  (CCheckpointContractError a) -> Just a
  _ -> Nothing

_EndpointDecodeContractError :: Prism' ContractError { eeEndpointDescription :: EndpointDescription, eeEndpointValue :: EndpointValue RawJson, eeErrorMessage :: String }
_EndpointDecodeContractError = prism' EndpointDecodeContractError case _ of
  (EndpointDecodeContractError a) -> Just a
  _ -> Nothing

_OtherContractError :: Prism' ContractError String
_OtherContractError = prism' OtherContractError case _ of
  (OtherContractError a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

newtype MatchingError = WrongVariantError { unWrongVariantError :: String }

derive instance Eq MatchingError

instance Show MatchingError where
  show a = genericShow a

instance EncodeJson MatchingError where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { unWrongVariantError: E.value :: _ String }
    )

instance DecodeJson MatchingError where
  decodeJson = defer \_ -> D.decode $ (WrongVariantError <$> D.record "WrongVariantError" { unWrongVariantError: D.value :: _ String })

derive instance Generic MatchingError _

derive instance Newtype MatchingError _

--------------------------------------------------------------------------------

_WrongVariantError :: Iso' MatchingError { unWrongVariantError :: String }
_WrongVariantError = _Newtype
