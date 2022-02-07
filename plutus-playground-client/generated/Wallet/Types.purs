-- File auto generated by purescript-bridge! --
module Wallet.Types where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.RawJson (RawJson)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Data.UUID.Argonaut (UUID)
import Type.Proxy (Proxy(Proxy))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

data ContractActivityStatus
  = Active
  | Stopped
  | Done

derive instance Eq ContractActivityStatus

derive instance Ord ContractActivityStatus

instance Show ContractActivityStatus where
  show a = genericShow a

instance EncodeJson ContractActivityStatus where
  encodeJson = defer \_ -> E.encode E.enum

instance DecodeJson ContractActivityStatus where
  decodeJson = defer \_ -> D.decode D.enum

derive instance Generic ContractActivityStatus _

instance Enum ContractActivityStatus where
  succ = genericSucc
  pred = genericPred

instance Bounded ContractActivityStatus where
  bottom = genericBottom
  top = genericTop

--------------------------------------------------------------------------------

_Active :: Prism' ContractActivityStatus Unit
_Active = prism' (const Active) case _ of
  Active -> Just unit
  _ -> Nothing

_Stopped :: Prism' ContractActivityStatus Unit
_Stopped = prism' (const Stopped) case _ of
  Stopped -> Just unit
  _ -> Nothing

_Done :: Prism' ContractActivityStatus Unit
_Done = prism' (const Done) case _ of
  Done -> Just unit
  _ -> Nothing

--------------------------------------------------------------------------------

newtype ContractInstanceId = ContractInstanceId { unContractInstanceId :: UUID }

derive instance Eq ContractInstanceId

derive instance Ord ContractInstanceId

instance Show ContractInstanceId where
  show a = genericShow a

instance EncodeJson ContractInstanceId where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { unContractInstanceId: E.value :: _ UUID }
    )

instance DecodeJson ContractInstanceId where
  decodeJson = defer \_ -> D.decode $ (ContractInstanceId <$> D.record "ContractInstanceId" { unContractInstanceId: D.value :: _ UUID })

derive instance Generic ContractInstanceId _

derive instance Newtype ContractInstanceId _

--------------------------------------------------------------------------------

_ContractInstanceId :: Iso' ContractInstanceId { unContractInstanceId :: UUID }
_ContractInstanceId = _Newtype

--------------------------------------------------------------------------------

newtype EndpointDescription = EndpointDescription { getEndpointDescription :: String }

instance Show EndpointDescription where
  show a = genericShow a

derive instance Eq EndpointDescription

derive instance Ord EndpointDescription

instance EncodeJson EndpointDescription where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { getEndpointDescription: E.value :: _ String }
    )

instance DecodeJson EndpointDescription where
  decodeJson = defer \_ -> D.decode $ (EndpointDescription <$> D.record "EndpointDescription" { getEndpointDescription: D.value :: _ String })

derive instance Generic EndpointDescription _

derive instance Newtype EndpointDescription _

--------------------------------------------------------------------------------

_EndpointDescription :: Iso' EndpointDescription { getEndpointDescription :: String }
_EndpointDescription = _Newtype

--------------------------------------------------------------------------------

newtype EndpointValue a = EndpointValue { unEndpointValue :: a }

derive instance (Eq a) => Eq (EndpointValue a)

instance (Show a) => Show (EndpointValue a) where
  show a = genericShow a

instance (EncodeJson a) => EncodeJson (EndpointValue a) where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { unEndpointValue: E.value :: _ a }
    )

instance (DecodeJson a) => DecodeJson (EndpointValue a) where
  decodeJson = defer \_ -> D.decode $ (EndpointValue <$> D.record "EndpointValue" { unEndpointValue: D.value :: _ a })

derive instance Generic (EndpointValue a) _

derive instance Newtype (EndpointValue a) _

--------------------------------------------------------------------------------

_EndpointValue :: forall a. Iso' (EndpointValue a) { unEndpointValue :: a }
_EndpointValue = _Newtype

--------------------------------------------------------------------------------

newtype Notification = Notification
  { notificationContractID :: ContractInstanceId
  , notificationContractEndpoint :: EndpointDescription
  , notificationContractArg :: RawJson
  }

derive instance Eq Notification

instance Show Notification where
  show a = genericShow a

instance EncodeJson Notification where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { notificationContractID: E.value :: _ ContractInstanceId
        , notificationContractEndpoint: E.value :: _ EndpointDescription
        , notificationContractArg: E.value :: _ RawJson
        }
    )

instance DecodeJson Notification where
  decodeJson = defer \_ -> D.decode $
    ( Notification <$> D.record "Notification"
        { notificationContractID: D.value :: _ ContractInstanceId
        , notificationContractEndpoint: D.value :: _ EndpointDescription
        , notificationContractArg: D.value :: _ RawJson
        }
    )

derive instance Generic Notification _

derive instance Newtype Notification _

--------------------------------------------------------------------------------

_Notification :: Iso' Notification { notificationContractID :: ContractInstanceId, notificationContractEndpoint :: EndpointDescription, notificationContractArg :: RawJson }
_Notification = _Newtype

--------------------------------------------------------------------------------

data NotificationError
  = EndpointNotAvailable ContractInstanceId EndpointDescription
  | MoreThanOneEndpointAvailable ContractInstanceId EndpointDescription
  | InstanceDoesNotExist ContractInstanceId
  | NotificationJSONDecodeError EndpointDescription RawJson String

derive instance Eq NotificationError

instance Show NotificationError where
  show a = genericShow a

instance EncodeJson NotificationError where
  encodeJson = defer \_ -> case _ of
    EndpointNotAvailable a b -> E.encodeTagged "EndpointNotAvailable" (a /\ b) (E.tuple (E.value >/\< E.value))
    MoreThanOneEndpointAvailable a b -> E.encodeTagged "MoreThanOneEndpointAvailable" (a /\ b) (E.tuple (E.value >/\< E.value))
    InstanceDoesNotExist a -> E.encodeTagged "InstanceDoesNotExist" a E.value
    NotificationJSONDecodeError a b c -> E.encodeTagged "NotificationJSONDecodeError" (a /\ b /\ c) (E.tuple (E.value >/\< E.value >/\< E.value))

instance DecodeJson NotificationError where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "NotificationError"
    $ Map.fromFoldable
        [ "EndpointNotAvailable" /\ D.content (D.tuple $ EndpointNotAvailable </$\> D.value </*\> D.value)
        , "MoreThanOneEndpointAvailable" /\ D.content (D.tuple $ MoreThanOneEndpointAvailable </$\> D.value </*\> D.value)
        , "InstanceDoesNotExist" /\ D.content (InstanceDoesNotExist <$> D.value)
        , "NotificationJSONDecodeError" /\ D.content (D.tuple $ NotificationJSONDecodeError </$\> D.value </*\> D.value </*\> D.value)
        ]

derive instance Generic NotificationError _

--------------------------------------------------------------------------------

_EndpointNotAvailable :: Prism' NotificationError { a :: ContractInstanceId, b :: EndpointDescription }
_EndpointNotAvailable = prism' (\{ a, b } -> (EndpointNotAvailable a b)) case _ of
  (EndpointNotAvailable a b) -> Just { a, b }
  _ -> Nothing

_MoreThanOneEndpointAvailable :: Prism' NotificationError { a :: ContractInstanceId, b :: EndpointDescription }
_MoreThanOneEndpointAvailable = prism' (\{ a, b } -> (MoreThanOneEndpointAvailable a b)) case _ of
  (MoreThanOneEndpointAvailable a b) -> Just { a, b }
  _ -> Nothing

_InstanceDoesNotExist :: Prism' NotificationError ContractInstanceId
_InstanceDoesNotExist = prism' InstanceDoesNotExist case _ of
  (InstanceDoesNotExist a) -> Just a
  _ -> Nothing

_NotificationJSONDecodeError :: Prism' NotificationError { a :: EndpointDescription, b :: RawJson, c :: String }
_NotificationJSONDecodeError = prism' (\{ a, b, c } -> (NotificationJSONDecodeError a b c)) case _ of
  (NotificationJSONDecodeError a b c) -> Just { a, b, c }
  _ -> Nothing
