-- File auto generated by purescript-bridge! --
module DemoContract where

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
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(Proxy))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

data DemoContract = DemoContract

derive instance Eq DemoContract

instance Show DemoContract where
  show a = genericShow a

instance EncodeJson DemoContract where
  encodeJson = defer \_ -> E.encode E.enum

instance DecodeJson DemoContract where
  decodeJson = defer \_ -> D.decode D.enum

derive instance Generic DemoContract _

instance Enum DemoContract where
  succ = genericSucc
  pred = genericPred

instance Bounded DemoContract where
  bottom = genericBottom
  top = genericTop

--------------------------------------------------------------------------------

_DemoContract :: Iso' DemoContract Unit
_DemoContract = iso (const unit) (const DemoContract)
