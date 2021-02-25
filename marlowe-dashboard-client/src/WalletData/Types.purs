module WalletData.Types
  ( WalletLibrary
  , WalletNicknameKey
  , WalletDetails
  ) where

import Data.Tuple (Tuple)
import Data.Map (Map)
import Marlowe.Semantics (PubKey)

type WalletLibrary
  = Map WalletNicknameKey WalletDetails

-- make the nickname (string) the first key so we get alphabetical ordering for free
type WalletNicknameKey
  = Tuple String PubKey

type WalletDetails
  = { userHasPickedUp :: Boolean
    }
