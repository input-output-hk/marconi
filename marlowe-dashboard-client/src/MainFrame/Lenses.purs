module MainFrame.Lenses
  ( _overlay
  , _screen
  , _card
  , _on
  ) where

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import MainFrame.Types (Card, ContractInstance, ContractTemplate, Notification, Overlay, Screen, State)

_overlay :: Lens' State (Maybe Overlay)
_overlay = prop (SProxy :: SProxy "overlay")

_screen :: Lens' State Screen
_screen = prop (SProxy :: SProxy "screen")

_card :: Lens' State (Maybe Card)
_card = prop (SProxy :: SProxy "card")

_notifications :: Lens' State (Array Notification)
_notifications = prop (SProxy :: SProxy "notifications")

_templates :: Lens' State (Array ContractTemplate)
_templates = prop (SProxy :: SProxy "templates")

_contracts :: Lens' State (Array ContractInstance)
_contracts = prop (SProxy :: SProxy "contracts")

_on :: Lens' State Boolean
_on = prop (SProxy :: SProxy "on")
