module HaskellEditor.Types where

import Prelude
import Analytics (class IsEvent, Event)
import Analytics as A
import Data.Maybe (Maybe(..))
import Halogen.Monaco (KeyBindings)
import Halogen.Monaco as Monaco

data Action
  = Compile
  | ChangeKeyBindings KeyBindings
  | LoadScript String
  | HandleEditorMessage Monaco.Message
  | ShowBottomPanel Boolean
  | SendResultToSimulator
  | SendResultToBlockly

defaultEvent :: String -> Event
defaultEvent s = A.defaultEvent $ "Haskell." <> s

instance actionIsEvent :: IsEvent Action where
  toEvent Compile = Just $ defaultEvent "Compile"
  toEvent (ChangeKeyBindings _) = Just $ defaultEvent "ChangeKeyBindings"
  toEvent (LoadScript _) = Just $ defaultEvent "LoadScript"
  toEvent (HandleEditorMessage _) = Just $ defaultEvent "HandleEditorMessage"
  toEvent (ShowBottomPanel _) = Just $ defaultEvent "ShowBottomPanel"
  toEvent SendResultToSimulator = Just $ defaultEvent "SendResultToSimulator"
  toEvent SendResultToBlockly = Just $ defaultEvent "SendResultToBlockly"
