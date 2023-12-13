{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Cardano.ChainIndex.Api.Rest.Endpoint.Time where

import Control.Monad.Trans (liftIO)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import Marconi.Cardano.ChainIndex.Api.Types (HttpServerConfig)
import Marconi.Core.JsonRpc (ReaderHandler)
import Servant (Get, PlainText, (:>))

------------------
-- Method types --
------------------

type GetTime = "time" :> Get '[PlainText] String

----------------------------
-- Query and result types --
----------------------------

{- | Returns current time as REST response. Used for testing the http server outside of jsonrpc
 protocol.
-}
getTimeHandler :: ReaderHandler HttpServerConfig String
getTimeHandler = timeString <$> liftIO getCurrentTime
  where
    timeString = formatTime defaultTimeLocale "%T"
