{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.Sidechain.Experimental.Api.Rest.Endpoint.Metrics where

import Control.Monad.Trans (liftIO)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Marconi.Core.JsonRpc (ReaderHandler)
import Marconi.Sidechain.Experimental.Api.Types (SidechainHttpServerConfig)
import Prometheus qualified as P
import Servant (Get, PlainText, (:>))

------------------
-- Method types --
------------------

type GetMetrics = "metrics" :> Get '[PlainText] Text

----------------------------
-- Query and result types --
----------------------------

getMetricsHandler :: ReaderHandler SidechainHttpServerConfig Text
getMetricsHandler = liftIO $ Text.decodeUtf8 . BS.toStrict <$> P.exportMetricsAsText
