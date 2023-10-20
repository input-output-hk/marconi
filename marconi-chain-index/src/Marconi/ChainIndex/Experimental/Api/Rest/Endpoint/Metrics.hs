{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Marconi.ChainIndex.Experimental.Api.Rest.Endpoint.Metrics where

import Control.Monad.Trans (liftIO)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Marconi.ChainIndex.Experimental.Api.Types (HttpServerConfig)
import Marconi.Core.JsonRpc (ReaderHandler)
import Prometheus qualified as P
import Servant (Get, PlainText, (:>))

------------------
-- Method types --
------------------

type GetMetrics = "metrics" :> Get '[PlainText] Text

----------------------------
-- Query and result types --
----------------------------

getMetricsHandler :: ReaderHandler HttpServerConfig Text
getMetricsHandler = liftIO $ Text.decodeUtf8 . BS.toStrict <$> P.exportMetricsAsText
