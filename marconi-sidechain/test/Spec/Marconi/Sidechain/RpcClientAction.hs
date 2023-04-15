module Spec.Marconi.Sidechain.RpcClientAction where

import Control.Concurrent.STM (atomically)
import Control.Lens ((^.))
import Data.Proxy (Proxy (Proxy))
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as UIQ
import Marconi.Sidechain.Api.Routes (GetCurrentSyncedBlockResult, GetUtxosFromAddressParams (GetUtxosFromAddressParams),
                                     GetUtxosFromAddressResult, JsonRpcAPI)
import Marconi.Sidechain.Api.Types (SidechainEnv, sidechainAddressUtxoIndexer, sidechainEnvIndexers)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.JsonRpc.Client.Types ()
import Network.JsonRpc.Types (JsonRpcResponse)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, HasClient (Client), Scheme (Http), client, hoistClient,
                       mkClientEnv, runClientM)

-- | Type for the storable action and RPC client Action pair, to simplify the type signatures.
data RpcClientAction = RpcClientAction
    { insertUtxoEventsAction  :: !InsertUtxoEventsCallback
    , queryAddressUtxosAction :: !(String -> IO (JsonRpcResponse String GetUtxosFromAddressResult))
    , querySyncedBlockAction  :: !(IO (JsonRpcResponse String GetCurrentSyncedBlockResult))
    }

mkRpcClientAction :: SidechainEnv -> Port -> IO RpcClientAction
mkRpcClientAction env port = do
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (baseUrl port)
        (_ :<|> _ :<|> rpcSyncPoint :<|> rpcUtxos :<|> _ :<|> _ :<|> _)
          = mkHoistedHttpRpcClient clientEnv
    pure $ RpcClientAction (mkInsertUtxoEventsCallback env)
                           (\a -> rpcUtxos $ GetUtxosFromAddressParams a Nothing)
                           (rpcSyncPoint "")

baseUrl :: Warp.Port -> BaseUrl
baseUrl port = BaseUrl Http "localhost" port ""

-- | An alias for storing events in the indexer.
type InsertUtxoEventsCallback = [Utxo.StorableEvent Utxo.UtxoHandle] -> IO () --  callback

mkInsertUtxoEventsCallback :: SidechainEnv -> InsertUtxoEventsCallback
mkInsertUtxoEventsCallback env =
  let cb :: Utxo.UtxoIndexer -> IO ()
      cb = atomically . UIQ.updateEnvState (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)
  in mocUtxoWorker cb

-- | Insert events, and perform the callback.
-- Note, the in-memory DB provides the isolation we need per property test as the memory cache
-- is owned and visible only to the process that opened the connection
mocUtxoWorker
  :: (Utxo.UtxoIndexer -> IO ()) -- ^  callback to be refreshed
  -> [Utxo.StorableEvent Utxo.UtxoHandle] -- ^  events to store
  -> IO ()
mocUtxoWorker callback events =
  let depth :: Utxo.Depth
      depth = Utxo.Depth (1 + length events) -- use in-memory store
  in
    Utxo.open ":memory:" depth False >>= Storable.insertMany events >>= callback

-- hoist http servant client from clientM to IO
mkHoistedHttpRpcClient :: ClientEnv -> Client IO JsonRpcAPI
mkHoistedHttpRpcClient cEnv =
    hoistClient
        hoistClientApi
        ( fmap (either (error . show) id)
        . flip runClientM cEnv
        )
        (client hoistClientApi)
 where
    hoistClientApi :: Proxy JsonRpcAPI
    hoistClientApi = Proxy
