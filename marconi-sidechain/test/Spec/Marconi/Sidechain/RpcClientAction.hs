module Spec.Marconi.Sidechain.RpcClientAction where

import Cardano.Api (AssetName, PolicyId)
import Control.Concurrent.STM (atomically)
import Control.Lens ((^.))
import Control.Monad.Except (runExceptT)
import Data.Proxy (Proxy (Proxy))
import Marconi.ChainIndex.Indexers.MintBurn qualified as MintBurn
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types (SecurityParam (SecurityParam))
import Marconi.Core.Storable qualified as Storable
import Marconi.Sidechain.Api.Query.Indexers.MintBurn qualified as MIQ
import Marconi.Sidechain.Api.Query.Indexers.Utxo qualified as UIQ
import Marconi.Sidechain.Api.Routes (GetCurrentSyncedBlockResult,
                                     GetTxsBurningAssetIdParams (GetTxsBurningAssetIdParams),
                                     GetTxsBurningAssetIdResult, GetUtxosFromAddressParams (GetUtxosFromAddressParams),
                                     GetUtxosFromAddressResult, JsonRpcAPI)
import Marconi.Sidechain.Api.Types (SidechainEnv, mintBurnIndexerEnvIndexer, sidechainAddressUtxoIndexer,
                                    sidechainEnvIndexers, sidechainMintBurnIndexer)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.JsonRpc.Client.Types ()
import Network.JsonRpc.Types (JsonRpcResponse)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.API ((:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl), ClientEnv, HasClient (Client), Scheme (Http), client, hoistClient,
                       mkClientEnv, runClientM)
import System.Exit (exitFailure)

-- | Type for the storable action and RPC client Action pair, to simplify the type signatures.
data RpcClientAction = RpcClientAction
    { insertUtxoEventsAction     :: !InsertUtxoEventsCallback
    , insertMintBurnEventsAction :: !InsertMintBurnEventsCallback
    , queryAddressUtxosAction    :: !(String -> IO (JsonRpcResponse String GetUtxosFromAddressResult))
    , querySyncedBlockAction     :: !(IO (JsonRpcResponse String GetCurrentSyncedBlockResult))
    , queryMintBurnAction        :: !((PolicyId, AssetName) -> IO (JsonRpcResponse String GetTxsBurningAssetIdResult))
    }

mkRpcClientAction :: SidechainEnv -> Port -> IO RpcClientAction
mkRpcClientAction env port = do
    manager <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager (baseUrl port)
        (_ :<|> _ :<|> rpcSyncPoint :<|> rpcUtxos :<|> rpcMinting :<|> _ :<|> _)
          = mkHoistedHttpRpcClient clientEnv
    pure $ RpcClientAction (mkInsertUtxoEventsCallback env)
                           (mkInsertMintBurnEventsCallback env)
                           (\a -> rpcUtxos $ GetUtxosFromAddressParams a Nothing maxBound)
                           (rpcSyncPoint "")
                           (\(p,a) -> rpcMinting $ GetTxsBurningAssetIdParams p a Nothing)

baseUrl :: Warp.Port -> BaseUrl
baseUrl port = BaseUrl Http "localhost" port ""

-- | An alias for storing events in the indexer.
type InsertUtxoEventsCallback = [Utxo.StorableEvent Utxo.UtxoHandle] -> IO () --  callback

-- | An alias for storing events in the indexer.
type InsertMintBurnEventsCallback = [Utxo.StorableEvent MintBurn.MintBurnHandle] -> IO () --  callback

mkInsertUtxoEventsCallback :: SidechainEnv -> InsertUtxoEventsCallback
mkInsertUtxoEventsCallback env =
  let cb :: Utxo.UtxoIndexer -> IO ()
      cb = atomically . UIQ.updateEnvState (env ^. sidechainEnvIndexers . sidechainAddressUtxoIndexer)
  in mocUtxoWorker cb

mkInsertMintBurnEventsCallback :: SidechainEnv -> InsertMintBurnEventsCallback
mkInsertMintBurnEventsCallback env =
  let cb :: MintBurn.MintBurnIndexer -> IO ()
      cb = atomically . MIQ.updateEnvState
              (env ^. sidechainEnvIndexers . sidechainMintBurnIndexer . mintBurnIndexerEnvIndexer)
  in mocMintBurnWorker cb

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
      handleError = either (const exitFailure) pure
  in
    runExceptT (Utxo.open ":memory:" depth False >>= Storable.insertMany events)
    >>= handleError
    >>= callback

-- | Insert events, and perform the callback.
-- Note, the in-memory DB provides the isolation we need per property test as the memory cache
-- is owned and visible only to the process that opened the connection
mocMintBurnWorker
  :: (MintBurn.MintBurnIndexer -> IO ()) -- ^  callback to be refreshed
  -> [MintBurn.StorableEvent MintBurn.MintBurnHandle] -- ^  events to store
  -> IO ()
mocMintBurnWorker callback events =
  let depth = SecurityParam (fromIntegral $ 1 + length events) -- use in-memory store
      handleError = either (const exitFailure) pure
  in runExceptT (MintBurn.open ":memory:" depth >>= Storable.insertMany events)
    >>= handleError
    >>= callback

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
