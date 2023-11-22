module Cardano.Api.Extended.IPC (
  mkLocalNodeConnectInfo,
) where

import Cardano.Api qualified as C

mkLocalNodeConnectInfo :: C.NetworkId -> FilePath -> C.LocalNodeConnectInfo C.CardanoMode
mkLocalNodeConnectInfo networkId socketPath =
  C.LocalNodeConnectInfo
    { C.localConsensusModeParams = C.CardanoModeParams epochSlots
    , C.localNodeNetworkId = networkId
    , C.localNodeSocketPath = C.File socketPath
    }
  where
    -- This a parameter needed only for the Byron era. Since the Byron
    -- era is over and the parameter has never changed it is ok to
    -- hardcode this. See comment on `Cardano.Api.ConsensusModeParams` in
    -- cardano-node.
    epochSlots = C.EpochSlots 21600 -- TODO: is this configurable? see below
