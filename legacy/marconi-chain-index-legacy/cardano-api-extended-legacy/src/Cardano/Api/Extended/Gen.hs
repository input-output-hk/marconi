module Cardano.Api.Extended.Gen where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash.Class qualified as Crypto
import Hedgehog qualified as H
import Hedgehog.Gen qualified as H.Gen
import Hedgehog.Range qualified as H.Range

{- | Temporary utility to generate @C.'TxId'@ whose values reasonably can be expected
 - to be unique over small collections of generated events. It's only purpose is to
 provide unique ids for testing.
-}
genTxId :: H.Gen C.TxId
genTxId = C.TxId . Crypto.castHash . Crypto.hashWith id <$> H.Gen.bytes (H.Range.singleton 30)
