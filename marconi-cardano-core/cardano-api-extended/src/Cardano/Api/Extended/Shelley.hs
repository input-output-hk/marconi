module Cardano.Api.Extended.Shelley where

import Cardano.Api qualified as C

-- | Converts a 'ShelleyBasedEra' to 'EraInMode'.
toShelleyEraInCardanoMode :: C.ShelleyBasedEra era -> C.EraInMode era C.CardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraShelley = C.ShelleyEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraAllegra = C.AllegraEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraMary = C.MaryEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraAlonzo = C.AlonzoEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraBabbage = C.BabbageEraInCardanoMode
toShelleyEraInCardanoMode C.ShelleyBasedEraConway = C.ConwayEraInCardanoMode

{- | Converts a 'CardanoEra' to the more specific 'ShelleyBasedEra'. Compare to
@C.'shelleyBasedToCardanoEra'@, which takes a 'ShelleyBasedEra' as input.
-}
cardanoEraToShelleyBasedEra :: C.CardanoEra era -> Maybe (C.ShelleyBasedEra era)
cardanoEraToShelleyBasedEra C.ByronEra = Nothing
cardanoEraToShelleyBasedEra C.ShelleyEra = Just C.ShelleyBasedEraShelley
cardanoEraToShelleyBasedEra C.AllegraEra = Just C.ShelleyBasedEraAllegra
cardanoEraToShelleyBasedEra C.MaryEra = Just C.ShelleyBasedEraMary
cardanoEraToShelleyBasedEra C.AlonzoEra = Just C.ShelleyBasedEraAlonzo
cardanoEraToShelleyBasedEra C.BabbageEra = Just C.ShelleyBasedEraBabbage
cardanoEraToShelleyBasedEra C.ConwayEra = Just C.ShelleyBasedEraConway
