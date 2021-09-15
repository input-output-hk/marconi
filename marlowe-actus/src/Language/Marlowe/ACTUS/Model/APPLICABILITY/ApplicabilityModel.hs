{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.APPLICABILITY.ApplicabilityModel where

import           Data.Validation                                  (Validation (..))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms (ContractTerms (..), IPCB (IPCB_NTIED, IPCB_NTL),
                                                                   TermValidationError (..))

-- Applicability Validation Functions

-- Optional
_X :: a -> ContractTerms -> b -> Validation [TermValidationError] ContractTerms
_X _ ct _ = Success ct

-- The conditional term with c=1 is optional when any of the unconditional terms with c=0 is defined.
_X_I_1 :: [Bool] -> [Bool] -> ContractTerms -> [String] -> [String] -> Validation [TermValidationError] ContractTerms
_X_I_1 uncond cond ct@ContractTerms {..} uncondNames condNames
  | or uncond = Success ct
  | or cond = Failure [Required $ "The unconditional terms " ++ show uncondNames ++ " must be defined when any of " ++ show condNames ++ " are defined for contract type '" ++ show contractType ++ "'"]
  | otherwise = Success ct

-- If the unconditional term with c=0 in the group is defined, then at least one of the conditional terms with c=2 must be defined.
_X_I_2 :: Maybe a -> [Bool] -> ContractTerms -> String -> [String] -> Validation [TermValidationError] ContractTerms
_X_I_2 (Just _) cond ct _ _ | or cond = Success ct
_X_I_2 (Just _) _ ContractTerms {..} uncondName condNames = Failure [Required $ "At least one of the conditional terms in group " ++ show condNames ++ " must be defined when " ++ uncondName ++ " is defined for contract type '" ++ show contractType ++ "'"]
_X_I_2 Nothing _ ct _ _ = Success ct

-- at least one of the CAs with c=4 in this group has to be defined provided that CA IPCB of the group takes the value NTL
_X_I_4 :: [Bool] -> ContractTerms -> [String] -> Validation [TermValidationError] ContractTerms
_X_I_4 cond ct@ContractTerms {ct_IPCB = Just IPCB_NTL, ..} condNames =
  if or cond
    then Success ct
    else Failure [Required $ "At least one of the conditional terms in group " ++ show condNames ++ " must be defined when interest calculation base is NTL for contract type '" ++ show contractType ++ "'"]
_X_I_4 _ ct _ = Success ct

-- non-nullable / required
_NN :: Maybe a -> ContractTerms -> String -> Validation [TermValidationError] ContractTerms
_NN (Just _) ct _ = Success ct
_NN Nothing ContractTerms{..} n = Failure [Required $ "Contract term '" ++ n ++ "' is required for contract type '" ++ show contractType ++ "'"]

-- not applicable
_NA :: Maybe a -> ContractTerms -> String -> Validation [TermValidationError] ContractTerms
_NA (Just _) ContractTerms{..} n = Failure [NotApplicable $ "Contract term '" ++ n ++ "' is not applicable for contract type '" ++ show contractType ++ "'"]
_NA Nothing ct _ = Success ct

-- NN(I, 1, _) (If one is defined, all must be defined)
_NN_I_1 :: [Bool] -> ContractTerms -> [String] -> Validation [TermValidationError] ContractTerms
_NN_I_1 _cts ct@ContractTerms{..} ns
  | and _cts = Success ct
  | or _cts = Failure [Required $ "All contract terms in group " ++ show ns ++ " should be defined if one of them is defined for contract type '" ++ show contractType ++ "'"]
  | otherwise = Success ct

-- not nullable if CA IPCB of the group takes the value NTIED
_NN_I_3 :: Maybe a -> ContractTerms -> [Char] -> Validation [TermValidationError] ContractTerms
_NN_I_3 Nothing ContractTerms {ct_IPCB = Just IPCB_NTIED} n = Failure [Required $ "Contract term " ++ n ++ " must be defined when interest calculation base is NTIED"]
_NN_I_3 _ ct _ = Success ct
