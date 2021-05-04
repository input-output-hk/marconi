-- | A decentralized exchange for arbitrary token pairs following the
-- [Uniswap protocol](https://uniswap.org/whitepaper.pdf).
--
-- Details:
--
--  - 'OffChain' contains the instance endpoints and client functionality
--  - 'OnChain' contains the validation logic
--  - 'Data' conains a few common datatypes for working with this contract
--  - 'Pool' contains functions needed by both on-chain and off-chain code
--    related to working with liquidity pools.
module Plutus.Contracts.Uniswap
  ( module OnChain
  , module OffChain
  , module Data
  , module Pool
  ) where

import           Plutus.Contracts.Uniswap.Data     as Data
import           Plutus.Contracts.Uniswap.OffChain as OffChain
import           Plutus.Contracts.Uniswap.OnChain  as OnChain
import           Plutus.Contracts.Uniswap.Pool     as Pool
