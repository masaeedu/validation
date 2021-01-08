module Validation.Positive
  ( type Positive
  , PositiveError(..)
  ) where

import Validation

-- Positive integers
newtype Positive = Positive { getPositive :: Int }
  deriving Show

data PositiveError = IsLessThanZero
  deriving Show

instance Applicative f => Validate f Positive
  where
  type instance Raw Positive = Int
  type instance Error Positive = PositiveError
  validate i
    | i < 0 = pure $ Left IsLessThanZero
    | otherwise = pure $ Right $ Positive i
