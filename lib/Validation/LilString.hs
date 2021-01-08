module Validation.LilString
  ( type LilString
  , LilStringError(..)
  ) where

import Validation

-- Strings that are not shorter than two characters and not longer than five
newtype LilString = LilString { getMySpecialString :: String }
  deriving Show

data LilStringError = LongerThanFive | ShorterThanTwo
  deriving Show

instance Applicative f => Validate f LilString
  where
  type instance Raw LilString = String
  type instance Error LilString = LilStringError
  validate s
    | length s > 5 = pure $ Left LongerThanFive
    | length s < 2 = pure $ Left ShorterThanTwo
    | otherwise    = pure $ Right $ LilString s
