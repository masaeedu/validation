module Validation.Past
  ( type Past
  , FutureError(..)
  ) where

import Data.Time (UTCTime, getCurrentTime)
import Validation (Validate(..))

-- Strings that are not shorter than two characters and not longer than five
newtype Past = Past { getPast :: UTCTime }
  deriving Show

data FutureError = TimeWasInTheFuture
  deriving Show

instance Validate IO Past
  where
  type instance Raw Past = UTCTime
  type instance Error Past = FutureError
  validate t = do
    now <- getCurrentTime
    pure $ if now < t
      then Left TimeWasInTheFuture
      else Right $ Past t
