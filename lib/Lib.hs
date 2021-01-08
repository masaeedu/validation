{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass, DerivingVia, LambdaCase #-}
module Lib where

import Data.Functor.Identity (Identity(..))
import GHC.Generics (Generic(..))
import Barbies (ConstraintsB(..), FunctorB(..), TraversableB(..), AllBF, Rec(..))
import Validation

-- Strings that are longer two characters and shorter than five characters
newtype LilString = LilString { getMySpecialString :: String }
  deriving Show

data LilStringError = LongerThanFive | ShorterThanTwo
  deriving Show

instance Validate LilString
  where
  type Raw LilString = String
  type Error LilString = LilStringError
  validate s
    | length s > 5 = pure $ Left LongerThanFive
    | length s < 2 = pure $ Left ShorterThanTwo
    | otherwise    = pure $ Right $ LilString s

-- Positive integers
newtype Positive = Positive { getPositive :: Int }
  deriving Show

data PositiveError = IsLessThanZero
  deriving Show

instance Validate Positive
  where
  type Raw Positive = Int
  type Error Positive = PositiveError
  validate i
    | i < 0 = pure $ Left IsLessThanZero
    | otherwise = pure $ Right $ Positive i

-- A type with fields that require validation
data SomeRequestParams f = SomeRequestParams
  { someBool :: Bool
  , somePositiveInt :: f Positive
  , someLilString :: f LilString
  }
  deriving stock Generic
  deriving anyclass (FunctorB, TraversableB, ConstraintsB)

deriving instance AllBF Show f SomeRequestParams => Show (SomeRequestParams f)

tests :: [SomeRequestParams UnvalidatedData]
tests =
  [ SomeRequestParams
    { someBool = False
    , somePositiveInt = Unvalidated (-10)
    , someLilString = Unvalidated "foo"
    }

  , SomeRequestParams
    { someBool = True
    , somePositiveInt = Unvalidated 42
    , someLilString = Unvalidated "this is bad!"
    }

  , SomeRequestParams
    { someBool = False
    , somePositiveInt = Unvalidated 42
    , someLilString = Unvalidated "yay!"
    }
  ]

result :: [Either (Partial SomeRequestParams InvalidData) (SomeRequestParams ValidData)]
result = runIdentity . validate <$> tests
{-
Left SomeRequestParams
    { someBool = False
    , somePositiveInt = Just IsLessThanZero
    , someLilString = Nothing
    }

Left SomeRequestParams
    { someBool = True
    , somePositiveInt = Nothing
    , someLilString = Just LongerThanFive
    }

Right
    ( SomeRequestParams
        { someBool = False
        , somePositiveInt = Positive
            { getPositive = 42 }
        , someLilString = LilString
            { getMySpecialString = "yay!" }
        }
    )
-}
