{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass, DerivingVia, LambdaCase, OverloadedStrings #-}
module Main where

import Data.Time
import Data.Fixed
import Data.Maybe (maybeToList)
import Data.Aeson (FromJSON(..), decode)
import GHC.Generics (Generic(..))
import Barbies (ConstraintsB(..), FunctorB(..), TraversableB(..), AllBF, Rec(..), Barbie(..))

import Text.Pretty.Simple

import Constraints
import Barbies.Augmented

import Validation
import Validation.LilString
import Validation.Positive
import Validation.Past


-- A type with fields that require validation
data SomeRequestParams f = SomeRequestParams
  { someBool :: Bool
  , somePositiveInt :: f Positive
  , someLilString :: f LilString
  , somePastTime :: f Past
  }
  deriving stock Generic
  deriving anyclass (FunctorB, TraversableB, ConstraintsB)

deriving via (Barbie SomeRequestParams ValidData) instance Validate IO (SomeRequestParams ValidData)

deriving instance AllBF Show f SomeRequestParams => Show (SomeRequestParams f)
deriving instance AllB (ValidateWhere FromJSON Trivial Trivial) SomeRequestParams => FromJSON (SomeRequestParams UnvalidatedData)


mkUTCTime :: (Integer, Int, Int)
          -> (Int, Int, Pico)
          -> UTCTime
mkUTCTime (year, mon, day) (hour, minute, sec) =
  UTCTime (fromGregorian year mon day)
          (timeOfDayToTime (TimeOfDay hour minute sec))

tests :: [SomeRequestParams UnvalidatedData]
tests =
  [ SomeRequestParams
    { someBool = False
    , somePositiveInt = Unvalidated (-10)
    , someLilString = Unvalidated "foo"
    , somePastTime = Unvalidated $ mkUTCTime (2022, 1, 1) (0, 0, 0)
    }

{-

Left SomeRequestParams
    { someBool = False
    , somePositiveInt = Just IsLessThanZero
    , someLilString = Nothing
    , somePastTime = Just TimeWasInThePast
    }
-}

  , SomeRequestParams
    { someBool = True
    , somePositiveInt = Unvalidated 42
    , someLilString = Unvalidated "this is bad!"
    , somePastTime = Unvalidated $ mkUTCTime (2020, 1, 1) (0, 0, 0)
    }

{-
Left SomeRequestParams
    { someBool = True
    , somePositiveInt = Nothing
    , someLilString = Just LongerThanFive
    , somePastTime = Nothing
    }
-}

  , SomeRequestParams
    { someBool = False
    , somePositiveInt = Unvalidated 42
    , someLilString = Unvalidated "yay!"
    , somePastTime = Unvalidated $ mkUTCTime (2020, 1, 1) (0, 0, 0)
    }

{-
Right
    ( SomeRequestParams
        { someBool = False
        , somePositiveInt = Positive
            { getPositive = 42 }
        , someLilString = LilString
            { getMySpecialString = "yay!" }
        , somePastTime = Future
            { getPast = 2020 - 01 - 01 00 : 00 : 00 UTC }
        }
    )
-}
  ]

  ++ maybeToList (decode "{ \"someBool\": false, \"somePositiveInt\": 11, \"someLilString\": \"good\", \"somePastTime\": \"2020-01-01T00:00:00Z\" }")
{-
Right
    ( SomeRequestParams
        { someBool = False
        , somePositiveInt = Positive
            { getPositive = 11 }
        , someLilString = LilString
            { getMySpecialString = "good" }
        , somePastTime = Future
            { getFuture = 2020 - 01 - 01 00 : 00 : 00 UTC }
        }
    )
-}

result :: IO [Either (Partial SomeRequestParams InvalidData) (SomeRequestParams ValidData)]
result = traverse validate tests
main :: IO ()
main = do
  _ <- result >>= traverse pPrint
  pure ()
