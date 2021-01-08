{-# LANGUAGE TypeFamilies, DeriveGeneric, DeriveAnyClass, DerivingVia, LambdaCase, OverloadedStrings #-}
module Lib where

import Data.Maybe (maybeToList)
import Data.Aeson (FromJSON(..), decode)
import Data.Functor.Identity (Identity(..))
import GHC.Generics (Generic(..))
import Barbies (ConstraintsB(..), FunctorB(..), TraversableB(..), AllBF, Rec(..), Barbie(..))
import Data.Coerce (Coercible)

import Validation
import Validation.LilString
import Validation.Positive

-- A type with fields that require validation
data SomeRequestParams f = SomeRequestParams
  { someBool :: Bool
  , somePositiveInt :: f Positive
  , someLilString :: f LilString
  }
  deriving stock Generic
  deriving anyclass (FunctorB, TraversableB, ConstraintsB)

type instance Raw (SomeRequestParams ValidData) = SomeRequestParams UnvalidatedData
type instance Error (SomeRequestParams ValidData) = Partial SomeRequestParams InvalidData
deriving via (Barbie SomeRequestParams ValidData) instance (Applicative f, forall x y. Coercible x y => Coercible (f x) (f y)) => Validate f (SomeRequestParams ValidData)

deriving instance AllBF Show f SomeRequestParams => Show (SomeRequestParams f)
deriving instance AllB (ValidateWhere FromJSON Trivial Trivial) SomeRequestParams => FromJSON (SomeRequestParams UnvalidatedData)

tests :: [SomeRequestParams UnvalidatedData]
tests =
  [ SomeRequestParams
    { someBool = False
    , somePositiveInt = Unvalidated (-10)
    , someLilString = Unvalidated "foo"
    }

{-

Left SomeRequestParams
    { someBool = False
    , somePositiveInt = Just IsLessThanZero
    , someLilString = Nothing
    }
-}

  , SomeRequestParams
    { someBool = True
    , somePositiveInt = Unvalidated 42
    , someLilString = Unvalidated "this is bad!"
    }

{-
Left SomeRequestParams
    { someBool = True
    , somePositiveInt = Nothing
    , someLilString = Just LongerThanFive
    }
-}

  , SomeRequestParams
    { someBool = False
    , somePositiveInt = Unvalidated 42
    , someLilString = Unvalidated "yay!"
    }

{-
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
  ]

  ++ maybeToList (decode "{ \"someBool\": false, \"somePositiveInt\": 11, \"someLilString\": \"good\" }")
{-
Right
    ( SomeRequestParams
        { someBool = False
        , somePositiveInt = Positive
            { getPositive = 11 }
        , someLilString = LilString
            { getMySpecialString = "good" }
        }
    )
-}

result :: [Either (Partial SomeRequestParams InvalidData) (SomeRequestParams ValidData)]
result = runIdentity . validate <$> tests
