{-# LANGUAGE DeriveGeneric, DeriveAnyClass, UndecidableSuperClasses, AllowAmbiguousTypes #-}
module Validation where

import Barbies (FunctorB(..), TraversableB(..), ConstraintsB(..), bsequence, bmapC, Barbie(..))
import Data.Kind (Type)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Sum
import Data.Bifunctor (Bifunctor(..))
import GHC.Generics (Generic(..))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Coerce (Coercible, coerce)

import Constraints
import Barbies.Augmented

-- Class for types that represent a small "validated" subset of some larger "raw" type, where the raw
-- type can be converted to a sum of some error representation and the the validated type in a given
-- context (e.g. purely with @Identity@, or with arbitrary side effects in @IO@).
class Functor f => Validate f v
  where
  type family Raw v :: Type
  type family Error v :: Type
  validate :: Raw v -> f (Either (Error v) v)
  extract :: v -> Raw v

  default extract :: Coercible v (Raw v) => v -> Raw v
  extract = coerce

-- Sometimes you need to pass @Type -> Constraint@-kinded constructors into stuff (e.g. Barbies), and when that involves
-- requiring something about the associated types in @Validate@, you'll need a class like this
class (cr (Raw v), ce (Error v), cv v) => ValidateWhere cr ce cv v
instance (cr (Raw v), ce (Error v), cv v) => ValidateWhere cr ce cv v

-- {{{ VData

-- Data that is either unvalidated, invalid, or valid

newtype UnvalidatedData v
  where
  Unvalidated :: { getUnvalidated :: Raw v } -> UnvalidatedData v

deriving newtype instance Generic (Raw v) => Generic (UnvalidatedData v)
deriving newtype instance Show (Raw v) => Show (UnvalidatedData v)
deriving newtype instance FromJSON (Raw v) => FromJSON (UnvalidatedData v)
deriving newtype instance ToJSON (Raw v) => ToJSON (UnvalidatedData v)

newtype InvalidData v
  where
  Invalid :: { getInvalid :: Error v } -> InvalidData v

deriving newtype instance Generic (Error v) => Generic (InvalidData v)
deriving newtype instance Show (Error v) => Show (InvalidData v)
deriving newtype instance FromJSON (Error v) => FromJSON (InvalidData v)
deriving newtype instance ToJSON (Error v) => ToJSON (InvalidData v)

newtype ValidData v
  where
  Valid :: { getValid :: v } -> ValidData v
  deriving newtype (Generic, Show, FromJSON, ToJSON)

type ValidatedData = Sum InvalidData ValidData

-- If a thing can be validated, so can a @VData@ wrapper around it
instance Validate f v => Validate f (ValidData v)
  where
  type instance Raw (ValidData v) = UnvalidatedData v
  type instance Error (ValidData v) = InvalidData v
  validate = fmap (bimap Invalid Valid). validate . getUnvalidated
  extract = Unvalidated . extract @f @v . getValid

validateFields :: forall b f. (Applicative f, TraversableB b) => b (Compose f ValidatedData) -> f (Either (Partial b InvalidData) (b ValidData))
validateFields = fmap go . bsequence
  where
  go :: b ValidatedData -> Either (Partial b InvalidData) (b ValidData)
  go input = case tryValidate input of
    Just v -> Right v
    Nothing -> Left $ tryInvalidate input

  tryValidate :: b ValidatedData -> Maybe (b ValidData)
  tryValidate = btraverse $ \case
    InL (Invalid _) -> Nothing
    InR (Valid v) -> Just $ Valid v

  tryInvalidate :: b ValidatedData -> Partial b InvalidData
  tryInvalidate = (Augmented .) $ bmap $ \case
    InR (Valid _) -> Compose Nothing
    InL (Invalid e) -> Compose $ Just $ Invalid e

instance
  ( Applicative f
  , AllB (Validate f) b
  , TraversableB b
  , ConstraintsB b
  ) =>
  Validate f (Barbie b ValidData)
  where
  type instance Raw (Barbie b ValidData) = b UnvalidatedData
  type instance Error (Barbie b ValidData) = Partial b InvalidData
  validate = fmap (fmap Barbie) . validateFields . bmapC @(Validate f) @b @UnvalidatedData @(Compose f ValidatedData) (Compose . fmap (either InL InR) . validate)
  extract = bmapC @(Validate f) (extract @f) . getBarbie

instance {-# OVERLAPS #-}
  ( Show1' f
  , Functor f
  , AllB (ValidateWhere Trivial Show Trivial) b
  , Show (b Shown)
  , FunctorB b
  , ConstraintsB b
  ) =>
  Show (Augmented f b InvalidData)
  where
  show = show . bmapC @(ValidateWhere Trivial Show Trivial) (Shown . show . fmap getInvalid . getCompose) . getAugmented
