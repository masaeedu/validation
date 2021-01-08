{-# LANGUAGE DeriveGeneric, DeriveAnyClass, UndecidableSuperClasses #-}
module Validation where

import Barbies (FunctorB(..), TraversableB(..), ConstraintsB(..), bsequence, bmapC, Barbie(..))
import Data.Kind (Type, Constraint)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Sum
import Data.Bifunctor (Bifunctor(..))
import GHC.Generics (Generic(..))
import Data.Aeson (FromJSON(..), ToJSON(..))

-- Augmented newtype for barbies
newtype Augmented f b g = Augmented { getAugmented :: b (Compose f g) }
  deriving Generic

type Partial = Augmented Maybe

instance (Functor g, FunctorB b) => FunctorB (Augmented g b)
  where
  bmap f = Augmented . bmap (Compose . fmap f . getCompose) . getAugmented

instance (Traversable g, TraversableB b) => TraversableB (Augmented g b)
  where
  btraverse f = fmap Augmented . btraverse (fmap Compose . traverse f . getCompose) . getAugmented

newtype Shown x = Shown { getShown :: String }
instance Show (Shown x)
  where
  show = getShown

instance
  ( Show1' f
  , Show1' g
  , AllB Show b
  , Show (b Shown)
  , FunctorB b
  , ConstraintsB b
  ) =>
  Show (Augmented f b g)
  where
  show = show . bmapC @Show (Shown . show . getCompose) . getAugmented

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

-- Some random constraint constructors we need
class Trivial x
instance Trivial x

type Show1' f = (forall x. Show x => Show (f x)) :: Constraint

class (x ~ y) => Equals x y
instance Equals x x

-- Class for types that represent a small "validated" subset of some larger "raw" type, where the raw
-- type can be converted to a sum of some error representation and the the validated type in a given
-- context (e.g. purely with @Identity@, or with arbitrary side effects in @IO@).
type family Raw v :: Type
type family Error v :: Type
class Functor f => Validate f v
  where
  validate :: Raw v -> f (Either (Error v) v)

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
type instance Raw (ValidData v) = UnvalidatedData v
type instance Error (ValidData v) = InvalidData v
instance Validate f v => Validate f (ValidData v)
  where
  validate = fmap (bimap Invalid Valid). validate . getUnvalidated

validateData :: forall v f.
  Validate f v =>
  UnvalidatedData v ->
  Compose f ValidatedData v
validateData = Compose . fmap (either InL InR) . validate

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

type instance Raw (Barbie b ValidData) = b UnvalidatedData
type instance Error (Barbie b ValidData) = Partial b InvalidData
instance
  ( Applicative f
  , AllB (Validate f) b
  , TraversableB b
  , ConstraintsB b
  ) =>
  Validate f (Barbie b ValidData)
  where
  validate = fmap (fmap Barbie) . validateFields . bmapC @(Validate f) @b @UnvalidatedData @(Compose f ValidatedData) validateData
