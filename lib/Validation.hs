{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ImpredicativeTypes, UndecidableSuperClasses #-}
module Validation where

import Barbies (FunctorB(..), TraversableB(..), ConstraintsB(..), bsequence, bmapC)
import Data.Kind (Type, Constraint)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))
import Data.Bifunctor (Bifunctor(..))
import GHC.Generics (Generic(..))
import Data.Aeson (FromJSON(..), ToJSON(..))

-- Some random constraint constructors we need
class Trivial x
instance Trivial x

type Show1' f = (forall x. Show x => Show (f x)) :: Constraint

class (x ~ y) => Equals x y
instance Equals x x

-- Class for types that represent a small "validated" subset of some larger "raw" type, where the raw
-- type can be converted to a sum of some error representation and the the validated type in a given
-- context (e.g. purely with @Identity@, or with arbitrary side effects in @IO@).
class Functor (Context v) => Validate v
  where
  type Context v :: Type -> Type
  type Context v = Identity
  type Raw v :: Type
  type Error v :: Type
  validate :: Raw v -> Context v (Either (Error v) v)

-- Sometimes you need to pass @Type -> Constraint@-kinded constructors into stuff (e.g. Barbies), and when that involves
-- requiring something about the associated types in @Validate@, you'll need a class like this
class (Validate v, cf (Context v), cr (Raw v), ce (Error v), cv v) => ValidateWhere cf cr ce cv v
instance (Validate v, cf (Context v), cr (Raw v), ce (Error v), cv v) => ValidateWhere cf cr ce cv v

-- {{{ VData

-- Data that is either unvalidated, invalid, or valid
data VData (validationState :: Maybe Bool) v
  where
  Unvalidated :: Validate v => { getUnvalidated :: Raw v   } -> UnvalidatedData v
  Invalid     :: Validate v => { getInvalid     :: Error v } -> InvalidData     v
  Valid       ::               { getValid       :: v       } -> ValidData       v

type UnvalidatedData = VData 'Nothing
type InvalidData = VData ('Just 'False)
type ValidData = VData ('Just 'True)

-- If a thing can be validated, so can a @VData@ wrapper around it
instance Validate v => Validate (ValidData v)
  where
  type Context (ValidData v) = Context v
  type Raw (ValidData v) = UnvalidatedData v
  type Error (ValidData v) = InvalidData v
  validate = fmap (bimap Invalid Valid). validate @v . getUnvalidated

data ValidatedData v = forall b. ValidatedData { getValidatedData :: VData ('Just b) v }

validateData :: forall v.
  Validate v =>
  UnvalidatedData v ->
  Compose (Context v) ValidatedData v
validateData = Compose . fmap (either ValidatedData ValidatedData) . validate @(ValidData v)

validateFields :: forall b f. (Applicative f, TraversableB b) => b (Compose f ValidatedData) -> f (Either (Partial b InvalidData) (b ValidData))
validateFields = fmap go . bsequence
  where
  go :: b ValidatedData -> Either (Partial b InvalidData) (b ValidData)
  go input = case tryValidate input of
    Just v -> Right v
    Nothing -> Left $ tryInvalidate input

  tryValidate :: b ValidatedData -> Maybe (b ValidData)
  tryValidate = btraverse $ \case
    ValidatedData (Invalid _) -> Nothing
    ValidatedData (Valid v) -> Just $ Valid v

  tryInvalidate :: b ValidatedData -> Partial b InvalidData
  tryInvalidate = (Augmented .) $ bmap $ \case
    ValidatedData (Valid _) -> Compose Nothing
    ValidatedData (Invalid e) -> Compose $ Just $ Invalid e

instance (AllB (ValidateWhere (Equals Identity) Trivial Trivial Trivial) b, TraversableB b, ConstraintsB b) => Validate (b ValidData)
  where
  type Raw (b ValidData) = b UnvalidatedData
  type Error (b ValidData) = Partial b InvalidData
  validate = validateFields . bmapC @(ValidateWhere (Equals Identity) Trivial Trivial Trivial) validateData

-- {{{ Instances

--     {{{ Generic

instance (Validate v, Generic (Raw v)) => Generic (UnvalidatedData v)
  where
  type Rep (UnvalidatedData v) = Rep (Raw v)
  from = from . getUnvalidated
  to = Unvalidated . to

instance (Validate v, Generic (Error v)) => Generic (InvalidData v)
  where
  type Rep (InvalidData v) = Rep (Error v)
  from = from . getInvalid
  to = Invalid . to

instance Generic v => Generic (ValidData v)
  where
  type Rep (ValidData v) = Rep v
  from = from . getValid
  to = Valid . to

--     }}}

--     {{{ Show

instance Show (Raw v) => Show (UnvalidatedData v)
  where
  show = show . getUnvalidated

instance Show (Error v) => Show (InvalidData v)
  where
  show = show . getInvalid

instance Show v => Show (ValidData v)
  where
  show = show . getValid

--     }}}

--     {{{ Aeson

--         {{{ FromJSON

instance (Validate v, FromJSON (Raw v)) => FromJSON (UnvalidatedData v)
  where
  parseJSON = fmap Unvalidated . parseJSON

instance (Validate v, FromJSON (Error v)) => FromJSON (InvalidData v)
  where
  parseJSON = fmap Invalid . parseJSON

instance (Validate v, FromJSON v) => FromJSON (ValidData v)
  where
  parseJSON = fmap Valid . parseJSON

--         }}}

--         {{{ ToJSON

instance (Validate v, ToJSON (Raw v)) => ToJSON (UnvalidatedData v)
  where
  toJSON = toJSON . getUnvalidated

instance (Validate v, ToJSON (Error v)) => ToJSON (InvalidData v)
  where
  toJSON = toJSON . getInvalid

instance (Validate v, ToJSON v) => ToJSON (ValidData v)
  where
  toJSON = toJSON . getValid

--         }}}

--     }}}
-- }}}

-- }}}

data Augmented f b g = Augmented { getAugmented :: b (Compose f g) }
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

class (Validate v, Show (Error v)) => ShowError v
instance (Validate v, Show (Error v)) => ShowError v

instance {-# OVERLAPS #-}
  ( Show1' f
  , Functor f
  , AllB ShowError b
  , Show (b Shown)
  , FunctorB b
  , ConstraintsB b
  ) =>
  Show (Augmented f b InvalidData)
  where
  show = show . bmapC @ShowError (Shown . show . fmap getInvalid . getCompose) . getAugmented
