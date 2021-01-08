{-# LANGUAGE DeriveGeneric, DeriveAnyClass, ImpredicativeTypes #-}
module Validation where

import Barbies (FunctorB(..), TraversableB(..), ConstraintsB(..), bsequence, bmapC)
import Data.Kind (Type, Constraint)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Compose (Compose(..))
import Data.Bifunctor (Bifunctor(..))
import GHC.Generics (Generic(..))
import Data.Aeson (FromJSON(..), ToJSON(..))

class Functor (Context v) => Validate v
  where
  type Context v :: Type -> Type
  type Context v = Identity
  type Raw v :: Type
  type Error v :: Type
  validate :: Raw v -> Context v (Either (Error v) v)

-- {{{ VData

data VData (validationState :: Maybe Bool) v
  where
  Unvalidated :: Validate v => { getUnvalidated :: Raw v   } -> UnvalidatedData v
  Invalid     :: Validate v => { getInvalid     :: Error v } -> InvalidData     v
  Valid       ::               { getValid       :: v       } -> ValidData       v

type UnvalidatedData = VData 'Nothing
type InvalidData = VData ('Just 'False)
type ValidData = VData ('Just 'True)

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

class (Validate v, Context v ~ f) => ValidateInContext f v
instance (Validate v, Context v ~ f) => ValidateInContext f v

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

-- instance Show1 (InvalidData)
--
-- instance Show1 (VData ('Nothing))

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

type Show1' f = (forall x. Show x => Show (f x)) :: Constraint

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


instance (AllB (ValidateInContext Identity) b, TraversableB b, ConstraintsB b) => Validate (b ValidData)
  where
  type Raw (b ValidData) = b UnvalidatedData
  type Error (b ValidData) = Partial b InvalidData
  validate = validateFields . bmapC @(ValidateInContext Identity) validateData
