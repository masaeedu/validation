{-# LANGUAGE DeriveGeneric #-}
module Barbies.Augmented where

import Barbies (FunctorB(..), TraversableB(..), ConstraintsB(..), bmapC)
import Data.Functor.Compose (Compose(..))
import GHC.Generics (Generic(..))
import Constraints (Show1')

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
