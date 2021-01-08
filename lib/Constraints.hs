module Constraints where

import Data.Kind (Constraint)
import Data.Coerce (Coercible)

class Trivial x
instance Trivial x

type Show1' f = (forall x. Show x => Show (f x)) :: Constraint

class (x ~ y) => Equals x y
instance Equals x x

type CoercibleF f = ((forall x y. Coercible x y => Coercible (f x) (f y)) :: Constraint)
