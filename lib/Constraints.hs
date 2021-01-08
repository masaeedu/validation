module Constraints where

import Data.Kind (Constraint)

class Trivial x
instance Trivial x

type Show1' f = (forall x. Show x => Show (f x)) :: Constraint

class (x ~ y) => Equals x y
instance Equals x x
