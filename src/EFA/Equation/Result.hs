

module EFA.Equation.Result where

import Control.Applicative (Applicative, pure, (<*>))

data Result a = Undetermined
              | Determined a deriving (Show)


instance Functor Result where
  fmap _ Undetermined = Undetermined
  fmap f (Determined a) = Determined $ f a

instance Applicative Result where
  pure = Determined
  (Determined f) <*> (Determined a) = Determined $ f a
  _ <*> _ = Undetermined

