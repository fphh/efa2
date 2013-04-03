

module EFA.Equation.Result where

import Control.Applicative (Applicative, pure, (<*>))
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mempty, Monoid(..))


data Result a = Undetermined
              | Determined a deriving (Show)


instance Functor Result where
  fmap _ Undetermined = Undetermined
  fmap f (Determined a) = Determined $ f a

instance Applicative Result where
  pure = Determined
  (Determined f) <*> (Determined a) = Determined $ f a
  _ <*> _ = Undetermined

instance Foldable Result where
  foldMap f r =
    case r of
      Undetermined -> mempty
      Determined x -> f x

toMaybe :: Result a -> Maybe a
toMaybe Undetermined = Nothing
toMaybe (Determined x) = Just x
