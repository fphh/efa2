

module EFA.Equation.Result where

import Control.Applicative (Applicative, pure, (<*>), Alternative, empty, (<|>))
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (mempty)


data Result a = Undetermined
              | Determined a deriving (Show, Eq)


instance Functor Result where
  fmap _ Undetermined = Undetermined
  fmap f (Determined a) = Determined $ f a

instance Applicative Result where
  pure = Determined
  (Determined f) <*> (Determined a) = Determined $ f a
  _ <*> _ = Undetermined

instance Alternative Result where
  empty = Undetermined
  x@(Determined _) <|> _ = x
  _ <|> x = x

instance Foldable Result where
  foldMap f r =
    case r of
      Undetermined -> mempty
      Determined x -> f x

toMaybe :: Result a -> Maybe a
toMaybe Undetermined = Nothing
toMaybe (Determined x) = Just x

fromMaybe :: Maybe a -> Result a
fromMaybe Nothing = Undetermined
fromMaybe (Just x) = Determined x

switch :: b -> (a -> b) -> Result a -> b
switch b _ Undetermined = b
switch _ b (Determined a) = b a

{- |
This is only useful for testing!
In application code you risk non-total functions.
Better use Result.toMaybe or pattern matching.
-}
isDetermined :: Result a -> Bool
isDetermined (Determined _) = True
isDetermined _ = False
