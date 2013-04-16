module EFA.Utility.TypeConstructor (
   Eq, eq,
   Ord, cmp,
   Class.Show, Class.showsPrec,
   Wrap(Wrap, unwrap),
   ) where

import qualified Data.NonEmpty.Class as Class

import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)

import qualified Prelude as P
import Prelude (($))


class Eq f where
   eq :: (P.Eq a) => f a -> f a -> P.Bool

class Eq f => Ord f where
   cmp :: (P.Ord a) => f a -> f a -> P.Ordering


newtype Wrap f a = Wrap {unwrap :: f a}

instance (Eq f, P.Eq a) => P.Eq (Wrap f a) where
   Wrap x == Wrap y  =  eq x y

instance (Ord f, P.Ord a) => P.Ord (Wrap f a) where
   compare (Wrap x) (Wrap y)  =  cmp x y

instance (Class.Show f, P.Show a) => P.Show (Wrap f a) where
   showsPrec p (Wrap x)  =  Class.showsPrec p x

instance P.Functor f => P.Functor (Wrap f) where
   fmap f (Wrap a) = Wrap (P.fmap f a)

instance Foldable f => Foldable (Wrap f) where
   foldMap f (Wrap a) = foldMap f a

instance Traversable f => Traversable (Wrap f) where
   traverse f (Wrap a) = P.fmap Wrap $ traverse f a
