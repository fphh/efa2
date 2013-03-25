-- inspired by total-map
module EFA.Utility.TotalMap where

import Control.Applicative (Applicative, pure, (<*>))

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid ((<>))


data TotalMap k a = TotalMap {deflt :: a, core :: Map k a}

cons :: a -> Map k a -> TotalMap k a
cons = TotalMap


instance Functor (TotalMap k) where
   fmap f (TotalMap d m) = TotalMap (f d) (fmap f m)

instance (Ord k) => Applicative (TotalMap k) where
   pure a = TotalMap a Map.empty
   TotalMap fd fm <*> TotalMap ad am =
      TotalMap (fd ad) $
         fmap ($ad) (Map.difference fm am) <>
         fmap (fd$) (Map.difference am fm) <>
         Map.intersectionWith ($) fm am
