{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module EFA.Application.ReqsAndDofs where


import qualified EFA.Application.Sweep as Sweep

import qualified Data.Traversable as Trav
import Data.Traversable (Traversable, traverse)

import Data.Foldable (Foldable, foldMap)

import qualified Data.Vector.Unboxed as UV(Unbox)
import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.List as List

import Data.Monoid ((<>))
import Control.Applicative (liftA, liftA2)



newtype ReqsT f a = Reqs { unReqs :: f a } deriving (Show, Eq, Ord)

type Reqs = ReqsT []

instance Functor f => Functor (ReqsT f) where
         fmap f (Reqs xs) = Reqs (fmap f xs)

instance Foldable f => Foldable (ReqsT f) where
         foldMap f (Reqs xs) = foldMap f xs

instance Traversable f => Traversable (ReqsT f) where
  traverse f (Reqs xs) = liftA Reqs (traverse f xs)


newtype DofsT g a = Dofs { unDofs :: g a } deriving (Show)

type Dofs = DofsT []

instance Functor f => Functor (DofsT f) where
         fmap f (Dofs x) = Dofs (fmap f x)

instance Foldable f => Foldable (DofsT f) where
         foldMap f (Dofs xs) = foldMap f xs

instance Traversable f => Traversable (DofsT f) where
  traverse f (Dofs xs) = liftA Dofs (traverse f xs)


data Pair f g a =
  Pair {
    reqs :: f a,
    dofs :: g a
  } deriving (Show)

instance (Functor f, Functor g) => Functor (Pair f g) where
  fmap f (Pair xs ys) = Pair (fmap f xs) (fmap f ys)

instance (Foldable f, Foldable g) => Foldable (Pair f g) where
  foldMap f (Pair xs ys) = foldMap f xs <> foldMap f ys

instance (Traversable f, Traversable g) => Traversable (Pair f g) where
  traverse f (Pair xs ys) = liftA2 Pair (traverse f xs) (traverse f ys)

reqsPts :: Reqs (pos, a) -> Reqs a
reqsPts = fmap snd

dofsPts :: Dofs (pos, a) -> Dofs a
dofsPts = fmap snd

reqsPos :: Reqs (pos, a) -> Reqs pos
reqsPos = fmap fst

dofsPos :: Dofs (pos, a) -> Dofs pos
dofsPos = fmap fst

sweepLength  :: Pair f Dofs (a, [b]) -> Int
sweepLength = product . map (length . snd) . unDofs . dofs

pairToPts :: Pair Reqs Dofs (a, b) -> Pair Reqs Dofs b
pairToPts = fmap snd

pairToPos :: Pair Reqs Dofs (a, b) -> Pair Reqs Dofs a
pairToPos = fmap fst


mkPts ::
  (Traversable f, Traversable g, Ord (f a)) =>
  Pair f g [a] -> Map (f a) [Pair f g a]
mkPts =
  List.foldl'
    (\acc xy -> Map.insertWith' (++) (reqs xy) [xy] acc)
    Map.empty .
  Trav.sequence


mkPts2 ::
  (Ord b, UV.Unbox b,
   Sweep.SweepVector vec b,
   Sweep.SweepClass sweep vec b) =>
  Pair Reqs Dofs (a, [b]) ->
  Map [b] (Pair (Sweep.List sweep vec) (Sweep.List sweep vec) b)
mkPts2 pair@(Pair rs ds) = res
  where Reqs rs' = reqsPts rs
        os = sequence rs'
        Dofs ds' = dofsPts ds
        is = List.transpose $ sequence ds'

        len = sweepLength pair

        res = List.foldl' insert Map.empty os
        insert acc o =
          let v = Pair (Sweep.List (map (Sweep.fromRational len) o))
                       (Sweep.List (map Sweep.fromList is))
          in Map.insert o v acc

