{-# LANGUAGE FlexibleContexts #-}

module EFA.Application.ReqsAndDofs where


import qualified EFA.Application.Sweep as Sweep

import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified Data.NonEmpty.Class as NonEmptyC

import qualified Data.Traversable as Trav
import Data.Traversable (Traversable, traverse)

import Data.Foldable (Foldable, foldMap)

import qualified Data.Vector.Unboxed as UV
import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.List as List

import Data.Monoid ((<>))
import Control.Applicative (liftA2)



data Pair f g a =
  Pair {
    reqs :: f a,
    dofs :: g a
  }


instance (Functor f, Functor g) => Functor (Pair f g) where
  fmap f (Pair xs ys) = Pair (fmap f xs) (fmap f ys)


instance (Foldable f, Foldable g) => Foldable (Pair f g) where
  foldMap f (Pair xs ys) = foldMap f xs <> foldMap f ys

instance (Traversable f, Traversable g) => Traversable (Pair f g) where
  traverse f (Pair xs ys) = liftA2 Pair (traverse f xs) (traverse f ys)

instance (NonEmptyC.Zip f, NonEmptyC.Zip g) => NonEmptyC.Zip (Pair f g) where
  zipWith f (Pair x0 y0) (Pair x1 y1) =
     Pair (NonEmptyC.zipWith f x0 x1) (NonEmptyC.zipWith f y0 y1)

instance (Show (f a), Show (g a)) => Show (Pair f g a) where
  show (Pair f g) = "Pair (" ++ show f ++ ") (" ++ show g ++ ")"

type Points f g v = Pair f g [v]


newtype Reqs a = Reqs { unReqs :: a } deriving (Show)

newtype Dofs a = Dofs { unDofs :: a } deriving (Show)


instance Functor Reqs where
         fmap f (Reqs x) = Reqs (f x)

instance Functor Dofs where
         fmap f (Dofs x) = Dofs (f x)

reqsPts ::
  Reqs [(TopoIdx.Position node, [Double])] ->
  Reqs [[Double]]
reqsPts = fmap (map snd)

dofsPts ::
  Dofs [(TopoIdx.Position node, [Double])] ->
  Dofs [[Double]]
dofsPts = fmap (map snd)

reqsPos ::
  Reqs [(TopoIdx.Position node, [Double])] ->
  Reqs [TopoIdx.Position node]
reqsPos = fmap (map fst)

dofsPos ::
  Dofs [(TopoIdx.Position node, [Double])] ->
  Dofs [TopoIdx.Position node]
dofsPos = fmap (map fst) 


sweepLength  :: Pair f Dofs [(a, [b])] -> Int
sweepLength = product . map (length . snd) . unDofs . dofs


pairToPts :: Pair Reqs Dofs [(a, b)] -> Pair Reqs Dofs [b]
pairToPts = fmap (map snd)


pairToPos :: Pair Reqs Dofs [(a, b)] -> Pair Reqs Dofs [a]
pairToPos = fmap (map fst)

mkPts ::
  (Traversable f, Traversable g, Ord (f a)) =>
  Points f g a -> Map (f a) [Pair f g a]
mkPts =
  List.foldl'
    (\acc xy -> Map.insertWith' (++) (reqs xy) [xy] acc)
    Map.empty .
  Trav.sequence

innerSweep ::
  (UV.Unbox a, Sweep.SweepClass sweep vec a, Sweep.SweepVector vec a) =>
  [[a]] -> [sweep vec a]
innerSweep xs =
  let ys = sequence xs
      go [] = []
      go [[]] = []
      go zs =
        let (as, bs) = List.foldl' f ([], []) zs
            f (ss, ts) (c:cs) = (c:ss, cs:ts)
            f _ [] = error "EFA.Application.DoubleSweep.innerSweep: empty list"
        in as : go bs
  in map Sweep.fromList $ go ys

mkPts2 ::
  (Ord b, UV.Unbox b,
   Sweep.SweepVector vec b, Sweep.SweepClass sweep vec b) =>
  Pair Reqs Dofs [(a, [b])] ->
  Map [b] (Pair (Sweep.List sweep vec) (Sweep.List sweep vec) b)
mkPts2 pair@(Pair (Reqs as) (Dofs bs)) =
  let is = innerSweep (map snd bs)
      len = sweepLength pair

      os = sequence (map snd as)
      toSw n x = Sweep.fromRational n x
      f o = (o, Pair (Sweep.List $ map (toSw len) o) (Sweep.List is))
  in Map.fromList (map f os)
