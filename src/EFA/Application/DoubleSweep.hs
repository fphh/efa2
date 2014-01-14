{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Application.DoubleSweep where

import qualified EFA.Application.Sweep as Sweep

import qualified EFA.Signal.Signal as Sig

import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.SystemEta as SeqEta

import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.State.SystemEta as StateEta

import qualified EFA.Flow.Topology.Variable as TopoVar

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Graph.Topology.Node as Node

import Control.Applicative (liftA2)

import qualified Data.Traversable as Trav
import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.List as List
import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>), Monoid)
import Data.Ord (comparing)
import qualified Control.Monad.Trans.Writer as MW

import Control.Applicative (liftA3)

-- | Map a two dimensional load room (varX, varY) and find per load situation
-- | the optimal solution in the 2d-solution room (two degrees of freevarOptX varOptY)

data Pair f g a =
  Pair {
    fstRecord :: f a,
    sndRecord :: g a
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

mkPts ::
  (Traversable f, Traversable g, Ord (f a)) =>
  Points f g a -> Map (f a) [Pair f g a]
mkPts =
  List.foldl'
    (\acc xy -> Map.insertWith' (++) (fstRecord xy) [xy] acc)
    Map.empty .
  Trav.sequence


innerSweep ::
  (Sweep.Size x, Sweep.DoubleList x, UV.Unbox a,
   Sweep.SweepClass sweep vec a, Sweep.SweepVector vec a) =>
  x [a] -> [sweep vec a]
innerSweep xs =
  let ys = sequence $ Sweep.doubleList xs
      go [] = []
      go zs =
        let (as, bs) = List.foldl' f ([], []) zs
            f (ss, ts) (c:cs) = (c:ss, cs:ts)
            f _ [] = error "EFA.Application.DoubleSweep.innerSweep: empty list"
        in as : go bs
  in map Sweep.fromList $ go ys


mkPts2 ::
  (Ord a, UV.Unbox a, Sweep.Size y,
   Sweep.DoubleList x, Sweep.DoubleList y,
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a) =>
  Pair x y [a] -> Map [a] (Pair (Sweep.List sweep vec) (Sweep.List sweep vec) a)
mkPts2 (Pair as bs) =
  let is = innerSweep bs
      len = Sweep.length (head is)
      os = sequence $ Sweep.doubleList as
      toSw n x = Sweep.fromRational n x
      f o = (o, Pair (Sweep.List $ map (toSw len) o) (Sweep.List is))
  in Map.fromList (map f os)

doubleSweep ::
  (Pair g h a -> b) ->
  Map (f a) (Pair g h a) ->
  Map (f a) b
doubleSweep = Map.map



-- verallgemeinern für n states
combineOptimalMaps ::
  Sig.UTSignal2 V.Vector V.Vector Sig.ArgMax ->
  Sig.PSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double
combineOptimalMaps state charge discharge =
  Sig.zipWith
     (\s (c, d) -> case s of Sig.ArgMax0 -> c; Sig.ArgMax1 -> d) state $
  Sig.zip charge discharge


optimalSolution2D ::
  (Node.C node, Eq a, Ord v, Arith.Constant v) =>
  SeqEta.Condition node a v ->
  SeqEta.Forcing node a v ->
  Sig.UTSignal2 V.Vector V.Vector (SeqFlow.Graph node a (Result v)) ->
  Maybe ((v, v), SeqFlow.Graph node a (Result v))
optimalSolution2D cond forcing =
  optimalSolutionGeneric (SeqEta.objectiveFunction cond forcing) .
  concat . Sig.toList


optimalSolutionState ::
  (Node.C node, Ord v, Arith.Constant v) =>
  StateEta.Condition node a v ->
  StateEta.Forcing node a v ->
  [StateFlow.Graph node a (Result v)] ->
  Maybe ((v, v), StateFlow.Graph node a (Result v))
optimalSolutionState cond forcing =
  optimalSolutionGeneric (StateEta.objectiveFunction cond forcing)


optimalSolutionGeneric ::
  Ord a =>
  (b -> Maybe a) -> [b] -> Maybe (a, b)
optimalSolutionGeneric f =
  fmap (NonEmpty.maximumBy (comparing fst)) . NonEmpty.fetch .
  mapMaybe (\x -> fmap (flip (,) x) $ f x)


findBestIndex ::
  (UV.Unbox a, Ord a, Arith.Constant a, Show a,
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Sweep.SweepVector vec Bool,
   Sweep.SweepClass sweep vec Bool) =>
  (sweep (vec :: * -> *) Bool) ->
  (sweep vec a) ->
  (sweep vec a) ->
  Maybe (Int, a, a)
findBestIndex cond esys force =
  if (or c) then Just res else Nothing
  where c = Sweep.toList cond
        start = (0, Arith.zero, Arith.zero)
        res = List.foldl' f start 
                    (List.zip4 [0..] (Sweep.toList cond) 
                               (Sweep.toList esys) (Sweep.toList force))
        f acc@(_, fo, _) (i, b, es2, f2) =
          if b && f2 > fo
             then (i, f2, es2)
             else acc


optimalSolutionState2 ::
  (UV.Unbox a, Ord a, Node.C node, Arith.Constant a, Show a,
   Arith.Product (sweep vec a),
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Monoid (sweep vec Bool),
   Sweep.SweepVector vec Bool,
   Sweep.SweepClass sweep vec Bool,
   Sweep.SweepMap sweep vec a Bool) =>
  (StateFlow.Graph node (Result (sweep vec a)) (Result (sweep vec a)) ->
    Result (sweep vec a)) ->
  StateFlow.Graph node (Result (sweep vec a)) (Result (sweep vec a)) ->
  Maybe (a, a, StateFlow.Graph node (Result a) (Result a))
optimalSolutionState2 forcing env =
  let condVec = checkGreaterZero env
      esys = StateEta.etaSys env
      force = forcing env
      bestIdx = liftA3 findBestIndex condVec esys (liftA2 (Arith.~+) force esys)
  in case bestIdx of
          Determined (Just (n, x, y)) ->
            let choose = fmap (Sweep.!!! n)
                env2 = StateFlow.mapGraph choose choose env
            in Just (x, y, env2)
          _ -> Nothing

foldMap2 ::
  (Monoid c, Node.C node) =>
  (a -> c) -> (v -> c) -> StateFlow.Graph node a v -> c
foldMap2 fa fv = fold . StateFlow.mapGraph fa fv

fold ::
   (Node.C node, Monoid w) =>
   StateFlow.Graph node w w -> w
fold = MW.execWriter . StateFlow.traverseGraph MW.tell MW.tell


checkGreaterZero ::
  (Arith.Constant a, Ord a,
   Ord node,
   Monoid (sweep vec Bool), Node.C node,
   Sweep.SweepClass sweep vec a,
   Sweep.SweepClass sweep vec Bool,
   Sweep.SweepMap sweep vec a Bool) =>
  StateFlow.Graph node b (Result (sweep vec a)) ->
  Result (sweep vec Bool)
checkGreaterZero = fold . StateFlow.mapGraphWithVar
  (\_ _ -> Undetermined)
  (\(Idx.InPart _ var) v ->
     case var of
          TopoVar.Power _ ->
            case v of
                 (Determined w) -> Determined $ Sweep.map (> Arith.zero) w
                 _ -> Undetermined
          _ -> Undetermined)
