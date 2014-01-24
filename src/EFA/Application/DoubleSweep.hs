{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Application.DoubleSweep where

import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.ReqsAndDofs as ReqsAndDofs

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

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.List as List
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid)
import Data.Ord (comparing)
import qualified Control.Monad.Trans.Writer as MW

import Control.Applicative (liftA3)

-- | Map a two dimensional load room (varX, varY) and find per load situation
-- | the optimal solution in the 2d-solution room (two degrees of freevarOptX varOptY)

doubleSweep ::
  (ReqsAndDofs.Pair g h a -> b) ->
  Map (f a) (ReqsAndDofs.Pair g h a) ->
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



expectedValue ::
  (Fractional a, Ord a, Monoid (sweep vec Bool), UV.Unbox a,
   Node.C node, Arith.Product (sweep vec a), Arith.Constant a,
   Sweep.SweepVector vec a, Sweep.SweepVector vec Bool,
   Sweep.SweepMap sweep vec a Bool, Sweep.SweepClass sweep vec a,
   Sweep.SweepClass sweep vec Bool) =>
  StateFlow.Graph node (Result (sweep vec a)) (Result (sweep vec a)) -> Maybe a
expectedValue env =
  case (checkGreaterZero env, StateEta.etaSys env) of
       (Determined condVec, Determined esys) ->
         let condLst = Sweep.toList condVec
             esysLst = Sweep.toList esys
             (s, n) = List.foldl' f (0, 0) $ zip condLst esysLst
             f (t, m) (c, e) = if c then (t+e, m+1) else (s, n)
         in if n /= 0 then Just (s/n) else Nothing
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


