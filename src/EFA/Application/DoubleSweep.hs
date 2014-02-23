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
import qualified EFA.Application.Type as Type

import qualified EFA.Signal.Signal as Sig

--import qualified EFA.Flow.Sequence.Quantity as SeqFlow
--import qualified EFA.Flow.Sequence.SystemEta as SeqEta

import qualified EFA.Flow.State.Quantity as StateFlow
--import qualified EFA.Flow.State.SystemEta as StateEta

import qualified EFA.Flow.Topology.Variable as TopoVar

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.Graph.Topology.Node as Node

import Control.Applicative (liftA2)

import qualified Data.Map as Map; import Data.Map (Map)
--import qualified Data.NonEmpty as NonEmpty
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

--import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid)
--import Data.Ord (comparing)
import qualified Control.Monad.Trans.Writer as MW

import Control.Applicative (liftA3)

import Data.Tuple.HT (snd3)
import Data.Ord (comparing)

import Debug.Trace (trace)


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

{-
optimalSolution2D ::
  (Node.C node, Eq a, Ord v, Arith.Constant v) =>
  SeqEta.Condition node a v ->
  SeqEta.Forcing node a v ->
  Sig.UTSignal2 V.Vector V.Vector (SeqFlow.Graph node a (Result v)) ->
  Maybe ((v, v), SeqFlow.Graph node a (Result v))
optimalSolution2D cond forcing =
  optimalSolutionGeneric (SeqEta.objectiveFunction cond forcing) .
  concat . Sig.toList
-}
{-
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
-}
{-
-- NEW PG
findOptimalState ::  Map State.Idx StateForcing ->
                     Map State.Idx (Maybe (a, a, StateFlow.Graph node (Result a) (Result a)))
findOptimalState stateForcing stateMap = if Map.keys stateForcing == map.keys stateMap
                                         then optimalSolutionGeneric stateObjectives
                                         else error ("Error in findOptimalState - StateMap and StateForcings
                                                     have different State Keys: " ++ show stateForcing  ++ "    "
                                              ++ show stateMap)
  where stateObjectives = zipWith (~+) (Map.elem stateForcing) (fst & Map.elem stateMap)
-}

findBestIndex ::
  (Ord a, Arith.Constant a, UV.Unbox a,RealFloat a,
   Sweep.SweepVector UV.Vector a,
   Sweep.SweepClass sweep UV.Vector a,
   Sweep.SweepVector UV.Vector Bool,
   Sweep.SweepClass sweep UV.Vector Bool) =>
  (sweep UV.Vector Bool) ->
  (sweep UV.Vector a) ->
  (sweep UV.Vector a) ->
  Maybe (Int, a, a)

findBestIndex cond esys objVal = 
  if UV.null fv
     then Nothing
     else Just (idx, o, e)
  where
        c1 = Sweep.fromSweep cond
        e1 = Sweep.fromSweep esys
        objf1 = Sweep.fromSweep objVal
        
{-        comparingWithNaN p x y = case (isNaN $ p x,isNaN $ p y) of 
          (True,True) -> EQ
          (False,True) -> GT     
          (True,False) -> LT
          (False,False) -> comparing p x y-}

        fv = UV.filter (\(_,c,o,_) -> c && (not $ isNaN o)) $ UV.zipWith4 (,,,) (UV.fromList [0.. ((UV.length c1)-1)]) c1 objf1 e1
        maxIdx = UV.maxIndexBy (comparing (\ (_,_,o,_) -> o)) fv
        (idx,_, o, e) = fv UV.! maxIdx
{-

        -- alte Version erweitert um NaN - Fix:
  where                              

        c1 = Sweep.fromSweep cond
        e1 = Sweep.fromSweep esys
        objf1 = Sweep.fromSweep force


        start = (0, 0, Arith.zero, Arith.zero)

        -- altes Ergebnis:
        res = case UV.foldl' f start (UV.zipWith3 (,,) c1 e1 objf1) of
                   (x, _, ch, d) -> Just (x, ch, d)

        f (bestIdx, cnt, fo, eo) (b, es2, f2) =
          if b && g f2 fo
             then (cnt, cnt+1, f2, es2)
             else (bestIdx, cnt+1, fo, eo)
                  
        g x y =  case (isNaN $ x,isNaN $ y) of 
          (True,True) -> True
          (False,True) -> False     
          (True,False) -> True         
          (False,False) ->  x >= y
-}
{-
        -- alte Version:

        c1 = Sweep.fromSweep cond
        e1 = Sweep.fromSweep esys
        objf1 = Sweep.fromSweep force


        start = (0, 0, Arith.zero, Arith.zero)

        -- altes Ergebnis:
        res = case UV.foldl' f start (UV.zipWith3 (,,) c1 e1 objf1) of
                   (x, _, ch, d) -> Just (x, ch, d)

        f (bestIdx, cnt, fo, eo) (b, es2, f2) =
          if b && f2 >= fo
             then (cnt, cnt+1, f2, es2)
             else (bestIdx, cnt+1, fo, eo)
-}


optimalSolutionState2 ::
  (Ord a, Node.C node, Arith.Constant a, UV.Unbox a,RealFloat a,
   Arith.Product (sweep UV.Vector a),
   Sweep.SweepVector UV.Vector a,
   Sweep.SweepClass sweep UV.Vector a,
   Monoid (sweep UV.Vector Bool),
   Sweep.SweepVector UV.Vector Bool,
   Sweep.SweepClass sweep UV.Vector Bool,
   Sweep.SweepMap sweep UV.Vector a Bool) =>
  ( Map Idx.State (Map node (Maybe (sweep UV.Vector a))) ->
      Result (sweep UV.Vector a) ) ->
  Type.SweepPerReq node sweep UV.Vector a ->
  Maybe (a, a, StateFlow.Graph node (Result a) (Result a))

optimalSolutionState2 forcing (Type.SweepPerReq esys condVec powerMap env) =
  let force = forcing powerMap
      bestIdx = liftA3 findBestIndex condVec esys (liftA2 (Arith.~+) force esys)
  in case bestIdx of
          Determined (Just (n, x, y)) ->
            let choose = fmap (Sweep.!!! n)
                env2 = StateFlow.mapGraph choose choose env
            in Just (x, y, env2)
          _ -> Nothing



expectedValue ::
  (Arith.Constant a, Arith.Sum a, UV.Unbox a,
   Sweep.SweepClass sweep UV.Vector Bool,
   Sweep.SweepClass sweep UV.Vector a) =>
  Type.SweepPerReq node sweep UV.Vector a -> Maybe a
expectedValue (Type.SweepPerReq (Determined esys) (Determined condVec) _ _) =
  Just (s Arith.~/ n)
  where c = Sweep.fromSweep condVec
        e = Sweep.fromSweep esys
        (s, n) = UV.foldl' f (Arith.zero, Arith.zero) (UV.zip c e)
        f acc@(t, cnt) (x, y) =
          if x then (t Arith.~+ y, cnt Arith.~+ Arith.one) else acc
expectedValue _ = Nothing

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


