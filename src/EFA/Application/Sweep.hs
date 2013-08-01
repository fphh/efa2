{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module EFA.Application.Sweep where

import qualified EFA.Signal.Vector as SV
import EFA.Signal.Data (Data(..), Nil, (:>))
import EFA.Signal.Typ (Typ, UT)
import EFA.Signal.Signal (TC)
import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Data as Data

import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Graph.StateFlow.Environment as EqEnvState
import EFA.Equation.Result (Result)

import qualified EFA.Application.EtaSys as ES
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology as TD

import Control.Applicative (liftA2)

import qualified Data.List as List
import qualified Data.Map as Map; import Data.Map (Map)

import qualified Data.Vector as V
import Data.Function (on)
import Data.Maybe (mapMaybe)

-- | Map a two dimensional load room (varX, varY) and find per load situation
-- | the optimal solution in the 2d-solution room (two degrees of freevarOptX varOptY)

{-
-- verallgemeinern für (var1, ..., varM) und (varOpt1, ..., varOptN)
doubleSweep :: (SV.Zipper v2,
                SV.Zipper v1,
                SV.Walker v2,
                SV.Walker v1,
                SV.Storage v2 (v1 a),
                SV.Storage v1 a,
                SV.Storage v2 (v1 b),
                SV.Storage v1 b,
                SV.FromList v2,
                SV.Convert v1 v1,
                SV.Storage v2 (v1 (TC (Sig.Arith s s) (Typ UT UT UT) (Data (v2 :> (v1 :> Nil)) b))),
                SV.Storage v1 (TC (Sig.Arith s s) (Typ UT UT UT) (Data (v2 :> (v1 :> Nil)) b)),
               Sig.Arith s s ~ Sig.Signal) =>
               (a -> a -> a -> a -> b) ->
               TC s typ (Data (v2 :> v1 :> Nil) a) ->
               TC s typ (Data (v2 :> v1 :> Nil) a) ->
               TC s typ (Data (v2 :> v1 :> Nil) a) ->
               TC s typ (Data (v2 :> v1 :> Nil) a) ->
               Sig.UTSignal2 v2  v1 (Sig.UTSignal2 v2  v1 b)

doubleSweep fsolve varOptX varOptY varX varY =
  Sig.untype $ Sig.zipWith f varX  varY
  where f x y =
          Sig.untype $ Sig.convert $ Sig.zipWith (fsolve x y) varOptX varOptY

-}

class Sweep a b c where
      sweep :: (a -> b) -> [a] -> c

instance (Sweep a b c) => Sweep a (a -> b) c where
         sweep f (x:xs) = sweep (f x) xs
         sweep _ _ = error "sweep: How many arguments did you supply?"

instance Sweep a b b where
         sweep f [x] = f x
         sweep _ _ = error "sweep: How many arguments did you supply?"

doubleSweep ::
  (Ord a, Sweep a b c) => (a -> b) -> ([[a]], [[a]]) -> Map [a] [c]
doubleSweep f (xs, ys) = List.foldl' h Map.empty $ map g vs
  where vs = sequence (xs ++ ys)
        len = length xs
        g bs = (take len bs, sweep f bs)
        h acc (k, v) = Map.insertWith' (++) k [v] acc

-- verallgemeinern für n states
combineOptimalMaps ::
  Sig.UTSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double
combineOptimalMaps state charge discharge =
  Sig.zipWith f state $ Sig.zip charge discharge
  where f s (c, d) = if s < 0.1 then c else d

optimalSolution2D ::
  (Fractional v, Ord node, Show node, Show v, Ord v, Eq a) =>
  ES.Condition node a v ->
  ES.Forcing node a v ->
  Flow.RangeGraph node ->
  Sig.UTSignal2 V.Vector V.Vector (EqEnv.Complete node a (Result v)) ->
  Maybe (v, EqEnv.Complete node a (Result v))
optimalSolution2D cond forcing topo sigEnvs = liftA2 (,) etaMax env
  where etaSys = Sig.map (ES.objectiveFunction cond forcing topo) sigEnvs
        etaMax = Sig.fromScalar $ Sig.maximum etaSys
        (xIdx, yIdx) = Sig.findIndex2 (== etaMax) etaSys
        env = liftA2 (Sig.getSample2D sigEnvs) xIdx yIdx

optimalSolution2DState ::
  (Fractional v, Ord node, Show node, Show v, Ord v, Eq a) =>
  ES.ConditionState node a v ->
  ES.ForcingState node a v ->
  TD.StateFlowGraph node ->
  Sig.UTSignal2 V.Vector V.Vector (EqEnvState.Complete node a (Result v)) ->
  Maybe (v, EqEnvState.Complete node a (Result v))
optimalSolution2DState cond forcing topo sigEnvs = liftA2 (,) etaMax env
  where etaSys = Sig.map (ES.objectiveFunctionState cond forcing topo) sigEnvs
        etaMax = Sig.fromScalar $ Sig.maximum etaSys
        (xIdx, yIdx) = Sig.findIndex2 (== etaMax) etaSys
        env = liftA2 (Sig.getSample2D sigEnvs) xIdx yIdx


optimalSolutionState ::
  (Ord v, Fractional v, Ord node, Show node, Show v) =>
  ES.ConditionState node a v ->
  ES.ForcingState node a v ->
  TD.StateFlowGraph node ->
  [EqEnvState.Complete node a (Result v)] ->
  (v, EqEnvState.Complete node a (Result v))
optimalSolutionState cond forcing topo envs =
  List.maximumBy (compare `on` fst) es
  where es = mapMaybe f envs
        f e = fmap (,e) $ ES.objectiveFunctionState cond forcing topo e