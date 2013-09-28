{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module EFA.Application.Sweep where

import qualified EFA.Application.EtaSys as ES

import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Data as Data

import qualified EFA.Equation.Environment as EqEnv
import EFA.Equation.Result (Result)

import qualified EFA.Graph.StateFlow.Environment as StateEnv

import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology as Topo

import Control.Applicative (liftA2)

import qualified Data.Traversable as Trav
import qualified Data.List as List
import qualified Data.Map as Map; import Data.Map (Map)

import qualified Data.Vector as V
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))

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

type Points f g v = Pair f g [v]


doubleSweep ::
  (Traversable f, Traversable g, Ord (f a)) =>
  (Pair f g a -> b) -> Points f g a -> Map (f a) [b]
doubleSweep f =
  Map.fromListWith (++) . map (\xy -> (fstRecord xy, [f xy])) . Trav.sequence

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
  Topo.StateFlowGraph node ->
  Sig.UTSignal2 V.Vector V.Vector (StateEnv.Complete node a (Result v)) ->
  Maybe (v, StateEnv.Complete node a (Result v))
optimalSolution2DState cond forcing topo sigEnvs = liftA2 (,) etaMax env
  where etaSys = Sig.map (ES.objectiveFunctionState cond forcing topo) sigEnvs
        etaMax = Sig.fromScalar $ Sig.maximum etaSys
        (xIdx, yIdx) = Sig.findIndex2 (== etaMax) etaSys
        env = liftA2 (Sig.getSample2D sigEnvs) xIdx yIdx


optimalSolutionState ::
  (Ord v, Fractional v, Ord node, Show node, Show v) =>
  ES.ConditionState node a v ->
  ES.ForcingState node a v ->
  Topo.StateFlowGraph node ->
  [StateEnv.Complete node a (Result v)] ->
  Maybe (v, StateEnv.Complete node a (Result v))
optimalSolutionState cond forcing topo envs =
  case mapMaybe f envs of
       [] -> Nothing
       fs -> Just $ List.maximumBy (compare `on` fst) fs
  where f e = fmap (flip (,) e) $ ES.objectiveFunctionState cond forcing topo e
