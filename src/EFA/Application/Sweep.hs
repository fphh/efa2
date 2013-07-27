{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Application.Sweep where

import qualified EFA.Signal.Vector as SV
import EFA.Signal.Data (Data(..), Nil, (:>))
import EFA.Signal.Typ (Typ, UT)
import EFA.Signal.Signal (TC)
import qualified EFA.Signal.Signal as Sig

import qualified EFA.Equation.Environment as EqEnv
import EFA.Equation.Result (Result)

import qualified EFA.Application.EtaSys as ES
import qualified EFA.Graph.Flow as Flow

import Control.Applicative (liftA2)


import qualified Data.Vector as V

-- | Map a two dimensional load room (varX, varY) and find per load situation
-- | the optimal solution in the 2d-solution room (two degrees of freevarOptX varOptY)

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

