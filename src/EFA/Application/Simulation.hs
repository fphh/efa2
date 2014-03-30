{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Application.Simulation where


import EFA.Application.Utility (quantityTopology)
import qualified EFA.Application.Optimisation.Sweep as Sweep
import EFA.Application.Optimisation.Params (Name(Name))
import qualified EFA.Application.Optimisation.Params as Params

import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Topology.Variable as Variable
import EFA.Flow.Topology.Absolute ( (.=), (=.=) )

import qualified EFA.Flow.Absolute as EqAbs

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Verify as Verify
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Data as Data
import EFA.Signal.Data (Data(Data), Nil,(:>))

import qualified  UniqueLogic.ST.TF.System as ULSystem

import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Map (Map)
import Data.Monoid((<>))

solve ::
  (Ord c, Show c, Arith.ZeroTestable c, Arith.Constant c,
   SV.Storage t c, SV.Storage t Bool, SV.Singleton t, SV.Len (t d),
   Node.C node, SV.Zipper t, SV.Walker t,
   SV.FromList t) =>
  Topo.Topology node ->
  Map (XIdx.Position node) (Name, Name) ->
  Map Name (Params.EtaFunction c c) ->
  Record.Record s s1 typ t1 (XIdx.Position node) t d c ->
  FlowTopo.Section node (EFA.Equation.Result.Result (Data (t :> Nil) c))
solve topology etaAssign etaFunc powerRecord =
   EqSys.solve (quantityTopology topology) $
   givenSimulate etaAssign etaFunc powerRecord

givenSimulate ::
  (Ord c, Show c, Arith.Constant c, Node.C node,
   Verify.GlobalVar mode
     (Data (t :> Nil) c)
     (RecIdx.Record RecIdx.Absolute (Variable.Signal node)),
   SV.Zipper t, SV.Walker t, SV.Storage t c, SV.Len (t d),
   SV.FromList t) =>
  Map (XIdx.Position node) (Name, Name) ->
  Map Name (Params.EtaFunction c c) ->
  Record.Record s1 s2 typ t1 (XIdx.Position node) t d c ->
  EqSys.EquationSystem mode node s (Data (t :> Nil) c)
givenSimulate etaAssign etaFunc (Record.Record t xs) =
   (XIdx.dTime .=
     (Data $ SV.fromList $ replicate (Sig.len t) $ Arith.one))
   <> EqSys.withExpressionGraph (makeEtaFuncGiven etaAssign etaFunc)
   <> Fold.fold (Map.mapWithKey f xs)
   where
     f ppos p  =  XIdx.powerFromPosition ppos .= Sig.unpack p


-- | Generate given equations using efficiency curves or functions for a specified section

makeEtaFuncGiven ::
  (Ord node, Ord d1, Show d1,
   ULSystem.Value mode (Data c d1),
   Arith.Constant d1, Data.ZipWith c, Data.Storage c d1) =>
  Map (XIdx.Position node) (Name, Name) ->
  Map Name (Params.EtaFunction d1 d1) ->
  FlowTopo.Section node (EqAbs.Expression mode vars s (Data c d1)) ->
  EqAbs.VariableSystem mode vars s
makeEtaFuncGiven etaAssign etaFunc topo =
   Fold.fold $
   Map.mapWithKey
      (\se (strP, strN) ->
         Fold.foldMap
            (\(eta, power) ->
               eta
               =.=
               EqAbs.liftF
                  (Data.map (absEtaFunction strP strN etaFunc))
                  power)
            (FlowTopo.lookupAutoDirSection
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerOut flow))
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerIn  flow))
               id se topo))
      etaAssign


makeEtaFuncGiven2 ::
  (Ord node, Ord a, Show a, ULSystem.Value mode (sweep vec a),
   Arith.Sum (sweep vec a), Arith.Constant a,
   Sweep.SweepMap sweep vec a a) =>
  Map (XIdx.Position node) (Name, Name) ->
  Map Name (Params.EtaFunction a a) ->
  FlowTopo.Section node (EqAbs.Expression mode vars s (sweep vec a)) ->
  EqAbs.VariableSystem mode vars s
makeEtaFuncGiven2 etaAssign etaFunc topo =
   Fold.fold $
   Map.mapWithKey
      (\se (strP, strN) ->
         Fold.foldMap
            (\(eta, power) ->
               eta =.= EqAbs.liftF (Sweep.map (absEtaFunction strP strN etaFunc)) power)
            (FlowTopo.lookupAutoDirSection
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerOut flow))
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerIn  flow))
               id se topo))
      etaAssign



absEtaFunction ::
   (Ord a, Show a, Arith.Constant a, Arith.Product b) =>
   Name -> Name -> Map Name (Params.EtaFunction a b) -> a -> b
absEtaFunction strP strN etaFunc =
   let fpos = check strP id $ Map.lookup strP $ Map.map Params.func etaFunc
       fneg = check strN rev $ Map.lookup strN $ Map.map Params.func etaFunc
       rev h = Arith.recip . h . Arith.negate
       check (Name str) =
          maybe (\x -> error ("not defined: '" ++ str ++ "' for " ++ show x))
   in  \x -> if x >= Arith.zero then fpos x else fneg x
