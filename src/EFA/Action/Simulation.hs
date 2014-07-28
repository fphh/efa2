{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Simulation where

import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.Vector as DV
--import qualified EFA.Value.Type as Type
import EFA.Action.Utility (quantityTopology)
-- import qualified EFA.Application.Optimisation.Sweep as Sweep
-- import EFA.Application.Optimisation.Params (Name(Name))
-- import qualified EFA.Application.Optimisation.Params as Params
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.Topology.Record as TopoRecord
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
import qualified EFA.Flow.Topology as FlowTopoPlain
import qualified EFA.Action.Utility as ActUt
--import qualified EFA.Signal.Vector as SV
--import qualified EFA.Signal.Signal as Sig
--import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Data as Data
--import EFA.Signal.Data (Data(Data), Nil,(:>))


import qualified EFA.Action.EtaFunctions as EtaFunctions

import qualified EFA.Data.OD.Signal.Flow as SignalFlow

import qualified  UniqueLogic.ST.TF.System as ULSystem

import qualified Data.Map as Map
import qualified Data.Foldable as Fold
--import Data.Map (Map)
import Data.Monoid((<>))

import EFA.Utility(Caller,
                   -- merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Simulation"

nc :: FunctionName -> Caller
nc = genCaller modul


data Simulation node inst sigVec a = 
  Simulation 
  {accessFlowResult:: FlowTopo.Section node (EFA.Equation.Result.Result (SignalFlow.Data inst sigVec (Interp.Val a))),
   accessPowerRecord :: SignalFlow.HRecord (XIdx.Position node) inst String sigVec a (Interp.Val a)}


simulation ::
  (Arith.ZeroTestable (SignalFlow.Data inst sigVec (Interp.Val a)),
 Arith.Sum (SignalFlow.Data inst sigVec a),
 Arith.Product (SignalFlow.Data inst sigVec (Interp.Val a)),
 Arith.Constant a,
 Node.C node,
 DV.Walker sigVec,
 DV.Storage sigVec a,
 DV.Storage sigVec (Interp.Val a),DV.Storage sigVec (SignalFlow.TimeStep a),
 DV.Length sigVec,
 DV.FromList sigVec) =>
 Caller ->
 Topo.Topology node ->
 EtaFunctions.FunctionMap node (Interp.Val a) ->
 SignalFlow.HRecord (XIdx.Position node) inst String sigVec a (Interp.Val a) ->
 Simulation node inst sigVec a
simulation caller topology etaFunctions given = Simulation flow powerRecord
  where 
    time = SignalFlow.getHTime given
    flow = solve caller topology etaFunctions given
    powerRecord = envToPowerRecord time flow
  


solve ::
  (Node.C node, 
   DV.Storage sigVec (Interp.Val a), 
   DV.FromList sigVec,
   DV.Length sigVec,
   Arith.Constant a,
   DV.Walker sigVec,DV.Storage sigVec (SignalFlow.TimeStep a),
   Arith.ZeroTestable (SignalFlow.Data inst sigVec (Interp.Val a)),
   Arith.Sum (SignalFlow.Data inst sigVec a), 
   Ord node, DV.Storage sigVec a,
   Arith.Product (SignalFlow.Data inst sigVec (Interp.Val a))) =>
  Caller ->
  Topo.Topology node ->
  EtaFunctions.FunctionMap node (Interp.Val a) ->
  SignalFlow.HRecord (XIdx.Position node) inst String sigVec a (Interp.Val a) ->
  FlowTopo.Section node (EFA.Equation.Result.Result (SignalFlow.Data inst sigVec (Interp.Val a)))
solve caller topology etaFunctions powerRecord =
   EqSys.solve (quantityTopology topology) $
   givenSimulate (caller |> nc "solve") etaFunctions powerRecord

givenSimulate ::
  (ULSystem.Value mode (SignalFlow.Data inst sigVec (Interp.Val a)),
   ULSystem.Value  mode (SignalFlow.Data inst sigVec a),
   DV.Length sigVec,
   Arith.Constant a,
   DV.Storage sigVec a,
   DV.Walker sigVec,
   Verify.GlobalVar mode (SignalFlow.Data inst sigVec (Interp.Val a))
                        (RecIdx.Record RecIdx.Absolute (Variable.Signal node)),Ord node,
   Arith.Sum (SignalFlow.Data inst sigVec a),
   Arith.Sum (SignalFlow.Data inst sigVec (Interp.Val a)),
   Node.C node, DV.Storage sigVec (SignalFlow.TimeStep a),
   DV.Storage sigVec (Interp.Val a), 
   DV.FromList sigVec) =>
  Caller ->
  EtaFunctions.FunctionMap node (Interp.Val a) ->
  SignalFlow.HRecord (XIdx.Position node) inst String sigVec a (Interp.Val a) ->
  EqSys.EquationSystem mode node s (SignalFlow.Data inst sigVec (Interp.Val a))
givenSimulate caller etaFunctions (SignalFlow.HRecord t xs) =
   (XIdx.dTime .=
     (SignalFlow.fromListData (caller |> nc "givenSimulate") $ replicate (Strict.len t) $ Arith.one))
   <> EqSys.withExpressionGraph (makeEtaFuncGiven etaFunctions)
   <> Fold.fold (Map.mapWithKey f xs)
   where
     f ppos p  =  XIdx.powerFromPosition ppos .= p


makeEtaFuncGiven ::
  (Arith.Sum (SignalFlow.Data inst sigVec (Interp.Val a)), 
   ULSystem.Value mode (SignalFlow.Data inst sigVec (Interp.Val a)), 
   ULSystem.Value mode (SignalFlow.Data inst sigVec a), 
   DV.Storage sigVec (Interp.Val a),
   DV.Walker sigVec,
   Ord node,
   Arith.Sum (SignalFlow.Data inst sigVec a)) => 
  EtaFunctions.FunctionMap node (Interp.Val a) ->
  FlowTopo.Section node (EqAbs.Expression mode vars s (SignalFlow.Data inst sigVec (Interp.Val a))) ->
  EqAbs.VariableSystem mode vars s
makeEtaFuncGiven (EtaFunctions.FunctionMap etaFunctions) topo =
   Fold.fold $
   Map.mapWithKey
      (\position etaFunc ->
         Fold.foldMap
            (\(eta, power) ->
               eta
               =.=
               EqAbs.liftF
                  (SignalFlow.mapData etaFunc)
                  power)
            (FlowTopo.lookupAutoDirSection
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerOut flow))
               (\flow -> (FlowTopo.flowEta flow, FlowTopo.flowPowerIn  flow))
               id position topo))
      etaFunctions

-- TODO :: Move to better place ?
envToPowerRecord ::
  (DV.Walker vec,Arith.Constant a,
   DV.Storage vec (Interp.Val a),
   DV.Storage vec a,
   Ord node) =>
  Strict.Axis inst String vec (SignalFlow.TimeStep a) -> 
  TopoQty.Section node (Result (SignalFlow.Data inst vec (Interp.Val a))) ->
  SignalFlow.HRecord (XIdx.Position node) inst String vec a (Interp.Val a)
envToPowerRecord time =
  sectionToPowerRecord time
  . TopoQty.mapSection (ActUt.checkDetermined "envToPowerRecord")

-- TODO :: Move to better place ?
-- TODO :: Time has same format as Vectors hence has to be stored as signal Vector
-- TODO :: Old Time axis given -- is that the best way ? 
sectionToPowerRecord ::
  (DV.Walker vec,
   DV.Storage vec a,
   Arith.Constant a,
   DV.Storage vec (Interp.Val a)) =>
   (Ord node) =>
   Strict.Axis inst String vec (SignalFlow.TimeStep a) -> 
   FlowTopo.Section node (SignalFlow.Data inst vec (Interp.Val a)) ->
   SignalFlow.HRecord (XIdx.Position node) inst String vec a (Interp.Val a)
sectionToPowerRecord time (FlowTopoPlain.Section _ topo) =
   SignalFlow.HRecord time
   $ TopoRecord.topologyToPowerMap topo

