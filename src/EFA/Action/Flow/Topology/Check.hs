{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.Topology.Check where

--import qualified EFA.Action.Flow as ActFlow
import qualified EFA.Action.Flow.Check as ActFlowCheck
--import qualified EFA.Action.Flow.Topology.Optimality as ActFlowTopoOpt
--import qualified EFA.Action.Flow.Optimality as ActFlowOpt
--import qualified EFA.Action.Optimisation.Sweep as Sweep

import qualified EFA.Graph.Topology.Node as Node
--import EFA.Equation.Result (Result(Determined,Undetermined))
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Graph as Graph
import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.Vector as DV
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.Topology as FlowTopo

import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph


-- import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
--import Control.Applicative (liftA2)
--import Control.Monad(join)
--import Data.Foldable (Foldable, foldMap)

import EFA.Utility(Caller,
                 merror,
               --    (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.Topology"

nc :: FunctionName -> Caller
nc = genCaller modul

   
getFlowStatus :: 
  (Ord node, Ord (edge node), Ord a, Arith.Constant a,Arith.NaNTestable a,
   DV.Zipper vec, DV.Walker vec, DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec (Maybe Idx.AbsoluteState), DV.Storage vec (Interp.Val a),
   DV.Storage vec ActFlowCheck.Validity) =>
  Caller ->
  FlowTopo.Section node edge sectionLabel nodeLabel (Maybe (TopoQty.Flow (CubeMap.Data inst dim vec (Interp.Val a)))) -> 
  CubeMap.Data inst dim vec ActFlowCheck.EdgeFlowStatus
getFlowStatus caller flowGraph = 
  Maybe.fromJust $ snd $ Map.foldl (flip f) (0,Nothing) $ Graph.edgeLabels $ TopoQty.topology flowGraph
  where           
    f (Just flow) (expo,Just status) = (expo+1,Just $ combineStatusResults expo status (getEdgeFlowStatus flow))
    f (Just flow) (expo,Nothing) = (expo+1,Just $ getEdgeFlowStatus flow)
    f Nothing (_,_) = merror caller modul "getFlowStatus" "Flow not defined"
{-
getFlowStatus' :: 
  (Ord node, Ord (edge node), Ord a, Arith.Constant a,Arith.NaNTestable a,
   DV.Zipper vec, DV.Walker vec, DV.Storage vec ActFlowCheck.EdgeFlowStatus,
   DV.Storage vec (Maybe Idx.AbsoluteState), DV.Storage vec (Interp.Val a),
   DV.Storage vec ActFlowCheck.Validity) =>
  Caller ->
  Topo.Topology node ->
  FlowTopo.Section node edge sectionLabel nodeLabel (Maybe (TopoQty.Flow (CubeMap.Data inst dim vec (Interp.Val a)))) -> 
  CubeMap.Data inst dim vec ActFlowCheck.EdgeFlowStatus
getFlowStatus' caller topo flowGraph = let 
    flabels = Graph.edgeLabels $ TopoQty.topology flowGraph
    flipEdge (f,t) = (t,f)
    getFlow e = case (Map.lookup e flabels,Map.lookup (flipEdge e) flabels) of
                (Nothing,Just fl) -> f
                (Just fl,f) -> f
                
    f edge (expo,Just status) = (expo+1,Just $ combineStatusResults expo status (getEdgeFlowStatus (getFlow edge)))
    f edge (expo,Nothing) = (expo+1,Just $ getEdgeFlowStatus (getFlow edge))
    f _ (_,_) = merror caller modul "getFlowStatus" "Flow not defined or inconsistent"
                
    in Maybe.fromJust $ snd $ foldl f (0,Nothing) $ Graph.edges topo --TopoQty.topology flowGraph            
-}
combineStatusResults :: 
  (DV.Zipper vec, DV.Storage vec ActFlowCheck.EdgeFlowStatus) => 
  Int ->
  CubeMap.Data inst dim vec ActFlowCheck.EdgeFlowStatus -> 
  CubeMap.Data inst dim vec ActFlowCheck.EdgeFlowStatus ->   
  CubeMap.Data inst dim vec ActFlowCheck.EdgeFlowStatus
combineStatusResults expo s s1 = 
  CubeMap.zipWithData 
  (ActFlowCheck.combineEdgeFlowStatus expo) s s1   


getEdgeFlowStatus :: 
  (Ord a, Arith.Constant a, DV.Zipper vec, DV.Walker vec,Arith.NaNTestable a,
   DV.Storage vec (Maybe Idx.AbsoluteState), DV.Storage vec (Interp.Val a),
   DV.Storage vec ActFlowCheck.Validity, DV.Storage vec ActFlowCheck.EdgeFlowStatus) =>
  TopoQty.Flow (CubeMap.Data inst dim vec (Interp.Val a)) -> 
  CubeMap.Data inst dim vec ActFlowCheck.EdgeFlowStatus
getEdgeFlowStatus fl = f (TopoQty.flowPowerIn fl) (TopoQty.flowPowerOut fl)
  where 
     f p  p1 = CubeMap.zipWithData (\x y -> ActFlowCheck.EdgeFlowStatus x y) validity state
                           where validity = CubeMap.zipWithData (ActFlowCheck.validityCheck edgeFlowCheck) p p1
                                 state = CubeMap.zipWithData getEdgeState p p1

getEdgeState :: 
  (Ord a, Arith.Constant a) => 
  Interp.Val a -> 
  Interp.Val a -> 
  Maybe Idx.AbsoluteState
getEdgeState p p1 = 
  let g x = Just $ case (Arith.sign x) of 
          (Arith.Zero)  -> Idx.AbsoluteState 1
          (Arith.Positive) -> Idx.AbsoluteState 0 
          (Arith.Negative) -> Idx.AbsoluteState 2
  in case (p,p1) of
          (Interp.Invalid _,_) -> Nothing 
          (_,Interp.Invalid _) -> Nothing
          (x,_) -> g $ Interp.unpack x 


-- TODO: ETA-Check prüfen
edgeFlowCheck ::  (Arith.Product a, Ord a, Arith.Constant a, Arith.NaNTestable a) => a -> a -> ActFlowCheck.EdgeFlowConsistency 
edgeFlowCheck x y = ActFlowCheck.EFC signCheck etaCheck
  where
    eta = if x >= Arith.zero then y Arith.~/ x else x Arith.~/ y
    etaCheck = ActFlowCheck.etaCheckFromBool $ eta > Arith.zero && eta <= Arith.one
    signCheck = ActFlowCheck.signCheckFromBool $ Arith.sign x == Arith.sign y && nanCheck
    -- TODO: nanCheck nochmal gesondert einbauen 
    nanCheck = if Arith.checkIsNaN x || Arith.checkIsNaN y then error "NaN detected" else True


{-
TODO :: Check Flow Graphs for correct sink / source flow

sinkSourceFlowCheck ::  
  ActFlowTopoOpt.EndNodeEnergies node (Result (CubeMap.Data (Sweep.Search inst) dim vec a)) ->
  Bool
sinkSourceFlowCheck (ActFlowTopoOpt.EndNodeEnergies (ActFlowOpt.SinkMap sinks) (ActFlowOpt.SourceMap sources) _) =  
  alwaysSinksOK && sinksOK && sourcesOK && alwaysSourcesOK
  where 
    alwaysSinksOK = all (> Arith.zero) $ Map.elems $ Map.filterWithKey (\k _ -> Node.typ k == Node.AlwaysSink) sinks
    sinksOK = all (>= Arith.zero) $ Map.elems $ Map.filterWithKey (\k _ -> Node.typ k == Node.Sink) sinks
    sourcesOK = all (>= Arith.zero) $ Map.elems $ Map.filterWithKey (\k _ -> Node.typ k == Node.Source) sources
    alwaysSourcesOK = all (> Arith.zero) $ Map.elems $ Map.filterWithKey (\k _ -> Node.typ k == Node.AlwaysSource) sources
    greaterThan x = fmap  
-}