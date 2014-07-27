module EFA.Action.Flow.StateFlow.Optimality where

import qualified EFA.Graph as Graph
import qualified EFA.Equation.Result as Result

import qualified EFA.Data.Interpolation as Interp

import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Part.Map as PartMap
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Flow.State as StateFlow
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Utility as ActUt
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.Part.Index as Idx

import qualified EFA.Signal.Data as D 

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Foldable as Fold

import EFA.Utility(Caller,
                 merror,
                   (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "EFA.Action.Flow.StateFlow.Optimality"

nc :: FunctionName -> Caller
nc = genCaller modul

data LifeCycleMethod = 
  N_SFG_EQ_N_STATE  

-- Different methods: 

-- A. Use SFG directly to calculate Energy flow (old way)
-- B. Extract scaling factors on Storage Power from SFG to predict new eta
-- C. Calculate a calibrated storage usage or generation from an existing sfg 
-- D. Do the same, taking existing not stored flows of other states into account 
-- C. Use Mix Calculation to extract the flow path efficiency


-- | Function works only for systems with one storage 
updateOneStorageLifeCycleEfficiencies :: 
  (Ord a, Arith.Constant a, Node.C node, Show node) =>
  Caller ->
  Topo.Topology node ->
  LifeCycleMethod ->
  (FlowOpt.GenerationEfficiency a, FlowOpt.UsageEfficiency a) ->
  StateQty.Graph node (Result.Result ((D.Data D.Nil (Interp.Val a)))) 
                      (Result.Result ((D.Data D.Nil (Interp.Val a)))) ->
  FlowOpt.LifeCycleMap node (Maybe a) ->  
  FlowOpt.LifeCycleMap node (Maybe a)

updateOneStorageLifeCycleEfficiencies caller topo method globalLifeCycleEtas sfg (FlowOpt.LifeCycleMap oldMap) = 
  FlowOpt.LifeCycleMap $ Map.mapWithKey f oldMap 
  where
    absSfg = StateQty.mapGraph g g  $ ActUt.absoluteStateFlowGraph topo sfg
    g = checkValid. (\(D.Data x) -> x) . (ActUt.checkDetermined "updateOneStorageLifeCycleEfficiencies") 
    checkValid x = if Interp.isInvalid x then err else Interp.unpack x 
    err = merror caller modul "updateOneStorageLifeCycleEfficiencies" "Invalid Values in StateFlowChart"
    etaSysSfg = calculateEtaSys globalLifeCycleEtas absSfg  
    f (Idx.AbsoluteState state) m =  case method of
          N_SFG_EQ_N_STATE -> Map.mapWithKey (etaSysState_Eq_etaSysSfg 
                              (caller |> nc "updateOneStorageLifeCycleEfficiencies") absSfg etaSysSfg (Idx.State $ fromIntegral state)) m
    

-- | This Approach calculates generation or use efficiency in a way system efficiency for sfg and the respective state are the same   
-- TODO: extend to several storages or check for amount of storages
etaSysState_Eq_etaSysSfg ::
  (Show node,
   Arith.Constant a,
   Node.C node) =>
  Caller ->
  StateFlow.Graph node Graph.EitherEdge a (TopoQty.Sums a) storageLabel (Maybe (TopoQty.Flow a)) carryLabel ->
  a ->
  Idx.State ->
  node ->
  (FlowOpt.GenerationEfficiency (Maybe a),
   FlowOpt.UsageEfficiency (Maybe a)) ->
  (FlowOpt.GenerationEfficiency (Maybe a),
   FlowOpt.UsageEfficiency (Maybe a))
etaSysState_Eq_etaSysSfg caller sfg etaSysSfg state sto (oldEtaGen,oldEtaUse) = 
  let 
--    newCaller = caller |> nc "etaSysState_Eq_etaSysSfg"
    flowSection = Maybe.fromMaybe err $ Map.lookup state $ StateQty.states sfg
    flowTopo = TopoQty.topology flowSection
    err = merror caller modul "etaSysState_Eq_etaSysSfg" $ "state not found in stateFlowGraph " ++ show state       
    err3 = merror caller modul "etaSysState_Eq_etaSysSfg" $ "storage not found in stateFlowGraph " ++ show sto     
    
    nodes = Graph.nodeLabels flowTopo
    sumRes = Map.foldl ((Arith.~+)) Arith.zero

    storages = Map.mapWithKey (\node _ -> TopoQty.lookupSums node flowSection) 
              $ Map.filterWithKey (\node _ -> Node.isStorage $ Node.typ node) nodes
               
    sinks = Map.mapMaybe TopoQty.sumIn $
              Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node) nodes
            
    sources = Map.mapMaybe TopoQty.sumOut $
              Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node) nodes
    
    totalSourceEnergy = sumRes sources
    totalSinkEnergy = sumRes sinks
    etaUse eSto = FlowOpt.UsageEfficiency $ Just $ (etaSysSfg Arith.~* totalSourceEnergy  Arith.~- totalSinkEnergy) Arith.~/ eSto
    etaGen eSto = FlowOpt.GenerationEfficiency $ Just $ eSto  Arith.~/ (totalSinkEnergy  Arith.~/ etaSysSfg  Arith.~- totalSourceEnergy)
    err2 = merror caller modul "etaSysState_Eq_etaSysSfg" "no Sums found"
    err4 = merror caller modul "etaSysState_Eq_etaSysSfg" "invalid flow"
    
    in case Maybe.fromMaybe err2 $ Maybe.fromMaybe err3 $ Map.lookup sto storages of
             TopoQty.Sums Nothing (Just sumOut) -> (etaGen sumOut ,oldEtaUse)
             TopoQty.Sums (Just sumIn) Nothing -> (oldEtaGen,etaUse sumIn )
             TopoQty.Sums Nothing Nothing -> (oldEtaGen,oldEtaUse)     
             TopoQty.Sums (Just _) (Just _) -> err4

 
calculateEtaSys ::
  (Ord node, Node.C node,
   Arith.Constant a, 
   Ord a) =>
  (FlowOpt.GenerationEfficiency a, FlowOpt.UsageEfficiency a) ->
  StateQty.Graph node a a ->
  a
calculateEtaSys (FlowOpt.GenerationEfficiency etaGen,FlowOpt.UsageEfficiency etaUse) sfg =
   let flowTopos = Map.map TopoQty.topology $ StateQty.states sfg
       nodes = Map.map Graph.nodeLabels flowTopos

       initBalance = Map.elems $ fmap (PartMap.init . Storage.nodes) (StateQty.storages sfg)
       exitBalance = Map.elems $ fmap (PartMap.exit . Storage.nodes) (StateQty.storages sfg)

       balance = zipWith (Arith.~-) exitBalance initBalance

       sinks = fmap (Map.mapMaybe TopoQty.sumIn .
              Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node)) nodes
              
       sources = fmap (Map.mapMaybe TopoQty.sumOut .
              Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node)) nodes
       sumRes =
          Map.foldl ((Arith.~+)) Arith.zero . Fold.fold . Map.elems
          
       sumCharge = Fold.foldl (\acc x -> if x > Arith.zero 
                                  then acc Arith.~+ (x Arith.~* etaUse) else acc) Arith.zero 

       sumDischarge = 
          Fold.foldl (\acc x -> if x > Arith.zero 
                                  then acc Arith.~+ (x Arith.~/ etaGen) else acc) Arith.zero


   in ((sumRes sinks)) Arith.~+ (sumCharge balance)   
      Arith.~/ 
      ((sumRes sources)) Arith.~+ (sumDischarge balance)



