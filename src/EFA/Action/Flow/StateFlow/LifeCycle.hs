{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.StateFlow.LifeCycle where

import qualified EFA.Graph as Graph
import qualified EFA.Equation.Result as Result

--import qualified EFA.Data.Interpolation as Interp

import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Part.Map as PartMap
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Equation.Arithmetic as Arith

--import qualified EFA.Flow.State as StateFlow
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Utility as ActUt
import qualified EFA.Action.Flow.Topology.State as TopoState

import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.Part.Index as Idx

import qualified EFA.Signal.Data as D 

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Foldable as Fold

import qualified Debug.Trace as Trace

import qualified EFA.Utility.Trace as UtTrace 

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
  (Ord a, Arith.Constant a, Node.C node, Show node, Show a, 
   Show (TopoQty.Flow (Result.Result (D.Data D.Nil a)))) =>
  Caller ->
  Topo.Topology node ->
  LifeCycleMethod ->
  FlowOpt.GlobalLifeCycleMap node (a) ->
--  (FlowOpt.GenerationEfficiency a, FlowOpt.UsageEfficiency a) ->
  StateQty.Graph node (Result.Result ((D.Data D.Nil (a)))) 
                      (Result.Result ((D.Data D.Nil (a)))) ->
  FlowOpt.LifeCycleMap node (a) ->  
  (a,FlowOpt.LifeCycleMap node (a))

updateOneStorageLifeCycleEfficiencies caller topo method globalLifeCycleMap sfg (FlowOpt.LifeCycleMap oldMap) = (etaSysSfg,
  FlowOpt.LifeCycleMap $ Map.union (Map.mapKeys (\(Idx.State state) -> Idx.AbsoluteState $ fromIntegral state) $ 
                                    Map.mapWithKey f  $ StateQty.states absSfg) oldMap) 
  where
    newCaller = caller |> nc "updateOneStorageLifeCycleEfficiencies"
    absSfg = TopoState.absoluteStateFlowGraph topo sfg
    -- absSfg = fmap g g  $ ActUt.absoluteStateFlowGraph topo sfg
    --g = (\(D.Data x) -> x) . (ActUt.checkDetermined "updateOneStorageLifeCycleEfficiencies") 
--    checkValid x = if Interp.isInvalid x then err else Interp.unpack x 
    --err = merror caller modul "updateOneStorageLifeCycleEfficiencies" "Invalid Values in StateFlowChart"
    etaSysSfg = calculateEtaSys newCaller globalLifeCycleMap absSfg  
    f (Idx.State state) _ =  case method of
          N_SFG_EQ_N_STATE -> Map.mapWithKey (etaSysState_Eq_etaSysSfg 
                              (caller |> nc "updateOneStorageLifeCycleEfficiencies") absSfg etaSysSfg (Idx.State $ fromIntegral state)) 
                               (oldMap Map.! (Idx.AbsoluteState  $ fromIntegral state))
    

-- | This Approach calculates generation or use efficiency in a way system efficiency for sfg and the respective state are the same   
-- TODO: extend to several storages or check for amount of storages
etaSysState_Eq_etaSysSfg ::
  (Show node, Show a,
   Arith.Constant a,
   Node.C node) =>
  Caller ->
  StateQty.Graph node (Result.Result ((D.Data D.Nil (a)))) 
                      (Result.Result ((D.Data D.Nil (a)))) ->
  a ->
  Idx.State ->
  node ->
  (FlowOpt.GenerationEfficiency a,
   FlowOpt.UsageEfficiency a) ->
  (FlowOpt.GenerationEfficiency a,
   FlowOpt.UsageEfficiency a)
etaSysState_Eq_etaSysSfg caller absSfg etaSysSfg state sto (oldEtaGen,oldEtaUse) = 
  let 
--    newCaller = caller |> nc "etaSysState_Eq_etaSysSfg"
    flowSection = Maybe.fromMaybe err $ Map.lookup state $ StateQty.states absSfg
    flowTopo = TopoQty.topology flowSection
    err = merror caller modul "etaSysState_Eq_etaSysSfg" $ "state not found in stateFlowGraph " ++ show state       
    err3 = merror caller modul "etaSysState_Eq_etaSysSfg" $ "storage not found in stateFlowGraph " ++ show sto     
    
    nodes = Graph.nodeLabels flowTopo
    sumRes = Map.foldl ((Arith.~+)) Arith.zero

    f (TopoQty.Sums x y)  =  TopoQty.Sums  (fmap g x) (fmap g y)
    
    g (Result.Determined (D.Data x)) = x
    g Result.Undetermined = error "Bust"                                  
    
    storages = Map.map (fmap f) $ Map.mapWithKey (\node _ -> TopoQty.lookupSums node flowSection) 
              $ Map.filterWithKey (\node _ -> Node.isStorage $ Node.typ node) nodes
               
    sinks = Map.map g $ Map.mapMaybe TopoQty.sumIn $
              Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node) nodes
            
    sources = Map.map g $ Map.mapMaybe TopoQty.sumOut $
              Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node) nodes
    
    totalSourceEnergy = sumRes sources
    totalSinkEnergy = sumRes sinks
    etaUse eSto = FlowOpt.UsageEfficiency $ ((etaSysSfg Arith.~* totalSourceEnergy)  Arith.~- totalSinkEnergy) Arith.~/ eSto
    etaGen eSto = FlowOpt.GenerationEfficiency $ eSto  Arith.~/ (totalSinkEnergy  Arith.~/ etaSysSfg  Arith.~- totalSourceEnergy)
    err2 = merror caller modul "etaSysState_Eq_etaSysSfg" "no Sums found"
    err4 = merror caller modul "etaSysState_Eq_etaSysSfg" "invalid flow"
    
    traceStr = " State: " ++ show state ++ 
      " Sinks: " ++ show totalSinkEnergy  ++ 
      " Sources: " ++ show totalSourceEnergy ++ 
      " Sto: " ++ (show $ Map.lookup sto storages) ++
      " EtaSys: " ++ (show etaSysSfg)
    
    in Trace.trace traceStr $ case Maybe.fromMaybe err2 $ Maybe.fromMaybe err3 $ Map.lookup sto storages of
             TopoQty.Sums Nothing (Just sumOut) -> -- UtTrace.simTrace "etaGen" $ 
                                                   (etaGen sumOut , oldEtaUse)
             TopoQty.Sums (Just sumIn) Nothing -> --UtTrace.simTrace "etaUse" $ 
                                                  (oldEtaGen, etaUse sumIn )
             TopoQty.Sums Nothing Nothing -> (oldEtaGen,oldEtaUse)     
             TopoQty.Sums (Just _) (Just _) -> err4

 
calculateEtaSys ::
  (Ord node, Node.C node, Show a, Show node,
   Arith.Constant a, (Show (TopoQty.Flow (Result.Result (D.Data D.Nil a)))),
   Ord a) =>
  Caller -> 
  FlowOpt.GlobalLifeCycleMap node a ->
  StateQty.Graph node (Result.Result (D.Data D.Nil a)) (Result.Result (D.Data D.Nil a)) ->
  a
calculateEtaSys caller globalLifeCycleMap sfg = 
  --UtTrace.simTrace "calculateEtaSys-Eta" $ 
   let flowTopos = Map.map TopoQty.topology $ StateQty.states  sfg
       nodes = Map.map Graph.nodeLabels flowTopos

       initBalance = map f $ Map.toList $ fmap (PartMap.init . Storage.nodes) (StateQty.storages sfg)
       exitBalance = map f $ Map.toList $ fmap (PartMap.exit . Storage.nodes) (StateQty.storages sfg)

       f (node,Result.Determined (D.Data x)) = (node,x)
       f (_,Result.Undetermined) = error "calculateEtaSys -- Undetermined"
 
       f1 (Result.Determined (D.Data x)) = x
       f1 (Result.Undetermined) = error "calculateEtaSys -- Undetermined"
        
       balance = zipWith (\(node,x) (node1,y) -> if node == node1 then (node,x Arith.~- y) else err node node1) exitBalance initBalance
       
       err n n1 = merror caller modul "calculateEtaSys" $ "inconsistent storage nodes: " ++ show n ++ " " ++ show n1

       sinks = fmap (fmap f1) $ fmap (Map.mapMaybe TopoQty.sumIn .
              Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node)) nodes
              
       sources = fmap (fmap f1) $ fmap (Map.mapMaybe TopoQty.sumOut .
              Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node)) nodes

       sumRes =
          Map.foldl ((Arith.~+)) Arith.zero . Fold.fold . Map.elems
          
       sumCharge = 
         Fold.foldl (\acc (node,x) -> let 
                        (FlowOpt.UsageEfficiency etaUse) = snd $  Maybe.fromMaybe (err2 node) $
                             FlowOpt.lookupLifeCycleEtaGlobalLifeCycleEta globalLifeCycleMap node  
                                 in if x > Arith.zero 
                                   then acc Arith.~+ (x Arith.~* etaUse) 
                                   else acc) Arith.zero 

       
       sumDischarge = 
          Fold.foldl (\acc (node,x) -> let 
                         (FlowOpt.GenerationEfficiency etaGen) =  fst $ Maybe.fromMaybe (err2 node) $
                                FlowOpt.lookupLifeCycleEtaGlobalLifeCycleEta globalLifeCycleMap node
                         in if x < Arith.zero 
                            then acc Arith.~- (x Arith.~/ etaGen) 
                            else acc) Arith.zero
                                
       err2 n =  merror caller modul "calculateEtaSys" $ "node not found in globalLifeCycleMap: " ++ show n
       
       traceStr = 
         " Sinks: " ++ show (sumRes sinks)  ++ 
         " Sources: " ++ show (sumRes sources) ++ 
         " Charge: " ++ show (sumCharge balance) ++
         " Sto: " ++ show balance ++
         " DisCharge: " ++ show (sumDischarge balance)


   in Trace.trace traceStr $ ((sumRes sinks) Arith.~+ (sumCharge balance))   
      Arith.~/ 
      ((sumRes sources) Arith.~+ (sumDischarge balance))



