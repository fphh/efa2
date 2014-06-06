{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.Topology where

import qualified EFA.Action.Flow as ActFlow
import qualified EFA.Graph.Topology.Node as Node
import EFA.Equation.Result (Result(Determined,Undetermined))
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Graph as Graph
import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.Vector as DV
import qualified EFA.Flow.Topology.Quantity as TopoQty
-- import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

-- TODO:: Check function with Values
etaSys ::
   (Node.C node, Arith.Product v) =>
   TopoQty.Section node (Result v) -> Result v
etaSys =
   ActFlow.etaSys . fmap TopoQty.topology . Just

{-
absoluteStateIndex ::
   (Node.C node, Arith.Product v) =>
   Graph.Graph node Graph.DirEdge nodeLabel1 a1 ->
   TopoQty.Section node (Result v) -> Idx.AbsoluteState
absoluteStateIndex topo flow =
   ActFlow.absoluteStateIndex topo $ fmap TopoQty.topology flow 
-}

{-
absoluteStateIndex ::
  (Ord node,
   Ord (edge node),
   Data.Monoid.Monoid m) =>
  (edge node ->
   m) ->
  EFA.Flow.Topology.Section node edge sectionLabel nodeLabel edgeLabel ->
  m
-}

absoluteStateIndex :: 
  (Ord node,
   Ord a,
   Arith.Constant a,
   DV.Zipper vec,
   DV.Walker vec,
   DV.Storage vec Int,
   DV.Storage vec a,
   DV.Storage vec (Interp.Val a)) => 
  TopoQty.Section node (Result(CubeMap.Data inst dim vec (Interp.Val a))) -> 
  Result (CubeMap.Data inst dim vec Int)
absoluteStateIndex flowGraph = Maybe.fromJust $ snd $ Map.fold f (0,Nothing) $ Graph.edgeLabels $ TopoQty.topology flowGraph
          
f ::  (DV.Zipper vec, 
       DV.Storage vec Int, 
       Ord a, 
       Arith.Constant a, 
       DV.Storage vec (Interp.Val a), 
       DV.Storage vec a) => 
      Maybe (TopoQty.Flow (Result (CubeMap.Data inst dim vec (Interp.Val a)))) ->
    (Int,
    Maybe (Result (CubeMap.Data inst dim vec Int))) ->
    (Int,
    Maybe (Result (CubeMap.Data inst dim vec Int))) 
f (Just flow) (exp,Just state) = (exp+1,Just $ add state (newState flow exp))
f (Just flow) (exp,Nothing) = (exp+1,Just $ newState flow exp)


add (Determined cube) (Determined cube1) = Determined $ CubeMap.zipWithData  (+) cube cube1

edgeState :: (Ord a, Arith.Constant a) => Int -> Interp.Val a -> Interp.Val a -> Int
edgeState exp powerIn powerOut = case (powerIn, powerOut) of 
  (Interp.Invalid _,Interp.Invalid _) -> 9
  (_,Interp.Invalid _) -> 9
  (Interp.Invalid _,_) -> 9
  (_,_) -> es * 10^exp
  where es = case (Arith.sign powerIn, Arith.sign powerOut) of 
          (Arith.Zero,Arith.Zero)  -> 0 
          (Arith.Positive,Arith.Positive) -> 1
          (Arith.Negative,Arith.Negative) -> 2
          (Arith.Zero,Arith.Positive)  -> 3 
          (Arith.Zero,Arith.Negative) -> 4
          (Arith.Positive,Arith.Zero) -> 5
          (Arith.Positive,Arith.Negative) -> 6
          (Arith.Negative,Arith.Zero) -> 7
          (Arith.Negative,Arith.Positive) -> 8
 
newState :: 
  (DV.Zipper vec, Ord b, Arith.Constant b,
   DV.Storage vec b,DV.Storage vec (Interp.Val b),
   DV.Storage vec Int) =>
  TopoQty.Flow (Result (CubeMap.Data inst dim vec (Interp.Val b))) ->
  Int ->
  Result (CubeMap.Data inst dim vec Int)
newState fl exp = f (TopoQty.flowPowerIn fl) (TopoQty.flowPowerOut fl)
  where f (Determined p1) (Determined p2) = Determined  $ CubeMap.zipWithData (edgeState exp) p1 p2


{-
absoluteStateIndex2 :: 
  TopoQty.Section node (Result(CubeMap.Data inst dim vec a)) -> Result (CubeMap.Data inst dim vec a)
absoluteStateIndex2 flowGraph = 
  let
        edgeState p1 p2 = case (Arith.sign p1, Arith.sign p2) of 
          (Arith.Zero,Arith.Zero)  -> 0 
          (Arith.Positive,Arith.Positive) -> 1
          (Arith.Negative,Arith.Negative) -> 2
          (Arith.Zero,Arith.Positive)  -> 3 
          (Arith.Zero,Arith.Negative) -> 4
          (Arith.Positive,Arith.Zero) -> 5
          (Arith.Positive,Arith.Negative) -> 6
          (Arith.Negative,Arith.Zero) -> 7
          (Arith.Negative,Arith.Positive) -> 8
          
        newState fl = edgeState (TopoQty.flowPowerIn fl) (TopoQty.flowPowerOut fl)
        f (Just flow) (exp,state) = (exp+1, state Arith.~+ (newState flow)) --  Arith.~* (Arith.fromInteger $ 10^exp))
   
  in snd $ Map.fold f (0,Arith.zero) $ Graph.edgeLabels $ TopoQty.topology flowGraph
-}