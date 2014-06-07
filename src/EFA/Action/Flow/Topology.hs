{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.Topology where

import qualified EFA.Action.Flow as ActFlow
import qualified EFA.Graph.Topology.Node as Node
import EFA.Equation.Result (Result(Determined,Undetermined))
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Graph as Graph
--import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.Vector as DV
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Flow.Topology as FlowTopo

-- import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import EFA.Utility(Caller,
                 merror,
               --    (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.Topology"

nc :: FunctionName -> Caller
nc = genCaller modul

-- TODO:: Check function with Values
etaSys ::
   (Node.C node, Arith.Product v) =>
   TopoQty.Section node (Result v) -> Result v
etaSys =
   ActFlow.etaSys . fmap TopoQty.topology . Just
   
getFlowStatus :: 
  (Ord node, Ord (edge node), Ord a, Arith.Constant a,
   DV.Zipper vec, DV.Walker vec, DV.Storage vec EdgeFlowStatus,
   DV.Storage vec (Maybe Int), DV.Storage vec (Interp.Val a),
   DV.Storage vec Validity) =>
  Caller ->
  FlowTopo.Section node edge sectionLabel nodeLabel (Maybe (TopoQty.Flow (Result (CubeMap.Data inst dim vec (Interp.Val a))))) -> 
  Result (CubeMap.Data inst dim vec EdgeFlowStatus)
getFlowStatus caller flowGraph = 
  Maybe.fromJust $ snd $ Map.fold f (0,Nothing) $ Graph.edgeLabels $ TopoQty.topology flowGraph
  where           
    f (Just flow) (expo,Just status) = (expo+1,Just $ combineStatusResults expo status (getEdgeFlowStatus flow))
    f (Just flow) (expo,Nothing) = (expo+1,Just $ getEdgeFlowStatus flow)
    f Nothing (_,_) = merror caller modul "getFlowStatus" "Flow not defined"


combineStatusResults :: 
  (DV.Zipper vec, DV.Storage vec EdgeFlowStatus) => 
  Int ->
  Result (CubeMap.Data inst dim vec EdgeFlowStatus) -> 
  Result (CubeMap.Data inst dim vec EdgeFlowStatus) ->   
  Result (CubeMap.Data inst dim vec EdgeFlowStatus)
combineStatusResults expo (Determined s) (Determined s1) = 
  Determined $ CubeMap.zipWithData 
  (combineEdgeFlowStatus expo) s s1   
combineStatusResults _ _ _ = Undetermined  

combineEdgeFlowStatus :: Int -> EdgeFlowStatus -> EdgeFlowStatus -> EdgeFlowStatus
combineEdgeFlowStatus expo (EdgeFlowStatus v s) (EdgeFlowStatus v1 s1) = EdgeFlowStatus (combineValidity v v1) (combineFlowState expo s s1) 

combineFlowState :: Int -> Maybe Int -> Maybe Int -> Maybe Int
combineFlowState expo (Just x) (Just y) = Just $ x + y*3^expo 
combineFlowState _ _ _ = Nothing

combineValidity :: Validity ->  Validity -> Validity
combineValidity (Valid c) (Valid c1) = Valid $ combineConsistency c c1
combineValidity _ _ = Invalid

data EdgeFlowStatus = EdgeFlowStatus Validity (Maybe Int) deriving Show
data Validity = Valid EdgeFlowConsistency | Invalid deriving Show

getEdgeFlowStatus :: 
  (Ord a, Arith.Constant a, DV.Zipper vec, DV.Walker vec,
   DV.Storage vec (Maybe Int), DV.Storage vec (Interp.Val a),
   DV.Storage vec Validity, DV.Storage vec EdgeFlowStatus) =>
  TopoQty.Flow (Result (CubeMap.Data inst dim vec (Interp.Val a))) -> 
  Result (CubeMap.Data inst dim vec EdgeFlowStatus)
getEdgeFlowStatus fl = f (TopoQty.flowPowerIn fl) (TopoQty.flowPowerOut fl)
  where 
     f (Determined p)  (Determined p1) = Determined (CubeMap.zipWithData (\x y -> EdgeFlowStatus x y) validity state) 
                           where validity = CubeMap.zipWithData validityCheck p p1
                                 state = CubeMap.mapData getEdgeState p
     f _ _ = Undetermined

getEdgeState :: 
  (Ord a, Arith.Constant a) => 
  Interp.Val a -> Maybe Int
getEdgeState p = 
  let g x = Just $ case (Arith.sign x) of 
          (Arith.Zero)  -> 0
          (Arith.Positive) -> 1 
          (Arith.Negative) -> 2
  in case p of
          (Interp.Inter x) -> g x 
          (Interp.Invalid _) -> Nothing
          (Interp.Extra x) -> g x 


validityCheck :: 
  (Ord a, Arith.Constant a) => 
  (Interp.Val a) -> (Interp.Val a) -> Validity
validityCheck p p1 = if (not $ Interp.isInvalid p) && (not $ Interp.isInvalid p1) 
                     then Valid $ edgeFlowCheck (Interp.unpack p) (Interp.unpack p1)
                     else Invalid

combineConsistency :: EdgeFlowConsistency -> EdgeFlowConsistency -> EdgeFlowConsistency
combineConsistency (EFC s e) (EFC s1 e1) = EFC (combineSignCheck s s1)  (combineEtaCheck e e1)  

data EdgeFlowConsistency = EFC SignCheck EtaCheck deriving Show
data EtaCheck = EtaOK | EtaNotOK deriving Show
data SignCheck = SignsOK | SignsNotOK deriving Show

combineSignCheck :: SignCheck -> SignCheck -> SignCheck
combineSignCheck SignsOK SignsOK = SignsOK
combineSignCheck _ _ = SignsNotOK

combineEtaCheck :: EtaCheck -> EtaCheck -> EtaCheck
combineEtaCheck EtaOK EtaOK = EtaOK
combineEtaCheck _ _ = EtaNotOK

etaCheckFromBool :: Bool -> EtaCheck
etaCheckFromBool True = EtaOK
etaCheckFromBool False = EtaNotOK

etaCheckToBool :: EtaCheck -> Bool
etaCheckToBool EtaOK = True
etaCheckToBool EtaNotOK = False

signCheckFromBool :: Bool -> SignCheck
signCheckFromBool True = SignsOK
signCheckFromBool False = SignsNotOK

signCheckToBool :: SignCheck -> Bool
signCheckToBool SignsOK = True
signCheckToBool SignsNotOK = False

edgeFlowCheck ::  (Arith.Product a, Ord a, Arith.Constant a) => a -> a -> EdgeFlowConsistency 
edgeFlowCheck x y = EFC signCheck etaCheck
  where
    eta = if x >= Arith.zero then y Arith.~/ x else x Arith.~/ y
    etaCheck = etaCheckFromBool $ eta > Arith.zero && eta < Arith.one 
    signCheck = signCheckFromBool $ Arith.sign x == Arith.sign y
