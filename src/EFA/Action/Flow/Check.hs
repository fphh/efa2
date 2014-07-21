{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Flow.Check where

--import qualified EFA.Action.Flow as ActFlow
--import qualified EFA.Graph.Topology.Node as Node
--import EFA.Equation.Result (Result(Determined,Undetermined))
import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Graph as Graph
import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.Vector as DV
import qualified EFA.Flow.Topology.Quantity as TopoQty
--import qualified EFA.Flow.Topology as FlowTopo

-- import qualified Data.Foldable as Fold
--import qualified Data.Map as Map
--import qualified Data.Maybe as Maybe
--import Control.Applicative (liftA2)
--import Control.Monad(join)
--import Data.Foldable (Foldable, foldMap)

import EFA.Utility(Caller,
--                 merror,
               --    (|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Action.Flow.Topology"

nc :: FunctionName -> Caller
nc = genCaller modul

combineEdgeFlowStatus :: Int -> EdgeFlowStatus -> EdgeFlowStatus -> EdgeFlowStatus
combineEdgeFlowStatus expo (EdgeFlowStatus v s) (EdgeFlowStatus v1 s1) = 
  EdgeFlowStatus (combineValidity v v1) (combineFlowState expo s s1) 

combineFlowState :: Int -> Maybe Idx.AbsoluteState -> Maybe Idx.AbsoluteState -> Maybe Idx.AbsoluteState
combineFlowState expo (Just (Idx.AbsoluteState x)) (Just (Idx.AbsoluteState y)) = Just $ Idx.AbsoluteState $ x + y*3^expo 
combineFlowState _ _ _ = Nothing

combineValidity :: Validity ->  Validity -> Validity
combineValidity (Valid c) (Valid c1) = Valid $ combineConsistency c c1
combineValidity _ _ = Invalid

data EdgeFlowStatus = EdgeFlowStatus {getvalidity :: Validity, getState:: Maybe Idx.AbsoluteState} deriving (Show,Eq)
data Validity = Valid EdgeFlowConsistency | Invalid deriving (Show,Eq)

getEdgeFlowStatus :: 
  (Ord a, Arith.Constant a, DV.Zipper vec, DV.Walker vec,
   DV.Storage vec (Maybe Idx.AbsoluteState), DV.Storage vec (Interp.Val a),
   DV.Storage vec Validity, DV.Storage vec EdgeFlowStatus) =>
  (Interp.Val a -> Interp.Val a -> Maybe Idx.AbsoluteState) -> 
  (a -> a -> EdgeFlowConsistency) ->
  TopoQty.Flow (CubeMap.Data inst dim vec (Interp.Val a)) -> 
  CubeMap.Data inst dim vec EdgeFlowStatus
getEdgeFlowStatus getEdgeState edgeFlowCheck fl = f (TopoQty.flowPowerIn fl) (TopoQty.flowPowerOut fl)
  where 
     f p  p1 = (CubeMap.zipWithData (\x y -> EdgeFlowStatus x y) validity state) 
                           where validity = CubeMap.zipWithData (validityCheck edgeFlowCheck) p p1
                                 state = CubeMap.zipWithData getEdgeState p p1


validityCheck :: 
  (Ord a, Arith.Constant a) => 
  (a -> a -> EdgeFlowConsistency) ->
  (Interp.Val a) -> (Interp.Val a) -> Validity
validityCheck edgeFlowCheck p p1 = if (not $ Interp.isInvalid p) && (not $ Interp.isInvalid p1) 
                     then Valid $ edgeFlowCheck (Interp.unpack p) (Interp.unpack p1)
                     else Invalid

combineConsistency :: EdgeFlowConsistency -> EdgeFlowConsistency -> EdgeFlowConsistency
combineConsistency (EFC s e) (EFC s1 e1) = EFC (combineSignCheck s s1)  (combineEtaCheck e e1)  

data EdgeFlowConsistency = EFC SignCheck EtaCheck deriving (Show,Eq)
data EtaCheck = EtaOK | EtaNotOK deriving (Show,Eq)
data SignCheck = SignsOK | SignsNotOK deriving (Show,Eq)

combineSignCheck :: SignCheck -> SignCheck -> SignCheck
combineSignCheck SignsOK SignsOK = SignsOK
combineSignCheck _ _ = SignsNotOK

combineEtaCheck :: EtaCheck -> EtaCheck -> EtaCheck
combineEtaCheck EtaOK EtaOK = EtaOK
combineEtaCheck _ _ = EtaNotOK

etaCheckFromBool :: Bool -> EtaCheck
etaCheckFromBool True = EtaOK
etaCheckFromBool False = EtaNotOK

signCheckFromBool :: Bool -> SignCheck
signCheckFromBool True = SignsOK
signCheckFromBool False = SignsNotOK


