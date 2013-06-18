{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Modules.Optimisation where

-- import Debug.Trace

import qualified Modules.System as System
import Modules.System (Node(..))
import Modules.Utility as ModUt

import qualified EFA.Example.Absolute as EqGen
-- import qualified EFA.Example.Absolute as EqAbs
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Example.Index as XIdx
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import qualified EFA.Equation.Arithmetic as EqArith

import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Topology as TD

import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Data as Data
import qualified EFA.Signal.Base as Base

import qualified EFA.Signal.Vector as SV
-- import qualified Data.Vector as Vec

import EFA.Equation.Result (Result(..))
--import EFA.Utility.Map (checkedLookup)
import EFA.Signal.Data (Data(..), Nil, (:>))

import qualified EFA.Utility.Stream as Stream

import Data.Monoid (mconcat, (<>), mempty)
import EFA.Example.Absolute ( (.=), (%=), (=.=) )
import qualified EFA.Example.Utility as EqUt
import qualified EFA.Example.EtaSys as ES


import EFA.Utility.Stream (Stream((:~)))
import qualified Data.Foldable as Fold 
import qualified Data.Map as M
import qualified Data.Vector as V

import Control.Applicative (liftA2)

sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0

type SolveFunc a =
  (TIdx.Section -> 
     M.Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)) ->
  M.Map String (a -> a) ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  EqEnv.Complete  
    Node
    (EqRec.Absolute (Result (Data Nil a)))
    (EqRec.Absolute (Result (Data Nil a)))

commonGiven ::
  (EqArith.Sum a, Num a, Eq a) =>
  EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a)
commonGiven =
   mconcat $
   (XIdx.dTime sec0 .= Data 1) :
   (XIdx.dTime sec1 .= Data 1) :
   (XIdx.storage TIdx.Initial Water .= Data 0) :
   (XIdx.energy sec0 Water Network  %= XIdx.energy sec1 Water Network) :
   []

etaGiven :: (Eq (Data.Apply c d1), Eq (Data.Apply c d2), Fractional d2, Num d1,
         Show key, Show d1,
          Ord (idx1 node), Ord (idx node), Ord d1, Ord key, EqArith.Sum d1,
          EqArith.Sum d2, EqEnv.AccessMap idx1, EqEnv.AccessMap idx,
          Data.ZipWith c, Data.Storage c d2, Data.Storage c d1,
          EqEnv.PartElement (EqEnv.Environment idx) a v ~ Data c d2,
          EqEnv.PartElement (EqEnv.Environment idx1) a v ~ Data c d1) =>
         M.Map (idx node) (key, key, idx node -> idx1 node) ->
         M.Map key (d1 -> d2) ->
         EqGen.EquationSystem node s a v
etaGiven etaAssign etaFunc = Fold.fold $ M.mapWithKey f etaAssign
  where f n (strP, strN, g) =
          EqGen.variable n =.= EqGen.liftF (Data.map ef) (EqGen.variable $ g n)
          where ef x = if x >= 0 then fpos x else fneg x
                fpos = maybe (err strP) id (M.lookup strP etaFunc)
                fneg = maybe (err strN) (\n -> recip . n . negate) 
                                        (M.lookup strN etaFunc)
                err str x = error ("not defined: " ++ show str ++ " for " ++ show x)

{-
eqs ::
  (a ~ EqArith.Scalar v, Eq a,
   Eq v, EqArith.Product a, EqArith.Product v, EqArith.Integrate v) =>
  EqGen.EquationSystem Node s a v
eqs = EqGen.fromGraph True (TD.dirFromSequFlowGraph (snd System.seqTopoOpt))
-}

solveCharge ::
  ( Eq a, Show a, EqArith.Product a, EqArith.Integrate (Data Nil a),Ord a,
    Fractional a, EqArith.Scalar (Data Nil a) ~ Data Nil a) =>
  (forall s. EqGen.EquationSystem Node s (Data Nil a) (Data Nil a)) -> SolveFunc a
solveCharge eqs etaAssign etaFunc pRest pRestLocal pWater pGas =
  EqGen.solve2 eqs $
    givenCharging etaAssign etaFunc pRest pRestLocal pWater pGas

givenCharging ::
  (Eq a, Num a, Show a, EqArith.Sum a, Fractional a, Ord a) => 
  (TIdx.Section -> 
     M.Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)) ->
  M.Map String (a -> a) ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a)
givenCharging etaAssign etaFunc pRest pRestLocal pWater pGas =
   ((commonGiven <> etaGiven (etaAssign sec0) etaFunc) <>) $
   mconcat $
   -- Actual Section 0 Charhing to be varied and optimised
   (XIdx.power sec0 Rest Network .= pRest) :
   (XIdx.power sec0 LocalRest LocalNetwork .= pRestLocal) :
   (XIdx.power sec0 Network Water .= pWater) :
   (XIdx.power sec0 LocalNetwork Gas .= pGas) :

   -- Average Section 1 discharging
   (XIdx.eta sec1 Network Rest .= Data 1.0) :
   (XIdx.eta sec1 LocalNetwork LocalRest .= Data 1.0) :
   (XIdx.eta sec1 Network LocalNetwork .= Data 0.862) :
   (XIdx.eta sec1 Coal Network .= Data 0.345) :
   (XIdx.eta sec1 Gas LocalNetwork .= Data 0.346) :
   (XIdx.eta sec1 Water Network .= Data 0.82) :
   (XIdx.x sec1 Network Water .= Data 0.7) :
   (XIdx.x sec1 Network LocalNetwork .= Data 0.766) :
   (XIdx.x sec1 LocalNetwork Network .= Data 0.677) :
   []



solveDischarge :: 
  ( Eq a, Show a, EqArith.Product a, EqArith.Integrate (Data Nil a),Ord a,
    Fractional a, EqArith.Scalar (Data Nil a) ~ Data Nil a) =>
  (forall s. EqGen.EquationSystem Node s (Data Nil a) (Data Nil a)) -> SolveFunc a
solveDischarge eqs etaAssign etaFunc pRest pRestLocal pWater pGas =
  EqGen.solve2 eqs $
    givenDischarging etaAssign etaFunc pRest pRestLocal pWater pGas


givenDischarging ::
  (Eq a, Num a, Show a, EqArith.Sum a, Fractional a,Ord a) => 
  (TIdx.Section -> 
     M.Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)) ->
  M.Map String (a -> a) ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a)
givenDischarging etaAssign etaFunc pRest pRestLocal pWater pGas =
   ((commonGiven <> etaGiven (etaAssign sec1) etaFunc) <>) $
   mconcat $
   (XIdx.power sec1 Rest Network .= pRest) :
   (XIdx.power sec1 LocalRest LocalNetwork .= pRestLocal) :
   (XIdx.power sec1 Network Water .= pWater) :
   (XIdx.power sec1 LocalNetwork Gas .= pGas) :

   (XIdx.eta sec0 Coal Network .= Data 0.440) :
   (XIdx.eta sec0 Gas LocalNetwork .= Data 0.303) :
   (XIdx.eta sec0 Network Water .= Data 0.331) :
   (XIdx.eta sec0 Network LocalNetwork .= Data 0.939) :
   (XIdx.eta sec0 Network Rest .= Data 1) :
   (XIdx.eta sec0 LocalNetwork LocalRest .= Data 1.0) :
   
   (XIdx.x sec0 Network Water .= Data 0.054) :
   (XIdx.x sec0 Coal Network .= Data 1) :
   (XIdx.x sec0 Network LocalNetwork .= Data 0.669) :   
   (XIdx.x sec0 LocalNetwork Network .= Data 0.881) :
   []

givenSimulate ::
 (Num a, Eq a, Show a, Fractional a, Ord a,
  Base.BSum a, EqArith.Sum a,
  Eq (v a),
  SV.Zipper v,SV.FromList v,SV.Len (v a),
  SV.Singleton v,
  SV.Walker v,
  SV.Storage v a) =>
  (TIdx.Section -> 
     M.Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)) ->
  M.Map String (a -> a) ->
  SD.SequData (Record.PowerRecord Node v a) ->
  EqGen.EquationSystem Node s (Data Nil a) (Data (v :> Nil) a)

givenSimulate etaAssign etaFunc sf =
  (TIdx.absolute  (XIdx.storage TIdx.initial Water) EqUt..= Data 0)
   <> Fold.fold (SD.mapWithSection f sf)
   where f sec (Record.Record t xs) =
           (TIdx.absolute (XIdx.dTime sec) EqUt..= 
             (Data  $ SV.fromList $ replicate (Sig.len t) 1))
           <> etaGiven (etaAssign sec) etaFunc
           <> Fold.fold (M.mapWithKey g xs)
           where 
             g (TIdx.PPos (TIdx.StructureEdge p0 p1)) p =
                   (TIdx.absolute (XIdx.power sec p0 p1) EqUt..= Sig.unpack p)

      
-- | Avoid invalid solution by assigning NaN, which hits last in maximum
-- b == True -> Charge
-- b == False -> Discharge

calcOptFunc ::
  Flow.RangeGraph Node ->
  Bool ->
  Double ->
  (EqEnv.Complete  
    Node
    (EqRec.Absolute (Result Double))
    (EqRec.Absolute (Result Double))) -> Double
calcOptFunc topo b socDrive env =
  if and (map (>0) [eCoal0, eCoal1, eTrans0, eTrans1]) then res else nan
  where nan = 0/0
        lu idx = EqUt.checkDetermined (show idx) $ 
                   ES.lookupAbsEnergy "calcOptFunc" env idx
        eCoal      = lu $ XIdx.energy sec0 Coal Network
        eCoal0     = lu $ XIdx.energy sec0 Coal Network
        eCoal1     = lu $ XIdx.energy sec1 Coal Network
        eTrans0    = lu $ XIdx.energy sec0 Network LocalNetwork
        eTrans1    = lu $ XIdx.energy sec1 Network LocalNetwork

        eCharge    = lu $ XIdx.energy sec0 Water Network
        eDischarge = lu $ XIdx.energy sec1 Water Network

        Determined etaSys = ES.etaSys topo env
        res = etaSys + socDrive * (if b then eCharge else -eDischarge)


maxEta ::
  Flow.RangeGraph Node ->
  Sig.UTSignal2 V.Vector V.Vector 
  (EqEnv.Complete  
    Node
    (EqRec.Absolute (Result Double))
    (EqRec.Absolute (Result Double))) ->
    (Double, 
      Maybe (EqEnv.Complete          
              Node
              (EqRec.Absolute (Result Double))
              (EqRec.Absolute (Result Double))))
maxEta topo sigEnvs = maxOpt topo True 0 sigEnvs 

maxOpt ::
  Flow.RangeGraph Node ->
  Bool ->
  Double ->
  Sig.UTSignal2 V.Vector V.Vector
    (EqEnv.Complete  
      Node
      (EqRec.Absolute (Result Double))
      (EqRec.Absolute (Result Double))) ->
    (Double, 
      Maybe (EqEnv.Complete          
              Node
              (EqRec.Absolute (Result Double))
              (EqRec.Absolute (Result Double))))
maxOpt topo b socDrive sigEnvs = (etaMax, env) 
  where etaSys = Sig.map (calcOptFunc topo b socDrive) sigEnvs
        etaMax = Sig.fromScalar $ Sig.maximum etaSys
        (xIdx, yIdx) = Sig.findIndex2 (== etaMax) etaSys
        env = liftA2 (Sig.getSample2D sigEnvs) xIdx yIdx
