{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Modules.Optimisation where

import Debug.Trace

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
import EFA.Utility.Stream (Stream((:~)))
import qualified Data.Foldable as Fold 
import qualified Data.Map as M
import qualified Data.Vector as V

sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0

type SolveFunc a =
  (TIdx.Section -> 
     M.Map (XIdx.Eta Node) (String, XIdx.Eta Node -> XIdx.Power Node)) ->
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

etaGiven ::
  (Eq (Data.Apply c d1), Eq (Data.Apply c d2), Ord (idx node),
   Ord (idx1 node), Ord k, EqArith.Sum d2, EqArith.Sum d1,
   Data.ZipWith c, Data.Storage c d2, Data.Storage c d1,
   EqEnv.AccessMap idx, EqEnv.AccessMap idx1,
   EqEnv.PartElement (EqEnv.Environment idx) a v ~ Data c d2,
   EqEnv.PartElement (EqEnv.Environment idx1) a v ~ Data c d1) =>
   M.Map (idx node) (k, idx node -> idx1 node) ->
   M.Map k (d1 -> d2) ->
   EqGen.EquationSystem node s a v
etaGiven etaAssign etaFunc = Fold.fold $ M.mapWithKey f etaAssign
  where f n (str, g) = maybe mempty eq (M.lookup str etaFunc)
          where eq ef =
                  EqGen.variable n =.=
                    EqGen.liftF (Data.map ef) (EqGen.variable $ g n)


solveCharge ::
  ( Eq a, EqArith.Product a, EqArith.Integrate (Data Nil a),
    Fractional a, EqArith.Scalar (Data Nil a) ~ Data Nil a) => SolveFunc a
solveCharge etaAssign etaFunc pRest pRestLocal pWater pGas =
  EqGen.solve System.seqTopoOpt $
    givenCharging etaAssign etaFunc pRest pRestLocal pWater pGas

givenCharging ::
  (Eq a, Num a, EqArith.Sum a, Fractional a) => 
  (TIdx.Section -> 
     M.Map (XIdx.Eta Node) (String, XIdx.Eta Node -> XIdx.Power Node)) ->
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
  ( Eq a, EqArith.Product a, EqArith.Integrate (Data Nil a),
    Fractional a, EqArith.Scalar (Data Nil a) ~ Data Nil a) => SolveFunc a
solveDischarge etaAssign etaFunc pRest pRestLocal pWater pGas =
  EqGen.solve System.seqTopoOpt $
    givenDischarging etaAssign etaFunc pRest pRestLocal pWater pGas

givenDischarging ::
  (Eq a, Num a, EqArith.Sum a, Fractional a) => 
  (TIdx.Section -> 
     M.Map (XIdx.Eta Node) (String, XIdx.Eta Node -> XIdx.Power Node)) ->
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
 (Num a, Eq a,
  Base.BSum a, EqArith.Sum a,
  Eq (v a),
  SV.Zipper v,SV.FromList v,SV.Len (v a),
  SV.Singleton v,
  SV.Walker v,
  SV.Storage v a) =>
  (TIdx.Section -> 
     M.Map (XIdx.Eta Node) (String, XIdx.Eta Node -> XIdx.Power Node)) ->
  M.Map String (a -> a) ->
  SD.SequData (Record.PowerRecord Node v a) ->
  EqGen.EquationSystem Node s (Data Nil a) (Data (v :> Nil) a)

givenSimulate etaAssign etaFunc sf =
  (TIdx.absolute  (XIdx.storage TIdx.initial Water) EqUt..= Data 0)
   <> Fold.fold (SD.mapWithSection f sf)
   where f sec (Record.Record t xs) =
           (TIdx.absolute (XIdx.dTime sec) EqUt..= (Data  $ SV.fromList $ replicate (Sig.len t) 1)) --Sig.delta t))
           <> etaGiven (etaAssign sec) etaFunc
           <> Fold.fold (M.mapWithKey g xs)
           where 
             g (TIdx.PPos (TIdx.StructureEdge p0 p1)) p =
                   (TIdx.absolute (XIdx.power sec p0 p1) EqUt..= Sig.unpack p) -- $ Sip.deltaMap (\x y -> (x+y)/2) p)


-- | Avoid invalid solution by assigning NaN, which hits last in maximum
calcEtaSys :: (EqEnv.Complete  
               Node
               (EqRec.Absolute (Result Double))
               (EqRec.Absolute (Result Double)))
              -> Double
calcEtaSys env =
  if eCoal0 > 0 && eCoal1 > 0 && eTransformer0 > 0 && eTransformer1 > 0 then etaSys else (0/0)
  where
     eGas0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Gas LocalNetwork)) env
     eCoal0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Coal Network)) env
     eRest0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Rest Network)) env
     eRestLocal0 = (ModUt.lookupAbsEnergy (XIdx.energy sec0 LocalRest LocalNetwork)) env
     eGas1 = (ModUt.lookupAbsEnergy (XIdx.energy sec1 Gas LocalNetwork)) env
     eCoal1 =  (ModUt.lookupAbsEnergy (XIdx.energy sec1 Coal Network)) env
     eRest1 =  (lookupAbsEnergy (XIdx.energy sec1 Rest Network)) env
     eRestLocal1 =  (lookupAbsEnergy (XIdx.energy sec1 LocalRest LocalNetwork)) env
     eTransformer0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Network LocalNetwork)) env
     eTransformer1 =  (ModUt.lookupAbsEnergy (XIdx.energy sec1 Network LocalNetwork)) env

     etaSys = (eRest0 + eRest1 + eRestLocal0 + eRestLocal1) / (eGas0  + eGas1 + eCoal0 + eCoal1)
                 
{- -- | Avoid invalid solution by assigning NaN, which hits last in maximum
calcOptFunc :: (EqEnv.Complete  
               Node
               (EqRec.Absolute (Result Double))
               (EqRec.Absolute (Result Double)))
              -> Double
calcOptFunc env =
  -- trace (show [eCoal0, eCoal1, eTransformer0, eTransformer1]) $
  if eCoal0 > 0 && eCoal1 > 0 && eTransformer0 > 0 && eTransformer1 > 0 then etaSys else (0/0)
  where
     eGas0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Gas LocalNetwork)) env
     eCoal0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Coal Network)) env
     eRest0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Rest Network)) env
     eRestLocal0 = (ModUt.lookupAbsEnergy (XIdx.energy sec0 LocalRest LocalNetwork)) env
     eGas1 = (ModUt.lookupAbsEnergy (XIdx.energy sec1 Gas LocalNetwork)) env
     eCoal1 =  (ModUt.lookupAbsEnergy (XIdx.energy sec1 Coal Network)) env
     eRest1 =  (lookupAbsEnergy (XIdx.energy sec1 Rest Network)) env
     eRestLocal1 =  (lookupAbsEnergy (XIdx.energy sec1 LocalRest LocalNetwork)) env
     eTransformer0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Network LocalNetwork)) env
     eTransformer1 =  (ModUt.lookupAbsEnergy (XIdx.energy sec1 Network LocalNetwork)) env
     eCharge = (ModUt.lookupAbsEnergy (XIdx.energy sec0 Water Network)) env
     eDisCharge = (ModUt.lookupAbsEnergy (XIdx.energy sec1 Water Network)) env

     etaSys = (eRest0 + eRest1 + eRestLocal0 + eRestLocal1) / (eGas0  + eGas1 + eCoal0 + eCoal1)
     optFuncCharge = eCharge*socDrive+etaSys
     optFuncDischarge = -eDischarge*socDrive+etaSys
     
-- | Avoid invalid solution by assigning NaN, which hits last in maximum
calcEtaSys :: (EqEnv.Complete  
               Node
               (EqRec.Absolute (Result Double))
               (EqRec.Absolute (Result Double)))
              -> Double
calcEtaSys env =
  -- trace (show [eCoal0, eCoal1, eTransformer0, eTransformer1]) $
  if eCoal0 > 0 && eCoal1 > 0 && eTransformer0 > 0 && eTransformer1 > 0 then etaSys else (0/0)
  where
     eGas0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Gas LocalNetwork)) env
     eCoal0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Coal Network)) env
     eRest0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Rest Network)) env
     eRestLocal0 = (ModUt.lookupAbsEnergy (XIdx.energy sec0 LocalRest LocalNetwork)) env
     eGas1 = (ModUt.lookupAbsEnergy (XIdx.energy sec1 Gas LocalNetwork)) env
     eCoal1 =  (ModUt.lookupAbsEnergy (XIdx.energy sec1 Coal Network)) env
     eRest1 =  (lookupAbsEnergy (XIdx.energy sec1 Rest Network)) env
     eRestLocal1 =  (lookupAbsEnergy (XIdx.energy sec1 LocalRest LocalNetwork)) env
     eTransformer0 =  (ModUt.lookupAbsEnergy (XIdx.energy sec0 Network LocalNetwork)) env
     eTransformer1 =  (ModUt.lookupAbsEnergy (XIdx.energy sec1 Network LocalNetwork)) env

     etaSys = (eRest0 + eRest1 + eRestLocal0 + eRestLocal1) / (eGas0  + eGas1 + eCoal0 + eCoal1)
-}

maxEta :: Sig.UTSignal2 V.Vector V.Vector 
         (EqEnv.Complete  
         Node
         (EqRec.Absolute (Result Double))
         (EqRec.Absolute (Result Double))) ->
         (Double, 
          Maybe (EqEnv.Complete          
                 Node
                 (EqRec.Absolute (Result Double))
                 (EqRec.Absolute (Result Double))))
maxEta sigEnvs = (Sig.fromScalar etaMax, env) 
  where 
    etaSys = Sig.map calcEtaSys sigEnvs
    etaMax = Sig.maximum etaSys
    (xIdx, yIdx) = Sig.findIndex2 (== Sig.fromScalar etaMax) etaSys
    env = case (xIdx, yIdx) of 
      (Just xIdx', Just yIdx') -> Just $ Sig.getSample2D sigEnvs (xIdx',yIdx')
      _ -> Nothing



