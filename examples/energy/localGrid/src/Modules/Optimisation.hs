{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Modules.Optimisation where


import qualified Modules.System as System
import Modules.System (Node(..))

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
import qualified Data.Vector as Vec

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


sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0

solveCharge :: (Double -> Double) -> 
               (Double -> Double) -> 
               (Double -> Double) -> 
               (Double -> Double) -> Double ->               
               Double -> 
               Double -> 
               Double -> 
               EqEnv.Complete  
               Node
               (EqRec.Absolute (Result Double))
               (EqRec.Absolute (Result Double))
solveCharge etaWater etaCoal etaGas etaTrans pRest pRestLocal pWater pGas 
  = EqGen.solve System.seqTopoOpt $ 
    givenCharging etaWater etaCoal etaGas etaTrans pRest pRestLocal pWater pGas
 
solveDischarge :: (Double -> Double) -> 
                  (Double -> Double) -> 
                  (Double -> Double) -> 
                  (Double -> Double) -> 
                  Double ->               
                  Double -> 
                  Double -> 
                  Double -> 
                  EqEnv.Complete  
                  Node
                  (EqRec.Absolute (Result Double))
                  (EqRec.Absolute (Result Double))
solveDischarge etaWater etaCoal etaGas etaTrans pRest pRestLocal pWater pGas 
  = EqGen.solve System.seqTopoOpt $ 
    givenDischarging etaWater etaCoal etaGas etaTrans pRest pRestLocal pWater pGas

commonGiven :: EqGen.EquationSystem System.Node s Double Double
commonGiven =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (XIdx.dTime sec1 .= 1) :
   (XIdx.storage TIdx.Initial Water .= 0) :
   (XIdx.energy sec0 Water Network  %= XIdx.energy sec1 Water Network) :
   []

givenCharging :: (Double -> Double) -> 
                         (Double -> Double) -> 
                         (Double -> Double) -> 
                         (Double -> Double) -> 
                         Double -> 
                         Double -> 
                         Double -> 
                         Double -> 
                         EqGen.EquationSystem System.Node s Double Double
givenCharging lookupEtaWaterCharge lookupEtaCoal lookupEtaGas lookupEtaTransformerHL pRest pRestLocal pWater pGas =
   (commonGiven <>) $
   mconcat $

   -- Actual Section 0 Charhing to be varied and optimised
   (XIdx.power sec0 Rest Network .= pRest) :
   (XIdx.power sec0 LocalRest LocalNetwork .= pRestLocal) :
   (XIdx.power sec0 Network Water .= pWater) :
   (XIdx.power sec0 Gas LocalNetwork .= pGas) :
   ((EqGen.variable $ XIdx.eta sec0 Network Water) =.=
     EqGen.liftF lookupEtaWaterCharge (EqGen.variable $ XIdx.power sec0 Network Water)) :
   ((EqGen.variable $ XIdx.eta sec0 Coal Network) =.=
     EqGen.liftF lookupEtaCoal (EqGen.variable $ XIdx.power sec0 Network Coal)) :
   ((EqGen.variable $ XIdx.eta sec0 Gas LocalNetwork) =.=
     EqGen.liftF lookupEtaGas (EqGen.variable $ XIdx.power sec0 Gas LocalNetwork)) :
   ((EqGen.variable $ XIdx.eta sec0 Network LocalNetwork) =.=
     EqGen.liftF lookupEtaTransformerHL (EqGen.variable $ XIdx.power sec0 LocalNetwork Network)) :
   ((EqGen.variable $ XIdx.eta sec0 LocalNetwork Network) =.=
     EqGen.liftF lookupEtaTransformerHL (EqGen.variable $ XIdx.power sec0 LocalNetwork Network)) :
    (XIdx.eta sec0 Network Rest .= 1.0) :
   (XIdx.eta sec0 LocalNetwork LocalRest .= 1.0) :

   -- Average Section 1 discharging
   (XIdx.eta sec1 Network Rest .= 1.0) :
   (XIdx.eta sec1 LocalNetwork LocalRest .= 1.0) :
   (XIdx.eta sec1 Network LocalNetwork .= 0.8) :
   (XIdx.eta sec1 Coal Network .= 0.4) :
   (XIdx.eta sec1 Gas LocalNetwork .= 0.4) :
   (XIdx.eta sec1 Water Network .= 0.4) :
   (XIdx.eta sec1 Network Water .= 0.4) :
--   (XIdx.x sec1 Network Coal .= 0.7) :
   (XIdx.x sec1 Network Water .= 0.7) :
--   (XIdx.x sec1 LocalNetwork Gas .= 0) :
   (XIdx.x sec1 LocalNetwork Network .= 1.0) :
   (XIdx.x sec1 Network LocalNetwork .= 0.5) :

   []

givenDischarging :: (Double -> Double) ->                          
                            (Double -> Double) ->                        
                            (Double -> Double) ->  
                            (Double -> Double) ->  
                            Double -> 
                            Double -> 
                            Double -> 
                            Double -> 
                            EqGen.EquationSystem System.Node s Double Double
givenDischarging lookupEtaWaterDisCharge lookupEtaCoal lookupEtaGas lookupEtaTransformerHL pRest pRestLocal pWater pGas =
   (commonGiven <>) $
   mconcat $

   -- Actual Section 1 discharging to be varied and optimised
   (XIdx.power sec1 Rest Network .= pRest) :
   (XIdx.power sec1 LocalRest LocalNetwork .= pRestLocal) :
   (XIdx.power sec1 Network Water .= pWater) :
   (XIdx.power sec1 Gas LocalNetwork .= pGas) :
   ((EqGen.variable $ XIdx.eta sec1 Water Network) =.=
     EqGen.liftF lookupEtaWaterDisCharge (EqGen.variable $ XIdx.power sec1 Network Water)) :
   ((EqGen.variable $ XIdx.eta sec1 Coal Network) =.=
     EqGen.liftF lookupEtaCoal (EqGen.variable $ XIdx.power sec1 Network Coal)) :
   ((EqGen.variable $ XIdx.eta sec1 Gas LocalNetwork) =.=
     EqGen.liftF lookupEtaGas (EqGen.variable $ XIdx.power sec1 Gas LocalNetwork)) :
   ((EqGen.variable $ XIdx.eta sec1 Network LocalNetwork) =.=
     EqGen.liftF lookupEtaTransformerHL (EqGen.variable $ XIdx.power sec1 LocalNetwork Network)) :
   ((EqGen.variable $ XIdx.eta sec1 LocalNetwork Network) =.=
     EqGen.liftF lookupEtaTransformerHL (EqGen.variable $ XIdx.power sec1 LocalNetwork Network)) :
   (XIdx.eta sec1 Network Rest .= 1.0) :
   (XIdx.eta sec1 LocalNetwork LocalRest .= 1.0) :

   -- Average Section 0 discharging
   (XIdx.eta sec0 Network Rest .= 1.0) :
   (XIdx.eta sec0 LocalNetwork LocalRest .= 1.0) :
   (XIdx.eta sec0 Coal Network .= 0.4) :
   (XIdx.eta sec0 Gas LocalNetwork .= 0.4) :
   (XIdx.eta sec0 Water Network .= 0.4) :
   (XIdx.eta sec0 Network Water .= 0.4) :
   (XIdx.eta sec0 Network LocalNetwork .= 0.8) :
   (XIdx.x sec0 Network Water .= 0.2) :
   (XIdx.x sec0 Coal Network .= 0.5) :
   (XIdx.x sec0 LocalNetwork Network .= 0.8) :
   (XIdx.x sec0 Network LocalNetwork .= 0.5) :

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
etaGiven mnode mstr = Fold.fold $ M.mapWithKey f mnode
  where f n (str, g) = maybe mempty eq (M.lookup str mstr)
          where eq etaFunc =
                  EqGen.variable n =.=
                    EqGen.liftF (Data.map etaFunc) (EqGen.variable $ g n)


givenSimulate ::
 (Num a, Eq a,
  Base.BSum a, EqArith.Sum a,
  Eq (v a),
  SV.Zipper v,
  SV.Singleton v,
  SV.Walker v,
  SV.Storage v a) =>
  (TIdx.Section -> 
     M.Map (XIdx.Eta Node) (String, XIdx.Eta Node -> XIdx.Power Node)) ->
  M.Map String (a -> a) ->
  SD.SequData (Record.PowerRecord Node v a) ->
  EqGen.EquationSystem Node s (Data Nil a) (Data (v :> Nil) a)

givenSimulate mnodef mstr sf =
  (TIdx.absolute  (XIdx.storage TIdx.initial Water) EqUt..= Data 0)
   <> Fold.fold (SD.mapWithSection f sf)
   where f sec (Record.Record t xs) =
           (TIdx.absolute (XIdx.dTime sec) EqUt..= (Sig.unpack $ Sig.delta t))
           <> etaGiven (mnodef sec) mstr
          <> Fold.fold (M.mapWithKey g $ M.filterWithKey h xs)
           where 
             h (TIdx.PPos (TIdx.StructureEdge Rest Network)) _ = True
             h (TIdx.PPos (TIdx.StructureEdge LocalRest LocalNetwork)) _ = True
             h (TIdx.PPos (TIdx.StructureEdge Network Water)) _ = True
             h (TIdx.PPos (TIdx.StructureEdge LocalNetwork Gas)) _ = True
             h _ _ = False
             g (TIdx.PPos (TIdx.StructureEdge p0 p1)) e =
                   (TIdx.absolute (XIdx.energy sec p0 p1) EqUt..= Sig.unpack e)





