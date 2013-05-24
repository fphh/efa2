{-# LANGUAGE TypeOperators #-}

module Modules.Optimisation where

import qualified Modules.System as System
import Modules.System (Node(..))

import qualified EFA.Example.Absolute as EqGen
--import qualified EFA.Example.Absolute as EqAbs
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Example.Index as XIdx
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
-- import qualified EFA.Signal.Data as Data

import EFA.Equation.Result (Result(..))
--import EFA.Utility.Map (checkedLookup)
import EFA.Signal.Data (Data(..), Nil, (:>))

import qualified EFA.Utility.Stream as Stream

import Data.Monoid (mconcat, (<>))
import EFA.Example.Absolute ( (.=), (%=), (=.=) )
import EFA.Utility.Stream (Stream((:~)))
import qualified Data.Foldable as Fold 
import qualified Data.Map as Map


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
{-
givenSimulate :: SD.SequData (Record.PowerRecord node v a) -> EqGen.EquationSystem System.Node s (Data Nil Double) (Data (v1 :> Nil) Double)
givenSimulate sf  = 
   (TIdx.Record TIdx.Absolute (XIdx.storage TIdx.initial System.Water) .= 0)
   <> Fold.fold (SD.mapWithSection f sf)
   where f sec (Record.Record t xs) =
           (TIdx.Record TIdx.Absolute (TIdx.InSection sec TIdx.DTime) .= (Sig.unpack $ Sig.delta t)) <>
           Fold.fold (Map.mapWithKey g xs)
           where g (TIdx.PPos p) e =
                    TIdx.Record TIdx.Absolute (TIdx.InSection sec (TIdx.Energy p)) .= Sig.unpack e
-}
   {-        mconcat (
             ((EqGen.variable $ XIdx.eta sec Water Network) =.=
              EqGen.liftF lookupEtaWaterDisCharge (EqGen.variable $ XIdx.power sec Network Water)) :
             ((EqGen.variable $ XIdx.eta sec Coal Network) =.=
              EqGen.liftF lookupEtaCoal (EqGen.variable $ XIdx.power sec Network Coal)) :
             ((EqGen.variable $ XIdx.eta sec Gas LocalNetwork) =.=
              EqGen.liftF lookupEtaGas (EqGen.variable $ XIdx.power sec Gas LocalNetwork)) :
             ((EqGen.variable $ XIdx.eta sec Network LocalNetwork) =.=
              EqGen.liftF lookupEtaTransformerHL (EqGen.variable $ XIdx.power sec LocalNetwork Network)) :
             ((EqGen.variable $ XIdx.eta sec LocalNetwork Network) =.=
              EqGen.liftF lookupEtaTransformerHL (EqGen.variable $ XIdx.power sec LocalNetwork Network)) :
             (XIdx.eta sec Network Rest .= 1.0) :
             (XIdx.eta sec LocalNetwork LocalRest .= 1.0) : []) <> -}
