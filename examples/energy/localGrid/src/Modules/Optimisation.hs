{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Modules.Optimisation where

import qualified Modules.System as System
import Modules.System (Node(..))

import qualified EFA.Application.Absolute as EqGen
import qualified EFA.Application.Index as XIdx
import qualified EFA.Application.Utility as EqUt
import qualified EFA.Application.EtaSys as ES
import EFA.Application.Absolute ( (.=), (=%%=), (=.=) )

import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Arithmetic as EqArith
import EFA.Equation.Result (Result(..))

import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Flow as Flow

import qualified EFA.Application.Utility as AppUt

import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Data as Data
import qualified EFA.Signal.Base as Base
import qualified EFA.Signal.Vector as SV

import EFA.Signal.Data (Data(..), Nil, (:>))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Vector as V
import qualified Data.Foldable as Fold
import Data.Monoid (mconcat, (<>))

import Control.Applicative (liftA2)


sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0

type EtaAssignMap =
        Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)

type SolveFunc a =
  (TIdx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  EqEnv.Complete
    Node
    (Result (Data Nil a))
    (Result (Data Nil a))

commonGiven ::
  (EqArith.Sum a, Num a, Eq a) =>
  EqGen.EquationSystem System.Node s (Data Nil a) (Data Nil a)
commonGiven =
   mconcat $
   (XIdx.dTime sec0 .= Data 1) :
   (XIdx.dTime sec1 .= Data 1) :
   (XIdx.storage TIdx.initial Water .= Data 0) :
   (XIdx.energy sec0 Water Network =%%= XIdx.energy sec1 Water Network) :
   []

etaGiven ::
   (Fractional a, Ord a, Show a, EqArith.Sum a,
    Data.Apply c a ~ v, Eq v, Data.ZipWith c, Data.Storage c a) =>
   EtaAssignMap ->
   Map String (a -> a) ->
   EqGen.EquationSystem Node s x (Data c a)
etaGiven etaAssign etaFunc = Fold.fold $ Map.mapWithKey f etaAssign
  where f n (strP, strN, g) =
          EqGen.variable n =.= EqGen.liftF (Data.map ef) (EqGen.variable $ g n)
          where ef x = if x >= 0 then fpos x else fneg x
                fpos = maybe (err strP) id (Map.lookup strP etaFunc)
                fneg = maybe (err strN) (\h -> recip . h . negate)
                                        (Map.lookup strN etaFunc)
                err str x = error ("not defined: " ++ show str ++ " for " ++ show x)

{-
eqs ::
  (a ~ EqArith.Scalar v, Eq a,
   Eq v, EqArith.Product a, EqArith.Product v, EqArith.Integrate v) =>
  EqGen.EquationSystem Node s a v
eqs = EqGen.fromGraph True (TD.dirFromSequFlowGraph (snd System.seqTopoOpt))
-}

solveCharge ::
  (Ord a, Fractional a, Show a, EqArith.Sum a) =>
  (forall s. EqGen.EquationSystem Node s (Data Nil a) (Data Nil a)) ->
  SolveFunc a
solveCharge eqs etaAssign etaFunc pRest pRestLocal pWater pGas =
  EqGen.solveSimple $
    eqs <> givenCharging etaAssign etaFunc pRest pRestLocal pWater pGas

givenCharging ::
  (Ord a, Fractional a, Show a, EqArith.Sum a) =>
  (TIdx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
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
  ( Eq a, Show a, EqArith.Product a, EqArith.Integrate a, Ord a,
    Fractional a, EqArith.Scalar (Data Nil a) ~ Data Nil a) =>
  (forall s. EqGen.EquationSystem Node s (Data Nil a) (Data Nil a)) ->
  SolveFunc a
solveDischarge eqs etaAssign etaFunc pRest pRestLocal pWater pGas =
  EqGen.solveSimple $
    eqs <> givenDischarging etaAssign etaFunc pRest pRestLocal pWater pGas


givenDischarging ::
  (Eq a, Num a, Show a, EqArith.Sum a, Fractional a,Ord a) =>
  (TIdx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
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
  (TIdx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
  SD.SequData (Record.PowerRecord Node v a) ->
  EqGen.EquationSystem Node s (Data Nil a) (Data (v :> Nil) a)

givenSimulate etaAssign etaFunc sf =
  (TIdx.absolute  (XIdx.storage TIdx.initial Water) EqUt..= Data 0)
   <> Fold.fold (SD.mapWithSection f sf)
   where f sec (Record.Record t xs) =
           (TIdx.absolute (XIdx.dTime sec) EqUt..=
             (Data  $ SV.fromList $ replicate (Sig.len t) 1))
           <> etaGiven (etaAssign sec) etaFunc
           <> Fold.fold (Map.mapWithKey g xs)
           where
             g (TIdx.PPos (TIdx.StructureEdge p0 p1)) p =
                   (TIdx.absolute (XIdx.power sec p0 p1) EqUt..= Sig.unpack p)


-- | Avoid invalid solution by assigning NaN, which hits last in maximum
-- b == True -> Charge
-- b == False -> Discharge

type
   EnvDouble =
      EqEnv.Complete Node (Result Double) (Result Double)


lookupDetPower ::
  XIdx.Power Node -> EqEnv.Complete Node b (Result Double) -> Double
lookupDetPower idx =
  AppUt.checkDetermined ("lookupDetPower determined: " ++ show idx) .
  flip (ES.lookupAbsPower ("lookupDetPower lookup: " ++ show idx)) idx

lookupDetEnergy ::
  XIdx.Energy Node -> EqEnv.Complete Node b (Result Double) -> Double
lookupDetEnergy idx =
  AppUt.checkDetermined ("lookupDetEnergy determined: " ++ show idx) .
  flip (ES.lookupAbsEnergy ("lookupDetEnergy lookup: " ++ show idx)) idx


-----------------------------------------------------------------------------

data SocDrive a = NoDrive
                | ChargeDrive a
                | DischargeDrive a deriving (Show, Eq)


type Penalty = EnvDouble -> Double

penalty :: SocDrive Double -> EqEnv.Complete Node b (Result Double) -> Double
penalty socDrive env =
  case socDrive of
       NoDrive -> 0
       ChargeDrive soc -> soc * eCharge
       DischargeDrive soc -> soc * negate eDischarge
  where eCharge    = lookupDetEnergy (XIdx.energy sec0 Water Network) env
        eDischarge = lookupDetEnergy (XIdx.energy sec1 Water Network) env

-----------------------------------------------------------------------------


type Condition = EnvDouble -> Bool

condition :: EqEnv.Complete Node b (Result Double) -> Bool
condition env = all (>0) [eCoal0, eCoal1, eTrans0, eTrans1]
  where eCoal0     = lookupDetEnergy (XIdx.energy sec0 Coal Network) env
        eCoal1     = lookupDetEnergy (XIdx.energy sec1 Coal Network) env
        eTrans0    = lookupDetEnergy (XIdx.energy sec0 Network LocalNetwork) env
        eTrans1    = lookupDetEnergy (XIdx.energy sec1 Network LocalNetwork) env

-----------------------------------------------------------------------------

maxEta ::
  Flow.RangeGraph Node ->
  Sig.UTSignal2 V.Vector V.Vector EnvDouble ->
  Maybe (Double, EnvDouble)
maxEta topo sigEnvs =
  optimalSolution condition (penalty NoDrive) topo sigEnvs


------------------------------------------------------------------------------

-- soll z.B. in EtaSys kommen:

optimalSolution ::
  Condition ->
  Penalty ->
  Flow.RangeGraph Node ->
  Sig.UTSignal2 V.Vector V.Vector EnvDouble ->
  Maybe (Double, EnvDouble)
optimalSolution cond penalty topo sigEnvs = liftA2 (,) etaMax env
  where etaSys = Sig.map go sigEnvs
        go env =
          case cond env of 
               True -> Just $ ES.detEtaSys topo env + penalty env
               False -> Nothing
        etaMax = Sig.fromScalar $ Sig.maximum etaSys
        (xIdx, yIdx) = Sig.findIndex2 (== etaMax) etaSys
        env = liftA2 (Sig.getSample2D sigEnvs) xIdx yIdx

