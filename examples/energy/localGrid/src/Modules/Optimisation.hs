{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Modules.Optimisation where

import qualified Modules.System as System
import Modules.System (
   Node (
      Coal, Gas, Water,
      Network, LocalNetwork,
      Rest, LocalRest
   ))

import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.Utility as AppUt

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Flow.SequenceState.Variable as Var
import EFA.Equation.Result (Result)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.SequenceState.Index as Idx
import EFA.Flow.Sequence.Absolute ( (.=), (=%%=), (=.=) )

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Data as Data
import qualified EFA.Signal.Vector as SV

import EFA.Signal.Data (Data(Data), Nil, (:>))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Vector as V
import qualified Data.Foldable as Fold
import Data.Monoid (mconcat, (<>))


sec0, sec1 :: Idx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ Idx.section0

type EtaAssignMap =
        Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)

type SolveFunc a =
  (Idx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  SeqFlow.Graph Node
    (Result (Data Nil a))
    (Result (Data Nil a))


ratioData :: (Arith.Constant a) => Rational -> Data Nil a
ratioData = Data . Arith.fromRational

commonGiven ::
  (Arith.Constant a, Eq a) =>
  EqSys.EquationSystemIgnore System.Node s (Data Nil a) (Data Nil a)
commonGiven =
   mconcat $
   (XIdx.dTime sec0 .= ratioData 1) :
   (XIdx.dTime sec1 .= ratioData 1) :
   (XIdx.storage Idx.initial Water .= ratioData 0) :
   (XIdx.energy sec0 Water Network =%%= XIdx.energy sec1 Water Network) :
   []

etaGiven ::
   (Ord a, Show a, Arith.Constant a,
    Data.Apply c a ~ v, Eq v, Data.ZipWith c, Data.Storage c a) =>
   EtaAssignMap ->
   Map String (a -> a) ->
   EqSys.EquationSystemIgnore Node s x (Data c a)
etaGiven etaAssign etaFunc = Fold.fold $ Map.mapWithKey f etaAssign
  where f n (strP, strN, g) =
          EqSys.variable n =.= EqSys.liftF (Data.map ef) (EqSys.variable $ g n)
          where ef x = if x >= Arith.zero then fpos x else fneg x
                fpos = maybe (err strP) id (Map.lookup strP etaFunc)
                fneg = maybe (err strN) (\h -> Arith.recip . h . Arith.negate)
                                        (Map.lookup strN etaFunc)
                err str x = error ("not defined: " ++ show str ++ " for " ++ show x)

{-
eqs ::
  (a ~ Arith.Scalar v, Eq a,
   Eq v, Arith.Product a, Arith.Product v, Arith.Integrate v) =>
  EqSys.EquationSystemIgnore Node s a v
eqs = EqSys.fromGraph True (Topo.dirFromFlowGraph (snd System.flowGraphOpt))
-}

solveCharge ::
  (Ord a, Show a, Arith.Constant a) =>
  SeqFlow.Graph Node (Result (Data Nil a)) (Result (Data Nil a)) ->
  SolveFunc a
solveCharge flowGraph etaAssign etaFunc pRest pRestLocal pWater pGas =
  EqSys.solve flowGraph
     (givenCharging etaAssign etaFunc pRest pRestLocal pWater pGas)


givenCharging ::
  (Ord a, Show a, Arith.Constant a) =>
  (Idx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  EqSys.EquationSystemIgnore System.Node s (Data Nil a) (Data Nil a)
givenCharging etaAssign etaFunc pRest pRestLocal pWater pGas =
   ((commonGiven <> etaGiven (etaAssign sec0) etaFunc) <>) $
   mconcat $
   -- Actual Section 0 Charhing to be varied and optimised
   (XIdx.power sec0 Rest Network .= pRest) :
   (XIdx.power sec0 LocalRest LocalNetwork .= pRestLocal) :
   (XIdx.power sec0 Network Water .= pWater) :
   (XIdx.power sec0 LocalNetwork Gas .= pGas) :

   -- Average Section 1 discharging
   (XIdx.eta sec1 Network Rest .= ratioData 1.0) :
   (XIdx.eta sec1 LocalNetwork LocalRest .= ratioData 1.0) :
   (XIdx.eta sec1 Network LocalNetwork .= ratioData 0.862) :
   (XIdx.eta sec1 Coal Network .= ratioData 0.345) :
   (XIdx.eta sec1 Gas LocalNetwork .= ratioData 0.346) :
   (XIdx.eta sec1 Water Network .= ratioData 0.82) :
   (XIdx.x sec1 Network Water .= ratioData 0.7) :
   (XIdx.x sec1 Network LocalNetwork .= ratioData 0.766) :
   (XIdx.x sec1 LocalNetwork Network .= ratioData 0.677) :
   []



solveDischarge ::
  (Show a, Ord a, Arith.Constant a, Arith.Integrate a) =>
  SeqFlow.Graph Node (Result (Data Nil a)) (Result (Data Nil a)) ->
  SolveFunc a
solveDischarge flowGraph etaAssign etaFunc pRest pRestLocal pWater pGas =
  EqSys.solve flowGraph
    (givenDischarging etaAssign etaFunc pRest pRestLocal pWater pGas)


givenDischarging ::
  (Show a, Ord a, Arith.Constant a) =>
  (Idx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  Data Nil a ->
  EqSys.EquationSystemIgnore System.Node s (Data Nil a) (Data Nil a)
givenDischarging etaAssign etaFunc pRest pRestLocal pWater pGas =
   ((commonGiven <> etaGiven (etaAssign sec1) etaFunc) <>) $
   mconcat $
   (XIdx.power sec1 Rest Network .= pRest) :
   (XIdx.power sec1 LocalRest LocalNetwork .= pRestLocal) :
   (XIdx.power sec1 Network Water .= pWater) :
   (XIdx.power sec1 LocalNetwork Gas .= pGas) :

   (XIdx.eta sec0 Coal Network .= ratioData 0.440) :
   (XIdx.eta sec0 Gas LocalNetwork .= ratioData 0.303) :
   (XIdx.eta sec0 Network Water .= ratioData 0.331) :
   (XIdx.eta sec0 Network LocalNetwork .= ratioData 0.939) :
   (XIdx.eta sec0 Network Rest .= ratioData 1) :
   (XIdx.eta sec0 LocalNetwork LocalRest .= ratioData 1.0) :

   (XIdx.x sec0 Network Water .= ratioData 0.054) :
   (XIdx.x sec0 Coal Network .= ratioData 1) :
   (XIdx.x sec0 Network LocalNetwork .= ratioData 0.669) :
   (XIdx.x sec0 LocalNetwork Network .= ratioData 0.881) :
   []

givenSimulate ::
 (Show a, Ord a, Arith.Constant a,
  Eq (v a),
  SV.Zipper v,SV.FromList v,SV.Len (v a),
  SV.Singleton v,
  SV.Walker v,
  SV.Storage v a) =>
  (Idx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
  Sequ.List (Record.PowerRecord Node v a) ->
  EqSys.EquationSystemIgnore Node s (Data Nil a) (Data (v :> Nil) a)

givenSimulate etaAssign etaFunc sf =
  (XIdx.storage Idx.initial Water .= ratioData 0)
   <> Fold.fold (Sequ.mapWithSection f sf)
   where f sec (Record.Record t xs) =
           (XIdx.dTime sec .=
             (Data $ SV.fromList $ replicate (Sig.len t) $ Arith.fromRational 1))
           <> etaGiven (etaAssign sec) etaFunc
           <> Fold.fold
                (Map.mapWithKey (\ppos p ->
                  XIdx.powerFromPPos sec ppos .= Sig.unpack p) xs)


-- | Avoid invalid solution by assigning NaN, which hits last in maximum
-- b == True -> Charge
-- b == False -> Discharge

type
   EnvDouble =
      SeqFlow.Graph Node (Result Double) (Result Double)


lookupDetPower ::
  XIdx.Power Node -> SeqFlow.Graph Node b (Result Double) -> Double
lookupDetPower idx =
  AppUt.checkDetermined ("lookupDetPower determined: " ++ show idx) .
  Var.checkedLookup ("lookupDetPower lookup: " ++ show idx)
    SeqFlow.lookup idx

lookupDetEnergy ::
  XIdx.Energy Node -> SeqFlow.Graph Node b (Result Double) -> Double
lookupDetEnergy idx =
  AppUt.checkDetermined ("lookupDetEnergy determined: " ++ show idx) .
  Var.checkedLookup ("lookupDetEnergy lookup: " ++ show idx)
    SeqFlow.lookup idx


-----------------------------------------------------------------------------

data SocDrive a = NoDrive
                | ChargeDrive a
                | DischargeDrive a deriving (Show, Eq)



forcing :: SocDrive Double -> SeqFlow.Graph Node b (Result Double) -> Double
forcing socDrive env =
  case socDrive of
       NoDrive -> 0
       ChargeDrive soc -> soc * eCharge
       DischargeDrive soc -> soc * negate eDischarge
  where eCharge    = lookupDetEnergy (XIdx.energy sec0 Water Network) env
        eDischarge = lookupDetEnergy (XIdx.energy sec1 Water Network) env

-----------------------------------------------------------------------------


condition :: SeqFlow.Graph Node b (Result Double) -> Bool
condition env = all (>0) [eCoal0, eCoal1, eTrans0, eTrans1]
  where eCoal0     = lookupDetEnergy (XIdx.energy sec0 Coal Network) env
        eCoal1     = lookupDetEnergy (XIdx.energy sec1 Coal Network) env
        eTrans0    = lookupDetEnergy (XIdx.energy sec0 Network LocalNetwork) env
        eTrans1    = lookupDetEnergy (XIdx.energy sec1 Network LocalNetwork) env

-----------------------------------------------------------------------------

maxEta ::
  Sig.UTSignal2 V.Vector V.Vector EnvDouble ->
  Maybe (Double, EnvDouble)
maxEta = Sweep.optimalSolution2D condition (forcing NoDrive)
