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

import qualified EFA.Equation.Arithmetic as EqArith
import qualified EFA.Equation.Variable as Var
import EFA.Equation.Result (Result)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import EFA.Flow.Sequence.Absolute ( (.=), (=%%=), (=.=) )

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Data as Data
import qualified EFA.Signal.Base as Base
import qualified EFA.Signal.Vector as SV

import EFA.Signal.Data (Data(Data), Nil, (:>))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Vector as V
import qualified Data.Foldable as Fold
import Data.Monoid (mconcat, (<>))


sec0, sec1 :: Idx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ Idx.Section 0

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

commonGiven ::
  (EqArith.Sum a, Num a, Eq a) =>
  EqSys.EquationSystemIgnore System.Node s (Data Nil a) (Data Nil a)
commonGiven =
   mconcat $
   (XIdx.dTime sec0 .= Data 1) :
   (XIdx.dTime sec1 .= Data 1) :
   (XIdx.storage Idx.initial Water .= Data 0) :
   (XIdx.energy sec0 Water Network =%%= XIdx.energy sec1 Water Network) :
   []

etaGiven ::
   (Fractional a, Ord a, Show a, EqArith.Sum a,
    Data.Apply c a ~ v, Eq v, Data.ZipWith c, Data.Storage c a) =>
   EtaAssignMap ->
   Map String (a -> a) ->
   EqSys.EquationSystemIgnore Node s x (Data c a)
etaGiven etaAssign etaFunc = Fold.fold $ Map.mapWithKey f etaAssign
  where f n (strP, strN, g) =
          EqSys.variable n =.= EqSys.liftF (Data.map ef) (EqSys.variable $ g n)
          where ef x = if x >= 0 then fpos x else fneg x
                fpos = maybe (err strP) id (Map.lookup strP etaFunc)
                fneg = maybe (err strN) (\h -> recip . h . negate)
                                        (Map.lookup strN etaFunc)
                err str x = error ("not defined: " ++ show str ++ " for " ++ show x)

{-
eqs ::
  (a ~ EqArith.Scalar v, Eq a,
   Eq v, EqArith.Product a, EqArith.Product v, EqArith.Integrate v) =>
  EqSys.EquationSystemIgnore Node s a v
eqs = EqSys.fromGraph True (Topo.dirFromFlowGraph (snd System.seqTopoOpt))
-}

solveCharge ::
  (Ord a, Fractional a, Show a, EqArith.Constant a) =>
  SeqFlow.Graph Node (Result (Data Nil a)) (Result (Data Nil a)) ->
  SolveFunc a
solveCharge flowGraph etaAssign etaFunc pRest pRestLocal pWater pGas =
  EqSys.solve flowGraph
     (givenCharging etaAssign etaFunc pRest pRestLocal pWater pGas)

givenCharging ::
  (Ord a, Fractional a, Show a, EqArith.Sum a) =>
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
  (Show a, Ord a, Fractional a,
   EqArith.Constant a, EqArith.Integrate a) =>
  SeqFlow.Graph Node (Result (Data Nil a)) (Result (Data Nil a)) ->
  SolveFunc a
solveDischarge flowGraph etaAssign etaFunc pRest pRestLocal pWater pGas =
  EqSys.solve flowGraph
    (givenDischarging etaAssign etaFunc pRest pRestLocal pWater pGas)


givenDischarging ::
  (Show a, Ord a, Fractional a, EqArith.Sum a) =>
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
  (Idx.Section -> EtaAssignMap) ->
  Map String (a -> a) ->
  Sequ.List (Record.PowerRecord Node v a) ->
  EqSys.EquationSystemIgnore Node s (Data Nil a) (Data (v :> Nil) a)

givenSimulate etaAssign etaFunc sf =
  (XIdx.storage Idx.initial Water .= Data 0)
   <> Fold.fold (Sequ.mapWithSection f sf)
   where f sec (Record.Record t xs) =
           (XIdx.dTime sec .=
             (Data  $ SV.fromList $ replicate (Sig.len t) 1))
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
    SeqFlow.lookupPower idx

lookupDetEnergy ::
  XIdx.Energy Node -> SeqFlow.Graph Node b (Result Double) -> Double
lookupDetEnergy idx =
  AppUt.checkDetermined ("lookupDetEnergy determined: " ++ show idx) .
  Var.checkedLookup ("lookupDetEnergy lookup: " ++ show idx)
    SeqFlow.lookupEnergy idx


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
maxEta = Sweep.optimalSolution2DNew condition (forcing NoDrive)
