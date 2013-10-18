{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Modules.System as System
import qualified Modules.Optimisation as Optimisation


import Modules.Optimisation (EnvResult, EnvResultData, Param2)
import Modules.System (Node)

import qualified EFA.Application.OneStorage as One
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.Optimisation as AppOpt
import qualified EFA.Application.Simulation as AppSim
import qualified EFA.Application.Plot as PlotIO
import qualified EFA.Application.Utility as AppUt

import qualified EFA.Flow.Topology.Record as TopoRecord
import qualified EFA.Flow.Topology.Quantity as FlowTopo

import qualified EFA.Flow.Sequence.Absolute as EqAbs
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Record as RecSeq
import qualified EFA.Flow.Sequence.Index as SeqIdx
import EFA.Flow.Sequence.Absolute ((.=))

import qualified EFA.Flow.State.Index as StateIdx
import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.State.Absolute as StateEqAbs

import qualified EFA.Flow.Draw as Draw

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Signal.Signal as Sig; import EFA.Signal.Signal (TC,Scalar)
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Chop as Chop
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.Signal.Vector as Vec
import qualified EFA.Signal.Base as Base

import EFA.Signal.Data (Data(Data), Nil, (:>))
import EFA.Signal.Typ (Typ, F, T, A, Tt)

import qualified EFA.IO.TableParser as Table

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Variable as Var
import EFA.Equation.Result (Result)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Vector as V
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty

import Data.NonEmpty ((!:))
import Data.Monoid (mempty)
import Data.Tuple.HT (fst3, snd3, thd3)
import Data.Foldable (foldMap)

import Control.Functor.HT (for)
import Control.Monad (void, (>=>))


frameOpts ::
  Opts.T (Graph3D.T Double Double Double) ->
  Opts.T (Graph3D.T Double Double Double)
frameOpts = id
{-
--  Plot.heatmap .
  Plot.xyzrange3d (0.2, 2) (0.3, 3.3) (0, 1) .
  -- Plot.cbrange (0.2, 1) .
  Plot.xyzlabel "Load I Power [W]" "Load II Power [W]" "" .
  Plot.paletteGH
-}

noLegend :: Int -> String
noLegend =  (const "")

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"

scaleTableEta :: Map String (Double, Double)
scaleTableEta = Map.fromList $
  ("storage",     (3, 1)) :
  ("gas",         (3, 0.4)) :
  ("transformer", (3.0, 0.95)) :
  ("coal",        (10, 0.46)) :
  ("local",       (1, 1)) :
  ("rest",        (1, 1)) :
  []

restScale, localScale :: Double
restScale = 1.0
localScale = 1.0

------------------------------------------------------------------------

local, rest, water, gas :: [Double]
{-
local = [0.1, 0.5]
rest =  [0.2, 0.6]
water = [0.3, 0.7]
gas =   [0.4, 0.8]
-}
{-
local = [0.2, 0.7, 1.0, 1.9, 3]
rest =  [0.2, 0.7, 0.8, 1.9, 3]
water = [0.2, 0.3, 0.9, 1.9, 3]
gas =   [0.2, 0.7, 1.1, 2.7, 3]
-}

local = [0.2, 1, 2, 3]
rest =  [0.2, 1, 2, 3]
water = [0.3, 0.7, 3]
gas =   [0.4, 1, 2, 3]

sweepPts :: Sweep.Points Param2 Param2 Double
sweepPts = Sweep.Pair (local !: rest !: Empty.Cons) (water !: gas !: Empty.Cons)

optimalPower :: One.OptimalPower Node
optimalPower =
  One.optimalPower $
    (Optimisation.state0, lst) :
    (Optimisation.state1, lst) :
    (Optimisation.state2, lst) :
    (Optimisation.state3, lst) : []
  where lst =
           [SeqIdx.ppos System.Network System.Water,
            SeqIdx.ppos System.LocalNetwork System.Gas]

force :: One.SocDrive Double
force = One.ChargeDrive 0

initStorage :: (Arith.Constant a) => [(Node, a)]
initStorage = [(System.Water, Arith.fromRational $ 0.7*3600*1000)]

unzipMap :: Map k (a, b) -> (Map k a, Map k b)
unzipMap m = (Map.map fst m, Map.map snd m)

unzip3Map :: Map k (a, b, c) -> (Map k a, Map k b, Map k c)
unzip3Map m = (Map.map fst3 m, Map.map snd3 m, Map.map thd3 m)



-- @HT Hier wollen wir unabhaengig von Node und Double werden
-- also z.B. nur Typvariablen node und v sollen vorkommen.
optimalEtasWithPowers ::
  One.OptimalEnvParams Node Param2 Param2 Double ->
  One.SocDrive Double ->
  EnvResultData Double ->
  One.OptimalEtaWithEnv Node Param2 Double
optimalEtasWithPowers params forceFactor env =
  Map.mapWithKey f op
  where op = One.optimalPowers params
        forcing = One.noforcing forceFactor
        etaMap = One.etaMap params

        f :: Idx.State ->
             [Idx.PPos Node] ->
             Map (Idx.PPos Node) (Map (Param2 Double) (Double, Double))
        f state = Map.fromList . map (\p -> (p, h p optEtaEnv))
          where h ppos =
                   Map.mapMaybe (fmap (fmap
                      (AppUt.checkDetermined "optimalEtasWithPowers" .
                       Var.checkedLookup "optimalEtasWithPowers"
                       StateFlow.lookupPower
                          (StateIdx.powerFromPPos state ppos))))

                solveFunc :: Optimisation.Param2x2 Double -> EnvResult Double
                solveFunc =
                  Optimisation.solve env
                    System.etaAssignMap
                    etaMap
                    state

                envsSweep :: Map (Param2 Double) [EnvResult Double]
                envsSweep =
                  Sweep.doubleSweep solveFunc (One.points params)

                optEtaEnv = for envsSweep $
                  Sweep.optimalSolutionState
                    --One.nocondition
                    Optimisation.condition
                    forcing

------------------------------------------------------------------------


envToPowerRecord ::
  FlowTopo.Section System.Node (Result (Data ([] :> Nil) Double)) ->
  Record.PowerRecord System.Node [] Double
envToPowerRecord =
  Chop.addZeroCrossings .
  TopoRecord.sectionToPowerRecord .
  FlowTopo.mapSection (AppUt.checkDetermined "envToPowerRecord")


external ::
  (Eq (v a), Arith.Constant a, Base.BSum a, Vec.Zipper v,
  Vec.Walker v, Vec.Singleton v, Vec.Storage v a, Node.C node) =>
  [(node, a)] ->
  SeqFlow.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a)) ->
  SeqFlow.Graph node (Result (Data Nil a)) (Result (Data (v :> Nil) a))

external initSto flowGraph =
  EqAbs.solveOpts (EqAbs.independentInOutSums EqAbs.optionsDefault) flowGraph $
  foldMap f initSto
  where f (st, val) =
          SeqIdx.storage Idx.initial st .= Data val

varRestPower', varLocalPower' :: [[Double]]
(varLocalPower', varRestPower') = CT.varMat local rest

restSig :: Sig.PSignal V.Vector Double
restSig = Sig.fromList rest

varRestPower :: Sig.PSignal2 V.Vector V.Vector Double
varRestPower = Sig.fromList2 varRestPower'

varLocalPower :: Sig.PSignal2 V.Vector V.Vector Double
varLocalPower = Sig.fromList2 varLocalPower'



to2DMatrix ::
  (Vec.Storage v1 a, Vec.Storage v2 (v1 a),
   Vec.FromList v1, Vec.FromList v2, Ord b) =>
  Map (NonEmpty.T f b) a ->
  TC tr (Typ x y z) (Data (v2 :> v1 :> Nil) a)
to2DMatrix =
  Sig.fromList2 . Map.elems .
  Map.mapKeysWith (++) NonEmpty.head . fmap (:[])

optimalMaps :: (Num a, Ord a, Ord (f a), Ord node) =>
  Map Idx.State (Map (Idx.PPos node) (Map (NonEmpty.T f a) (a, a))) ->
  ( Sig.NSignal2 V.Vector V.Vector a,
    Sig.UTSignal2 V.Vector V.Vector Idx.State,
    Map (Idx.PPos node) (Sig.PSignal2 V.Vector V.Vector a) )
optimalMaps =
  (\(eta, st, power) -> (head $ Map.elems eta, head $ Map.elems st, power))
  . unzip3Map
  . Map.map (h . unzip3Map)
  . Map.unionsWith (Map.unionWith max)
  . Map.elems
  . Map.mapWithKey f
  where f st = Map.map (Map.map (\(eta, power) -> (eta, st, power)))
        h (eta, st, power) =
          (to2DMatrix eta, to2DMatrix st, to2DMatrix power)


givenSignals ::
  Ord node =>
  Sig.TSignal [] Double ->
  Map (Idx.PPos node) (Sig.PSignal [] Double) ->
  Record.PowerRecord node [] Double
givenSignals time =
  Chop.addZeroCrossings . Record.Record time

solveAndCalibrateAvgEffWithGraph ::
  Sig.TSignal [] Double ->
  Sig.PSignal [] Double ->
  Sig.PSignal [] Double ->
  Map String (Double -> Double) ->
  EnvResultData Double ->
  IO (EnvResultData Double)
solveAndCalibrateAvgEffWithGraph time prest plocal etaMap stateFlowGraph = do
  let sectionFilterTime ::  TC Scalar (Typ A T Tt) (Data Nil Double)
      sectionFilterTime = Sig.toScalar 0

      sectionFilterEnergy ::  TC Scalar (Typ A F Tt) (Data Nil Double)
      sectionFilterEnergy = Sig.toScalar 0

      optParams =
        One.OptimalEnvParams
          etaMap
          sweepPts
          optimalPower
      optEtaWithPowers ::
        Map Idx.State
          (Map (Idx.PPos Node)
            (Map (Param2 Double) (Double, Double)))
      optEtaWithPowers = optimalEtasWithPowers optParams force stateFlowGraph
      (_optEta, _optState, optPower) = optimalMaps optEtaWithPowers

      optPowerInterp ::
        Map (Idx.PPos Node) (Sig.PSignal [] Double)
      optPowerInterp =
        for optPower $ \powerStateOpt ->
          Sig.tzipWith
            (Sig.interp2WingProfile "solveAndCalibrateAvgEffWithGraph"
              restSig varLocalPower powerStateOpt)
            prest plocal


      givenSigs :: Record.PowerRecord Node [] Double
      givenSigs =
        givenSignals time $
        Map.union optPowerInterp $
        Map.fromList $
          (SeqIdx.ppos System.LocalRest System.LocalNetwork, plocal) :
          (SeqIdx.ppos System.Rest System.Network, prest) :
          []

      envSims :: FlowTopo.Section Node (Result (Data ([] :> Nil) Double))
      envSims =
        AppSim.solve
          System.topology
          System.etaAssignMap
          etaMap
          givenSigs

      recZeroCross = envToPowerRecord envSims

      sequencePowers :: Sequ.List (Record.PowerRecord System.Node [] Double)
      sequencePowers = Chop.genSequ recZeroCross

      sequenceFlowsFilt :: Sequ.List (Record.FlowRecord Node [] Double)
      sequenceFlowsFilt =
        Sequ.filter (Record.major sectionFilterEnergy sectionFilterTime) $
        fmap Record.partIntegrate sequencePowers

      sequenceFlowGraph ::
        SeqFlow.Graph Node
          (Result (Data Nil Double)) (Result (Data ([] :> Nil) Double))
      sequenceFlowGraph =
        RecSeq.flowGraphFromSequence $
        fmap (TopoRecord.flowTopologyFromRecord System.topology) $
        sequenceFlowsFilt

      stateFlowEnvWithGraph :: EnvResultData Double
      stateFlowEnvWithGraph =
        StateEqAbs.solveOpts
          (EqAbs.equalInOutSums EqAbs.optionsDefault)
          (StateFlow.flowGraphFromCumResult $
           StateFlow.fromSequenceFlowResult True $
           SeqFlow.mapGraph id (fmap Arith.integrate) $
           external initStorage sequenceFlowGraph)
          mempty

  --Draw.xterm $ Draw.stateFlowGraph (fst stateFlowEnvWithGraph)

  PlotIO.record "Calculated Signals" DefaultTerm.cons show id recZeroCross

  Draw.xterm $ Draw.stateFlowGraph Draw.optionsDefault stateFlowEnvWithGraph
  return stateFlowEnvWithGraph


nestM :: (Monad m) => Int -> (a -> m a) -> a -> m a
nestM n act = foldr (>=>) return (replicate n act)

initEnv :: EnvResultData Double
initEnv = AppOpt.initialEnv System.stateFlowGraph

main :: IO()
main = do

  -- | Import Maps and power demand profiles

  tabEta <- Table.read "../maps/eta.txt"
  tabPower <- Table.read "../maps/power.txt"

  let etaMap = CT.makeEtaFunctions2D scaleTableEta tabEta


      (time,
       NonEmpty.Cons pwind
          (NonEmpty.Cons psolar
             (NonEmpty.Cons phouse
                (NonEmpty.Cons pindustry Empty.Cons)))) =
        CT.getPowerSignalsWithSameTime tabPower
          ("wind" !: "solar" !: "house" !: "industry" !: Empty.Cons)

      prest = Sig.scale restScale pwind
      plocal = Sig.offset 0.4 $ Sig.scale localScale $
        psolar Sig..+ Sig.makeDelta phouse Sig..+ Sig.makeDelta pindustry

  void $
     nestM 10
        (solveAndCalibrateAvgEffWithGraph time prest plocal etaMap)
        (AppOpt.initialEnv System.stateFlowGraph)
