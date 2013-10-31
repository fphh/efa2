{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Main where

import qualified Modules.System as System
import qualified Modules.Optimisation as Optimisation
import qualified Modules.Analysis as Analysis
import qualified Modules.Utility as ModUt
import Modules.System (
   Node (
      Coal, Gas, Water,
      Network, LocalNetwork,
      Rest, LocalRest
   ))
import Modules.Optimisation
   (EnvDouble, sec0,sec1, lookupDetPower, condition, forcing)

import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.Plot as PlotIO
import EFA.Application.Optimisation (etaOverPowerIn, etaOverPowerOut)
import EFA.Application.Utility (seqFlowGraphFromFlowTopos)

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Sequence.Record as RecSeq
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Flow.Topology.Record as TopoRecord

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.Arithmetic as EqArith
import EFA.Equation.Result (Result)

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Chop (addZeroCrossings, genSequ,)
import EFA.Signal.Signal (TC,Scalar)
import EFA.Signal.Data (Data(Data), Nil, getData, (:>))
import EFA.Signal.Typ (Typ, F, T, A, Tt) --,UT)

import EFA.Utility.Async (concurrentlyMany_)
import EFA.Report.Report(report, ROpt(RAll))

import qualified EFA.IO.TableParser as Table
import qualified EFA.IO.TableParserTypes as TPT

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
--import qualified Graphics.Gnuplot.Terminal.PNG as PNG

import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Vector as V

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import Data.NonEmpty ((!:))

import qualified System.IO as IO


-- ################### Plot Stuff

plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

colours :: [Colors.X11Color]
colours = [ Colors.White,
            Colors.Gray90,
            Colors.Gray80,
            Colors.Gray70 ]

frameOpts ::
  Opts.T (Graph3D.T Double Double Double) ->
  Opts.T (Graph3D.T Double Double Double)
frameOpts =
  Plot.heatmap .
  Plot.xyzrange3d (0.2, 2) (0.3, 3.3) (0, 1) .
  -- Plot.cbrange (0.2, 1) .
  Plot.xyzlabel "Rest Power [W]" "Local Power [W]" "" .
  Plot.paletteGH

scaleTableEta :: Map String (Double, Double)
scaleTableEta = Map.fromList $
  ("storage",     (1, 0.8)) :
  ("gas",         (1, 0.4)) :
  ("transformer", (3.0, 0.95)) :
  ("coal",        (6, 0.46)) :
  ("local",       (1, 1)) :
  ("rest",        (1, 1)) :
  []

-- ################### Efficiency Curves

etaAssign ::
  Idx.Section ->
  Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)
etaAssign sec = Map.fromList $
  (XIdx.eta sec Water Network, ( "storage", "storage", etaOverPowerIn)) :
  (XIdx.eta sec Network Water, ( "storage", "storage", etaOverPowerOut)) :

  (XIdx.eta sec Coal Network, ( "coal", "coal", etaOverPowerIn)) :
  (XIdx.eta sec Network Coal, ( "coal", "coal", etaOverPowerOut)) :

  (XIdx.eta sec Gas LocalNetwork, ( "gas", "gas", etaOverPowerIn)) :
  (XIdx.eta sec LocalNetwork Gas, ( "gas", "gas", etaOverPowerOut)) :

  (XIdx.eta sec Network LocalNetwork, ( "transformer", "transformer", etaOverPowerIn)) :
  (XIdx.eta sec LocalNetwork Network, ( "transformer", "transformer", etaOverPowerOut)) :

  (XIdx.eta sec LocalNetwork LocalRest, ( "local", "local", etaOverPowerIn)) :
  (XIdx.eta sec LocalRest LocalNetwork, ( "local", "local", etaOverPowerOut)) :

  (XIdx.eta sec Network Rest, ( "rest", "rest", etaOverPowerIn)) :
  (XIdx.eta sec Rest Network, ( "rest", "rest", etaOverPowerOut)) :

  []


-- ################### Vary Power Demand



restPower :: [Double]
-- restPower = [0.2, 0.4 .. 2]
restPower = [0.1, 0.6]

localPower :: [Double]
-- localPower = [0.3, 0.5 .. 3.3]
localPower = [0.4, 0.8]

varRestPower', varLocalPower' :: [[Double]]
(varLocalPower', varRestPower') = CT.varMat localPower restPower

restPowerScale :: Double
restPowerScale = 1

varRestPower1D :: Sig.PSignal V.Vector Double
varRestPower1D = Sig.fromList restPower

localPowerScale :: Double
-- localPowerScale = 1.2
localPowerScale = 1

varRestPower :: Sig.PSignal2 V.Vector V.Vector Double
varRestPower = Sig.fromList2 varRestPower'

varLocalPower :: Sig.PSignal2 V.Vector V.Vector Double
varLocalPower = Sig.fromList2 varLocalPower'


-- ################### Vary Degrees of Freedom for Optimisation

waterPower :: [Double]
--waterPower = [0.2,0.4 .. 0.8]
waterPower = [0.2,0.4 .. 0.8]

gasPower :: [Double]
-- gasPower = [0.2,0.4 .. 1.0]
gasPower = [0.3, 0.7]

varWaterPowerSig :: Sig.PTestRow [] Double
varWaterPowerSig = Sig.fromList waterPower

varGasPowerSig :: Sig.PTestRow [] Double
varGasPowerSig = Sig.fromList gasPower

varWaterPower', varGasPower' :: [[Double]]
(varWaterPower', varGasPower') = CT.varMat waterPower gasPower

varWaterPower :: Sig.PSignal2 V.Vector V.Vector Double
varWaterPower = Sig.fromList2 varWaterPower'

varGasPower :: Sig.PSignal2 V.Vector V.Vector Double
varGasPower = Sig.fromList2 varGasPower'


sweep ::(SV.Storage c2 d,
          SV.Storage c1 (c2 d), SV.FromList c2,
          SV.FromList c1) =>
         (a -> b -> d)
         -> [[a]] -> [[b]] -> TC s t (Data (c1 :> (c2 :> Nil)) d)
sweep func xs ys = Sig.fromList2 $ zipWith (zipWith func) xs ys

doubleSweep ::
  Optimisation.SolveFunc Double ->
  Map String (Double -> Double) ->
  [[Double]] ->
  [[Double]] ->
  [[Double]] ->
  [[Double]] ->
  Sig.UTSignal2 V.Vector V.Vector
    (Sig.UTSignal2 V.Vector V.Vector
      (SeqFlow.Graph Node
        (Result (Data Nil Double))
        (Result (Data Nil Double))))
doubleSweep func etaFunc varOptX varOptY varX varY=
  sweep f (mm varX) (mm varY)
  where f x y =
          sweep (func etaAssign etaFunc x y) xo yo
        xo = mm varOptX
        yo = mm varOptY
        mm = map (map Data)

sweepGetData ::
  Sig.UTSignal2 V.Vector V.Vector
    (Sig.UTSignal2 V.Vector V.Vector
      (SeqFlow.Graph Node
        (Result (Data Nil Double))
        (Result (Data Nil Double)))) ->
  Sig.UTSignal2 V.Vector V.Vector
    (Sig.UTSignal2 V.Vector V.Vector
      (SeqFlow.Graph Node
        (Result Double)
        (Result Double)))
sweepGetData =
  Sig.map (Sig.map (SeqFlow.mapGraph (fmap getData) (fmap getData)))

maxOptChargeFunc, maxOptDischargeFunc ::
  Double ->
  Sig.UTSignal2 V.Vector V.Vector EnvDouble ->
  (Double, EnvDouble)
maxOptChargeFunc socDrive =
  maybe (error "maxOptChargeFunc") id .
  Sweep.optimalSolution2D condition
    (forcing $ Optimisation.ChargeDrive socDrive)

maxOptDischargeFunc socDrive =
  maybe (error "maxOptDischargeFunc") id .
  Sweep.optimalSolution2D condition
    (forcing $ Optimisation.DischargeDrive socDrive)

makePics ::
  SeqFlow.Graph Node (Result (Data Nil Double)) (Result (Data Nil Double)) ->
  TPT.Map Double ->
  Double ->
  ( Sig.UTSignal2 V.Vector V.Vector Sig.ArgMax,
    Sig.PSignal2 V.Vector V.Vector Double,
    Sig.PSignal2 V.Vector V.Vector Double,
    Sig.NSignal2 V.Vector V.Vector Double )
makePics eqs tabEta socDrive = (state, optWater, optGas, etaSysMax)
  where chargeFunc = maxOptChargeFunc socDrive
        dischargeFunc = maxOptDischargeFunc socDrive

        state =
          Sig.zipArgMax maxETACharge maxETADischarge

        optWater = Sweep.combineOptimalMaps maxEtaSysState
                     powerWaterChargeOpt powerWaterDischargeOpt
        optGas = Sweep.combineOptimalMaps maxEtaSysState
                     powerGasChargeOpt powerGasDischargeOpt

        maxEtaSysState :: Sig.UTSignal2 V.Vector V.Vector Sig.ArgMax
        maxEtaSysState =
          Sig.zipArgMax maxETACharge maxETADischarge

        etaSysMax :: Sig.NSignal2 V.Vector V.Vector Double
        etaSysMax = Sig.zipWith max maxETACharge maxETADischarge

        powerWaterChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerWaterChargeOpt = Sig.setType $
          Sig.map (lookupDetPower $ XIdx.power sec0 Network Water) envsChargeOpt


        powerWaterDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerWaterDischargeOpt = Sig.setType $
          Sig.map (lookupDetPower $ XIdx.power sec1 Network Water) envsDischargeOpt

        powerGasChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerGasChargeOpt = Sig.setType $
          Sig.map (lookupDetPower $ XIdx.power sec0 LocalNetwork Gas) envsChargeOpt


        powerGasDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerGasDischargeOpt = Sig.setType $
          Sig.map (lookupDetPower $ XIdx.power sec1 LocalNetwork Gas) envsDischargeOpt

        maxETACharge :: Sig.NSignal2 V.Vector V.Vector Double
        maxETACharge = Sig.setType $ Sig.map fst $
          Sig.map chargeFunc envsCharge

        maxETADischarge :: Sig.NSignal2 V.Vector V.Vector Double
        maxETADischarge = Sig.setType $ Sig.map fst $
          Sig.map dischargeFunc envsDischarge

        envsCharge =
          sweepGetData $
          doubleSweep (Optimisation.solveCharge eqs)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

        envsDischarge =
          sweepGetData $
          doubleSweep (Optimisation.solveDischarge eqs)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

        envsChargeOpt = Sig.map snd $ Sig.map chargeFunc envsCharge

        envsDischargeOpt = Sig.map snd $ Sig.map dischargeFunc envsDischarge

        etaFunc = CT.makeEtaFunctions2D scaleTableEta tabEta

{-

main :: IO ()
main = do

   IO.hSetEncoding IO.stdout IO.utf8

   tabEta <- Table.read "../simulation/maps/eta.txt"
   tabPower <- Table.read "../simulation/maps/power.txt"

   let eqs ::
         (a ~ EqArith.Scalar v, Eq a, EqArith.Constant a,
         Eq v, EqArith.Product a, EqArith.Product v, EqArith.Integrate v) =>
         EqSys.EquationSystemIgnore Node s a v
       eqs = EqSys.fromGraph True (Topo.dirFromFlowGraph (snd System.flowGraphOpt))

       plotwaterpng n pic =
         PlotIO.surfaceWithOpts "Optimal Water Power"
           --(PNG.cons ("waterpics/optimal_water_power" ++ printf "%02d" n ++ ".png"))
           DefaultTerm.cons
           id
           frameOpts varRestPower varLocalPower
           pic


       plotgaspng n pic =
         PlotIO.surfaceWithOpts "Optimal Gas Power [W]"
           --(PNG.cons ("gaspics/optimal_gas_power" ++ printf "%02d" n ++ ".png"))
           DefaultTerm.cons
           id
           frameOpts varRestPower varLocalPower
           pic


       plotstatepng n pic =
         PlotIO.surfaceWithOpts "Optimal State"
           --(PNG.cons ("statepics/state_matrix" ++ printf "%02d" n ++ ".png"))
           DefaultTerm.cons
           id
           frameOpts varRestPower varLocalPower
           pic

       plotetasyspng n pic =
         PlotIO.surfaceWithOpts "System Efficiency [%]"
           (PNG.cons ("etasys/etasys" ++ printf "%02d" n ++ ".png"))
           id
           frameOpts varRestPower varLocalPower
           pic


       plotpng (n, x) =
         let (s, w, g, e) = makePics eqs tabEta x
         in  concurrentlyMany_ [
               -- plotstatepng n s
               plotwaterpng n w,
               plotgaspng n g ]




       (lst1, lst2) = splitAt 20 $ take 40 $ zip [0::Int, 1 ..] [0, 0.001 ..]

   concurrentlyMany_ [
     mapM_ plotpng [(lst1 ++ lst2) !! 30] ]
     -- mapM_ plotpng lst2 ]

-}


main :: IO ()
main = do

   IO.hSetEncoding IO.stdout IO.utf8

   tabEta <- Table.read "../simulation/maps/eta.txt"
   tabPower <- Table.read "../simulation/maps/power.txt"

   -- |Import Efficiency Curves
   let etaFunc = CT.makeEtaFunctions2D scaleTableEta tabEta
{-       etaWaterCharge:etaWaterDischarge:etaGas:etaCoal:etaTransHL:_ =
         getEtas etaFunc ["storage", "storage", "gas", "coal", "transformer"]-}

   -- | Import Power Curves
   let (NonEmpty.Cons powerSignalWind
         (NonEmpty.Cons powerSignalSolar
           (NonEmpty.Cons powerSignalHouse
             (NonEmpty.Cons powerSignalIndustry Empty.Cons))))
          = fmap snd $
            CT.getPowerSignals tabPower
               ("wind" !: "solar" !: "house" !: "industry" !: Empty.Cons)


       powerSignalRest = Sig.scale restPowerScale powerSignalWind
       powerSignalLocal = Sig.offset 0.5 $ Sig.scale localPowerScale $
         powerSignalSolar
         Sig..+ Sig.makeDelta powerSignalHouse
         Sig..+ Sig.makeDelta powerSignalIndustry

   let
    -- | Speep optimisation and operation space for charge and discharge case
     envsCharge ::
       Sig.UTSignal2 V.Vector V.Vector
         (Sig.UTSignal2 V.Vector V.Vector EnvDouble)
     envsCharge =
       sweepGetData $
       doubleSweep (Optimisation.solveCharge System.flowGraphOpt)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

     envsDischarge ::
       Sig.UTSignal2 V.Vector V.Vector
         (Sig.UTSignal2 V.Vector V.Vector EnvDouble)
     envsDischarge =
       sweepGetData $
       doubleSweep (Optimisation.solveDischarge System.flowGraphOpt)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

     socDrive = 0.0
     chargeFunc = maxOptChargeFunc socDrive
     dischargeFunc = maxOptDischargeFunc socDrive

     maxETACharge :: Sig.NSignal2 V.Vector V.Vector Double
     maxETACharge = Sig.setType $ Sig.map fst $
      Sig.map chargeFunc envsCharge

     maxETADischarge :: Sig.NSignal2 V.Vector V.Vector Double
     maxETADischarge = Sig.setType $ Sig.map fst $
       Sig.map dischargeFunc envsDischarge




     -- | Get maximum efficiency for both cases
     etaSysMax :: Sig.NSignal2 V.Vector V.Vector Double
     etaSysMax = Sig.zipWith max maxETACharge maxETADischarge

     maxEtaSysState :: Sig.UTSignal2 V.Vector V.Vector Sig.ArgMax
     maxEtaSysState = Sig.zipArgMax maxETACharge maxETADischarge


     -- | Get the correspondig optimal envs for both states

     envsChargeOpt :: Sig.UTSignal2 V.Vector V.Vector EnvDouble
     envsChargeOpt = Sig.map snd $ Sig.map chargeFunc envsCharge

     envsDischargeOpt :: Sig.UTSignal2 V.Vector V.Vector EnvDouble
     envsDischargeOpt = Sig.map snd $ Sig.map dischargeFunc envsDischarge


     powerGasChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerGasChargeOpt = Sig.setType $
       Sig.map (lookupDetPower $ XIdx.power sec0 LocalNetwork Gas) envsChargeOpt

     powerWaterChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerWaterChargeOpt = Sig.setType $
       Sig.map (lookupDetPower $ XIdx.power sec0 Network Water) envsChargeOpt

     powerGasDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerGasDischargeOpt = Sig.setType $
       Sig.map (lookupDetPower $ XIdx.power sec1 LocalNetwork Gas) envsDischargeOpt

     powerWaterDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerWaterDischargeOpt = Sig.setType $
       Sig.map (lookupDetPower $ XIdx.power sec1 Network Water) envsDischargeOpt

     powerTransformerChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerChargeOpt = Sig.setType $
       Sig.map (lookupDetPower $ XIdx.power sec0 Network LocalNetwork) envsChargeOpt

     powerTransformerDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerDischargeOpt = Sig.setType $
       Sig.map (lookupDetPower $ XIdx.power sec1 Network LocalNetwork) envsDischargeOpt

     powerTransformerChargeOptLV :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerChargeOptLV = Sig.setType $
       Sig.map (lookupDetPower $ XIdx.power sec0 LocalNetwork Network) envsChargeOpt

     powerTransformerDischargeOptLV :: Sig.PSignal2 V.Vector V.Vector Double
     powerTransformerDischargeOptLV = Sig.setType $
       Sig.map (lookupDetPower $ XIdx.power sec1 LocalNetwork Network) envsDischargeOpt

     ------------------------------------------------------------------------------------------
     -- | Start Simulation Here

     -- | Generate optimal control signals and build power record

     powerSignalWaterOptCharge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalWaterOptCharge"
           varRestPower1D varLocalPower powerWaterChargeOpt x y)
         powerSignalRest powerSignalLocal

     powerSignalGasOptCharge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalGasOptCharge"
           varRestPower1D varLocalPower powerGasChargeOpt x y)
         powerSignalRest powerSignalLocal

     powerSignalWaterOptDischarge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalWaterOptDischarge"
           varRestPower1D varLocalPower powerWaterDischargeOpt x y)
         powerSignalRest powerSignalLocal

     powerSignalGasOptDischarge =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "powerSignalGasOptDischarge"
           varRestPower1D varLocalPower powerGasDischargeOpt x y)
         powerSignalRest powerSignalLocal

     stateSignal =
       Sig.tzipWith
         (\x y -> Sig.interp2WingProfile
           "stateSignal"
           varRestPower1D varLocalPower
           (Sig.map (fromIntegral . fromEnum) maxEtaSysState)
           x y)
         powerSignalRest powerSignalLocal

     powerSignalWater =
       Sig.zipWith (\state (x, y) -> if state==0 then x else -y)
         stateSignal (Sig.zip powerSignalWaterOptCharge powerSignalWaterOptDischarge)

     powerSignalGas =
       Sig.zipWith (\state (x, y) -> if state==0 then x else y)
         stateSignal (Sig.zip powerSignalGasOptCharge powerSignalGasOptDischarge)

     time' :: Sig.TSignal [] Double
     time' = Sig.fromList [0 .. 23]

     rec :: Record.PowerRecord Node [] Double
     rec = addZeroCrossings $
           -- Record.diffTime $
           -- Record.rmap (Sig.changeSignalType . Sig.deltaMap (\x y -> (x+y)/2)) $
           Record.Record time' $
           Map.fromList [
              (XIdx.ppos Rest Network, powerSignalRest),
              (XIdx.ppos LocalRest LocalNetwork, powerSignalLocal),
              (XIdx.ppos Network Water, powerSignalWater),
              (XIdx.ppos LocalNetwork Gas, powerSignalGas)
             ]


     recConsumers :: Record.PowerRecord Node [] Double
     recConsumers = addZeroCrossings $
           -- Record.diffTime $
           -- Record.rmap (Sig.changeSignalType . Sig.deltaMap (\x y -> (x+y)/2)) $
           Record.Record time' $
           Map.fromList [
              (XIdx.ppos Rest Network, powerSignalRest),
              (XIdx.ppos LocalRest LocalNetwork, powerSignalLocal)
             ]

{-
   PlotIO.recordList "after zero" DefaultTerm.cons show id
                    [ (Record.Name "ohne zero", rec),
                      (Record.Name "mit zero", addZeroCrossings rec) ]
   let
-}

     -- | Build Sequenceflow graph for simulation
     flowGraphSim = seqFlowGraphFromFlowTopos [System.flowState4]

     -- | Generate and solve Equation System

     eqs :: EqSys.EquationSystemIgnore Node s (Data Nil Double) (Data ([] :> Nil) Double)
     eqs = Optimisation.givenSimulate etaAssign etaFunc $ Sequ.fromList [rec]

     envSim = EqSys.solve flowGraphSim eqs


     -- | extract power record from simulation env to allow subsequent EFA
     powerRecSim = ModUt.envToPowerRecord (Idx.Section 0) envSim

     -- | flip signs of power signals at water edge, as edge flips direction between state 0 and 4
     flipwater (Idx.PPos (Idx.TopologyEdge Network Water)) x = Sig.neg x
     flipwater (Idx.PPos (Idx.TopologyEdge Water Network)) x = Sig.neg x
     flipwater _ x = x
     powerRecSimCorr = Record.mapWithKey flipwater powerRecSim

     -- | make efa on simulation results
     rec0 = addZeroCrossings powerRecSimCorr

     sectionFilterTime ::  TC Scalar (Typ A T Tt) (Data Nil Double)
     sectionFilterTime = Sig.toScalar 0

     sectionFilterEnergy ::  TC Scalar (Typ A F Tt) (Data Nil Double)
     sectionFilterEnergy = Sig.toScalar 0

     sequencePowers :: Sequ.List (Record.PowerRecord System.Node [] Double)
     sequencePowers = genSequ rec0

     sequenceFlowTopologySim ::
        SeqFlow.Graph System.Node
           (Result (Data Nil Double))
           (Result (Data ([] :> Nil) Double))
     sequenceFlowTopologySim =
        RecSeq.flowGraphFromSequence $
        fmap (TopoRecord.flowTopologyFromRecord System.topologyOpt) $
        Sequ.filter (Record.major sectionFilterEnergy sectionFilterTime) $
        fmap Record.partIntegrate sequencePowers

     envSimAnalysis = Analysis.external2 sequenceFlowTopologySim
     envSimAnalysisCumulated =
        Analysis.external3 $
        SeqFlow.mapGraph id (fmap EqArith.integrate) sequenceFlowTopologySim

 {-
   concurrentlyMany_ [
     Draw.xterm $ Draw.seqFlowGraphAbsWithEnv System.flowGraphOpt
                  (fromJust (Sig.getSample2D envsChargeOpt (Sig.SignalIdx 0, Sig.SignalIdx 0))),


     Draw.xterm $ Draw.seqFlowGraphAbsWithEnv System.flowGraphOpt
                  (Sig.getSample2D (Sig.getSample2D envsDischarge (Sig.SignalIdx 1, Sig.SignalIdx 1)) (Sig.SignalIdx 1, Sig.SignalIdx 1)) ]

-}
   let optGas = Sweep.combineOptimalMaps maxEtaSysState powerGasChargeOpt powerGasDischargeOpt
       optWater = Sweep.combineOptimalMaps maxEtaSysState powerWaterChargeOpt powerWaterDischargeOpt


   concurrentlyMany_ $ [
     --putStrLn ("Storage Balance: " ++ show (ES.balance sequenceFlowTopologySim envSimAnalysisCumulated)),

{-

     Draw.xterm $ Draw.labeledTopology System.labeledTopologyOpt,

     putStrLn ("Number of possible flow states: " ++ show (length System.flowStatesOpt)),
     Draw.xterm $ Draw.flowTopologies (take 20 System.flowStatesOpt),
-}
     Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault System.flowGraphOpt,

     PlotIO.surfaceWithOpts "Optimal System Efficiency" DefaultTerm.cons id frameOpts varRestPower varLocalPower etaSysMax,


     PlotIO.surfaceWithOpts "Optimal State" DefaultTerm.cons id frameOpts varRestPower varLocalPower
        (Sig.map (fromIntegral . fromEnum) maxEtaSysState),

     PlotIO.surfaceWithOpts "Charging Optimal System Efficiency " DefaultTerm.cons id frameOpts varRestPower varLocalPower maxETACharge,
     PlotIO.surfaceWithOpts "Discharging Optimal System Efficiency " DefaultTerm.cons id frameOpts varRestPower varLocalPower maxETADischarge,

     --PlotIO.surface "Discharging Optimal System Efficiency " DefaultTerm.cons id varRestPower varLocalPower maxETADischarge,



     PlotIO.surfaceWithOpts "Charging Optimal Gas Power" DefaultTerm.cons id frameOpts varRestPower varLocalPower powerGasChargeOpt,
     PlotIO.surfaceWithOpts "Charging Optimal Water Power" DefaultTerm.cons id frameOpts varRestPower varLocalPower powerWaterChargeOpt,


--     PlotIO.surface "Discharging" DefaultTerm.cons id varRestPower varLocalPower maxETADischarge,


     PlotIO.surfaceWithOpts "Discharging Optimal Gas Power" DefaultTerm.cons id frameOpts varRestPower varLocalPower powerGasDischargeOpt,
     PlotIO.surfaceWithOpts "Discharging Optimal Water Power" DefaultTerm.cons id frameOpts varRestPower varLocalPower powerWaterDischargeOpt,

     PlotIO.surfaceWithOpts "Optimal Water Power" DefaultTerm.cons id frameOpts varRestPower varLocalPower optWater,
     PlotIO.surfaceWithOpts "Optimal Gas Power" DefaultTerm.cons id frameOpts varRestPower varLocalPower optGas,


     PlotIO.surfaceWithOpts "Transformer Power Charge HV" DefaultTerm.cons id frameOpts varRestPower varLocalPower powerTransformerChargeOpt,
     PlotIO.surfaceWithOpts "Transformer Power DisCharge HV" DefaultTerm.cons id frameOpts varRestPower varLocalPower powerTransformerDischargeOpt,

     PlotIO.surfaceWithOpts "Transformer Power Charge LV" DefaultTerm.cons id frameOpts varRestPower varLocalPower powerTransformerChargeOptLV,
     PlotIO.surfaceWithOpts "Transformer Power DisCharge LV" DefaultTerm.cons id frameOpts varRestPower varLocalPower powerTransformerDischargeOptLV,

     -- PlotIO.xy "Operation" DefaultTerm.cons id show powerSignalRest powerSignalLocal,


     report [] ("RestPower", varRestPower),
     report [] ("LocalPower", varLocalPower),
     report [] ("waterPower", varWaterPower),
     report [] ("gasPower", varGasPower),
     report [RAll] ("powerRecordSim", powerRecSim),
     report [RAll] ("rec0", rec0),
     report [RAll] ("rec", rec),

     PlotIO.record "Calculated Signals" DefaultTerm.cons show id rec,


     PlotIO.record "Local / Rest" DefaultTerm.cons show id recConsumers,
{-
     PlotIO.record "Simulation Result" DefaultTerm.cons show id powerRecSim,

     PlotIO.signal "State"  DefaultTerm.cons id stateSignal ,

     PlotIO.signal "Interpolated Signals"  DefaultTerm.cons id [powerSignalWaterOptCharge,
                                               powerSignalWaterOptDischarge,
                                               powerSignalGasOptCharge,
                                               powerSignalGasOptDischarge],
-}
     Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault envSimAnalysis,
     Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault envSim,

     Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault envSimAnalysisCumulated,
     return ()
     ]

