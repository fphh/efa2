{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}

module Main where



import qualified Modules.System as System
import qualified Modules.Optimisation as Optimisation
import qualified Modules.Analysis as Analysis
import Modules.System (Node(..))
import Modules.Optimisation (EnvDouble, sec0,sec1, SocDrive(..), lookupDetPower, condition, forcing )
import EFA.Application.Optimisation as AppOpt (etaOverPowerIn,etaOverPowerOut)
import qualified EFA.Application.Sweep as Sweep

import qualified Modules.Utility as ModUt
-- import Modules.Utility(getEtas, getPowerSignals,select)

--import qualified EFA.Application.EtaSys as ES
import qualified EFA.Application.Absolute as EqGen
import EFA.Application.Utility (select)
import qualified EFA.Application.Utility as AppUt
--import qualified EFA.Application.Optimisation as AppOpt
--import qualified EFA.Application.Sweep as Sweep

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph.Draw as Draw
--import qualified EFA.Graph as Graph

import qualified EFA.Equation.Arithmetic as EqArith
import qualified EFA.Equation.Environment as EqEnv
--import qualified EFA.Equation.Record as EqRec
import EFA.Equation.Result (Result(..))

import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.PlotIO as PlotIO
import qualified EFA.Signal.Plot as Plot
--import qualified EFA.Signal.Data as Data
import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Sequence (makeSeqFlowTopology, addZeroCrossings, genSequ,)
import EFA.Signal.Signal (TC,Scalar)
import EFA.Signal.Data (Data(..), Nil, (:>))-- getData)
import EFA.Signal.Typ (Typ, F, T, A, Tt) --,UT)

import EFA.Utility.Async (concurrentlyMany_)
import EFA.Report.Report(report,ROpt(..))

import qualified EFA.IO.TableParser as Table
import qualified EFA.IO.TableParserTypes as TPT

--import qualified EFA.Utility.Bifunctor as BF

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
--import qualified Graphics.Gnuplot.Terminal.PostScript as PostScript
--import qualified Graphics.Gnuplot.Terminal.PNG as PNG

import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Vector as V
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

--import qualified EFA.Graph.StateFlow.Environment as StFlEnv

--import Text.Printf (printf)

--import Data.Maybe (fromJust)
--import Data.Tuple.HT (fst3, snd3, thd3)

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

noLegend :: Int -> String
noLegend =  (const "")

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"

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
  TIdx.Section ->
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
      (EqEnv.Complete
        Node
        (Result (Data Nil Double))
        (Result (Data Nil Double))))
doubleSweep func etaFunc varOptX varOptY varX varY=
  sweep f (mm varX) (mm varY)
  where f x y =
          sweep (func etaAssign etaFunc x y) xo yo
        xo = mm varOptX
        yo = mm varOptY
        mm = map (map Data)

maxOptChargeFunc, maxOptDischargeFunc ::
  Double ->
  Sig.UTSignal2 V.Vector V.Vector EnvDouble ->
  (Double, EnvDouble)
maxOptChargeFunc socDrive =
  maybe (error "maxOptChargeFunc") id .
  Sweep.optimalSolution2D condition
    (forcing $ ChargeDrive socDrive) System.seqTopoOpt

maxOptDischargeFunc socDrive =
  maybe (error "maxOptDischargeFunc") id .
  Sweep.optimalSolution2D condition
    (forcing $ DischargeDrive socDrive) System.seqTopoOpt

makePics ::
  (forall s. EqGen.EquationSystem Node s (Data Nil Double) (Data Nil Double)) ->
  TPT.Map Double ->
  Double ->
  ( Sig.UTSignal2 V.Vector V.Vector Double,
    Sig.PSignal2 V.Vector V.Vector Double,
    Sig.PSignal2 V.Vector V.Vector Double,
    Sig.NSignal2 V.Vector V.Vector Double )
makePics eqs tabEta socDrive = t
  where t = (state, optWater, optGas, etaSysMax)

        chargeFunc = maxOptChargeFunc socDrive
        dischargeFunc = maxOptDischargeFunc socDrive

        state = Sig.map fromIntegral $ Sig.argMax maxETACharge maxETADischarge

        optWater = Sweep.combineOptimalMaps maxEtaSysState
                     powerWaterChargeOpt powerWaterDischargeOpt
        optGas = Sweep.combineOptimalMaps maxEtaSysState
                     powerGasChargeOpt powerGasDischargeOpt

        --maxEtaSysState :: Sig.UTSignal2 V.Vector V.Vector Double
        maxEtaSysState = Sig.map fromIntegral $
          Sig.argMax maxETACharge maxETADischarge

        --etaSysMax :: Sig.NSignal2 V.Vector V.Vector Double
        etaSysMax = Sig.zipWith max maxETACharge maxETADischarge

        --powerWaterChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerWaterChargeOpt = Sig.setType $
          Sig.map (lookupDetPower $ XIdx.power sec0 Network Water) envsChargeOpt


        --powerWaterDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerWaterDischargeOpt = Sig.setType $
          Sig.map (lookupDetPower $ XIdx.power sec1 Network Water) envsDischargeOpt

        -- powerGasChargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerGasChargeOpt = Sig.setType $
          Sig.map (lookupDetPower $ XIdx.power sec0 LocalNetwork Gas) envsChargeOpt


        --powerGasDischargeOpt :: Sig.PSignal2 V.Vector V.Vector Double
        powerGasDischargeOpt = Sig.setType $
          Sig.map (lookupDetPower $ XIdx.power sec1 LocalNetwork Gas) envsDischargeOpt

        --maxETACharge :: Sig.NSignal2 V.Vector V.Vector Double
        maxETACharge = Sig.setType $ Sig.map fst $
          Sig.map chargeFunc envsCharge

        --maxETADischarge :: Sig.NSignal2 V.Vector V.Vector Double
        maxETADischarge = Sig.setType $ Sig.map fst $
          Sig.map dischargeFunc envsDischarge

        envsCharge = Sig.map (Sig.map AppUt.envGetData) $
          doubleSweep (Optimisation.solveCharge eqs)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

        envsDischarge = Sig.map (Sig.map AppUt.envGetData) $
          doubleSweep (Optimisation.solveDischarge eqs)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

        envsChargeOpt = Sig.map snd $ Sig.map chargeFunc envsCharge

        envsDischargeOpt = Sig.map snd $ Sig.map dischargeFunc envsDischarge

        etaFunc = CT.makeEtaFunctions2D scaleTableEta tabEta

{-

-- Alternative Approch to get StateFlowGraph Data

stateFlow2SequFlow :: (Ord node) => Topo.StateFlowGraph node -> Topo.SequFlowGraph node
stateFlow2SequFlow = Graph.ixmap f g
  where f :: TIdx.AugNode TIdx.State node -> TIdx.AugNode TIdx.Section node
        f (TIdx.PartNode TIdx.Exit x) = TIdx.PartNode TIdx.Exit x
        f (TIdx.PartNode (TIdx.NoExit TIdx.Init) x) =
          TIdx.PartNode (TIdx.NoExit TIdx.Init) x
        f (TIdx.PartNode (TIdx.NoExit (TIdx.NoInit (TIdx.State i))) x) =
          TIdx.PartNode (TIdx.NoExit (TIdx.NoInit (TIdx.Section i))) x

        g :: Topo.FlowEdge Graph.EitherEdge (TIdx.AugNode TIdx.State node) ->
             Topo.FlowEdge Graph.EitherEdge (TIdx.AugNode TIdx.Section node)
        g = undefined


stateEnv2SequEnv :: (Ord node) => StFlEnv.Complete node a v -> EqEnv.Complete node a v
stateEnv2SequEnv (StFlEnv.Complete scal sig) =
  let StFlEnv.Scalar a b c d = scal
      StFlEnv.Signal u v w x y z = sig
      scalNew = EqEnv.Scalar
                  Map.empty
                  Map.empty
                  (Map.mapKeys f a)
                  (Map.mapKeys g b)
                  (Map.mapKeys h c)
                  undefined

      f (TIdx.ForNode (TIdx.StEnergy (TIdx.StorageEdge
          (TIdx.NoInit (TIdx.State i))
          (TIdx.NoExit (TIdx.State j)))) b) =
        (TIdx.ForNode (TIdx.StEnergy (TIdx.StorageEdge
          (TIdx.NoInit (TIdx.Section i))
          (TIdx.NoExit (TIdx.Section j)))) b)

      g (TIdx.ForNode (TIdx.StX (TIdx.StorageTrans
          (TIdx.NoExit (TIdx.NoInit (TIdx.State i)))
          (TIdx.NoExit (TIdx.NoInit (TIdx.State j))))) b) =
        (TIdx.ForNode (TIdx.StX (TIdx.StorageTrans
          (TIdx.NoExit (TIdx.NoInit (TIdx.Section i)))
          (TIdx.NoExit (TIdx.NoInit (TIdx.Section j))))) b)

      h = undefined

  in EqEnv.Complete scalNew undefined
-}

{-

main :: IO ()
main = do

   IO.hSetEncoding IO.stdout IO.utf8

   tabEta <- Table.read "../simulation/maps/eta.txt"
   tabPower <- Table.read "../simulation/maps/power.txt"

   let eqs ::
         (a ~ EqArith.Scalar v, Eq a, EqArith.Constant a,
         Eq v, EqArith.Product a, EqArith.Product v, EqArith.Integrate v) =>
         EqGen.EquationSystem Node s a v
       eqs = EqGen.fromGraph True (Topo.dirFromFlowGraph (snd System.seqTopoOpt))

       plotwaterpng n pic =
         PlotIO.surfaceWithOpts "Optimal Water Power"
           --(PNG.cons ("waterpics/optimal_water_power" ++ printf "%02d" n ++ ".png"))
           DefaultTerm.cons
           id
           frameOpts noLegend varRestPower varLocalPower
           pic


       plotgaspng n pic =
         PlotIO.surfaceWithOpts "Optimal Gas Power [W]"
           --(PNG.cons ("gaspics/optimal_gas_power" ++ printf "%02d" n ++ ".png"))
           DefaultTerm.cons
           id
           frameOpts noLegend varRestPower varLocalPower
           pic


       plotstatepng n pic =
         PlotIO.surfaceWithOpts "Optimal State"
           --(PNG.cons ("statepics/state_matrix" ++ printf "%02d" n ++ ".png"))
           DefaultTerm.cons
           id
           frameOpts noLegend varRestPower varLocalPower
           pic

       plotetasyspng n pic =
         PlotIO.surfaceWithOpts "System Efficiency [%]"
           (PNG.cons ("etasys/etasys" ++ printf "%02d" n ++ ".png"))
           id
           frameOpts noLegend varRestPower varLocalPower
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

   let eqsys ::
         (a ~ EqArith.Scalar v, Eq a, Eq v,
          EqArith.Constant a, EqArith.Product v, EqArith.Integrate v) =>
         EqGen.EquationSystem Node s a v
       eqsys = EqGen.fromGraph True (Topo.dirFromFlowGraph (snd System.seqTopoOpt))

   -- |Import Efficiency Curves
   let etaFunc = CT.makeEtaFunctions2D scaleTableEta tabEta
{-       etaWaterCharge:etaWaterDischarge:etaGas:etaCoal:etaTransHL:_ =
         getEtas etaFunc ["storage", "storage", "gas", "coal", "transformer"]-}

   -- | Import Power Curves
   let (_, powerSignalWind) :
         (_, powerSignalSolar) :
         (_, powerSignalHouse) :
         (_, powerSignalIndustry) : _
           = ModUt.getPowerSignals tabPower ["wind", "solar", "house", "industry"]


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
     envsCharge = Sig.map (Sig.map AppUt.envGetData) $
       doubleSweep (Optimisation.solveCharge eqsys)
                   etaFunc varWaterPower' varGasPower' varRestPower' varLocalPower'

     envsDischarge ::
       Sig.UTSignal2 V.Vector V.Vector
         (Sig.UTSignal2 V.Vector V.Vector EnvDouble)
     envsDischarge = Sig.map (Sig.map AppUt.envGetData) $
       doubleSweep (Optimisation.solveDischarge eqsys)
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

     maxEtaSysState :: Sig.UTSignal2 V.Vector V.Vector Double
     maxEtaSysState = Sig.map fromIntegral $
       Sig.argMax maxETACharge maxETADischarge


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
           varRestPower1D varLocalPower maxEtaSysState x y)
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
           Map.fromList [(TIdx.PPos (TIdx.StructureEdge Rest Network), powerSignalRest),
                       (TIdx.PPos (TIdx.StructureEdge LocalRest LocalNetwork), powerSignalLocal),
                       (TIdx.PPos (TIdx.StructureEdge Network Water), powerSignalWater),
                       (TIdx.PPos (TIdx.StructureEdge LocalNetwork Gas), powerSignalGas)
                      ]


     recConsumers :: Record.PowerRecord Node [] Double
     recConsumers = addZeroCrossings $
           -- Record.diffTime $
           -- Record.rmap (Sig.changeSignalType . Sig.deltaMap (\x y -> (x+y)/2)) $
           Record.Record time' $
           Map.fromList [(TIdx.PPos (TIdx.StructureEdge Rest Network), powerSignalRest),
                       (TIdx.PPos (TIdx.StructureEdge LocalRest LocalNetwork), powerSignalLocal)
                      ]

{-
   PlotIO.recordList "after zero" DefaultTerm.cons show id
                    [ (Record.Name "ohne zero", rec),
                      (Record.Name "mit zero", addZeroCrossings rec) ]
   let
-}

     time = Record.getTime rec

     -- | Build Sequenceflow graph for simulation
     seqTopoSim = Flow.sequenceGraph (select System.flowStatesOpt [4])

     -- | Generate and solve Equation System

     eqs :: EqGen.EquationSystem Node s (Data Nil Double) (Data ([] :> Nil) Double)
     eqs = Optimisation.givenSimulate etaAssign etaFunc $
             SD.SequData [SD.Section (TIdx.Section 0) undefined rec]

     envSim = EqGen.solve seqTopoSim eqs


     -- | extract power record from simulation env to allow subsequent EFA
     powerRecSim = ModUt.envToPowerRecord envSim time (TIdx.Section 0)

     -- | flip signs of power signals at water edge, as edge flips direction between state 0 and 4
     flipwater (TIdx.PPos (TIdx.StructureEdge Network Water)) x = Sig.neg x
     flipwater (TIdx.PPos (TIdx.StructureEdge Water Network)) x = Sig.neg x
     flipwater _ x = x
     powerRecSimCorr = Record.rmapWithKey flipwater powerRecSim

     -- | make efa on simulation results
     rec0 = addZeroCrossings powerRecSimCorr

     sectionFilterTime ::  TC Scalar (Typ A T Tt) (Data Nil Double)
     sectionFilterTime = Sig.toScalar 0

     sectionFilterEnergy ::  TC Scalar (Typ A F Tt) (Data Nil Double)
     sectionFilterEnergy = Sig.toScalar 0

     sequencePowers :: SD.SequData (Record.PowerRecord System.Node [] Double)
     sequencePowers = genSequ rec0
     (_, sequenceFlowsFilt) =
         SD.unzip $
         SD.filter (Record.major sectionFilterEnergy sectionFilterTime . snd) $
         fmap (\x -> (x, Record.partIntegrate x)) sequencePowers

     (flowStates, adjustedFlows) =
         SD.unzip $
         fmap
         (\state ->
           let flowState = Flow.genFlowState state
           in  (flowState, Flow.adjustSigns System.topologyOpt flowState state))
         sequenceFlowsFilt

     flowTopos = Flow.genSequFlowTops System.topologyOpt flowStates
     sequenceFlowTopologySim = makeSeqFlowTopology flowTopos
     envSimAnalysis = Analysis.external2 sequenceFlowTopologySim adjustedFlows
     envSimAnalysisCumulated = Analysis.external2 sequenceFlowTopologySim
                                 (fmap Record.sumFlowRecord adjustedFlows)

 {-
   concurrentlyMany_ [
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt
                  (fromJust (Sig.getSample2D envsChargeOpt (Sig.SignalIdx 0, Sig.SignalIdx 0))),


     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt
                  (Sig.getSample2D (Sig.getSample2D envsDischarge (Sig.SignalIdx 1, Sig.SignalIdx 1)) (Sig.SignalIdx 1, Sig.SignalIdx 1)) ]

-}
   let optGas = Sweep.combineOptimalMaps maxEtaSysState powerGasChargeOpt powerGasDischargeOpt
       optWater = Sweep.combineOptimalMaps maxEtaSysState powerWaterChargeOpt powerWaterDischargeOpt


   concurrentlyMany_ $ [
     --putStrLn ("Storage Balance: " ++ show (ES.balance sequenceFlowTopologySim envSimAnalysisCumulated)),

{-

     Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNamesOpt System.topologyOpt,

     putStrLn ("Number of possible flow states: " ++ show (length System.flowStatesOpt)),
     Draw.xterm $ Draw.flowTopologies (take 20 System.flowStatesOpt),
-}
     Draw.xterm $ Draw.sequFlowGraph System.seqTopoOpt,

     PlotIO.surfaceWithOpts "Optimal System Efficiency" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower etaSysMax,


     PlotIO.surfaceWithOpts "Optimal State" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower maxEtaSysState,

     PlotIO.surfaceWithOpts "Charging Optimal System Efficiency " DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower maxETACharge,
     PlotIO.surfaceWithOpts "Discharging Optimal System Efficiency " DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower maxETADischarge,

     --PlotIO.surface "Discharging Optimal System Efficiency " DefaultTerm.cons id noLegend varRestPower varLocalPower maxETADischarge,



     PlotIO.surfaceWithOpts "Charging Optimal Gas Power" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerGasChargeOpt,
     PlotIO.surfaceWithOpts "Charging Optimal Water Power" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerWaterChargeOpt,


--     PlotIO.surface "Discharging" DefaultTerm.cons id noLegend varRestPower varLocalPower maxETADischarge,


     PlotIO.surfaceWithOpts "Discharging Optimal Gas Power " DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerGasDischargeOpt,
     PlotIO.surfaceWithOpts "Discharging Optimal Water Power " DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerWaterDischargeOpt,

     PlotIO.surfaceWithOpts "Optimal Water Power" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower optWater,
     PlotIO.surfaceWithOpts "Optimal Gas Power" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower optGas,


     PlotIO.surfaceWithOpts "Transformer Power Charge HV " DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerTransformerChargeOpt,
     PlotIO.surfaceWithOpts "Transformer Power DisCharge HV" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerTransformerDischargeOpt,

     PlotIO.surfaceWithOpts "Transformer Power Charge LV " DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerTransformerChargeOptLV,
     PlotIO.surfaceWithOpts "Transformer Power DisCharge LV" DefaultTerm.cons id frameOpts noLegend varRestPower varLocalPower powerTransformerDischargeOptLV,

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
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv  sequenceFlowTopologySim envSimAnalysis,
     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv  seqTopoSim envSim,

     Draw.xterm $ Draw.sequFlowGraphAbsWithEnv sequenceFlowTopologySim envSimAnalysisCumulated,

       return ()
     ]

