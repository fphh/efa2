{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Modules.Optimisation as Optimisation
import qualified Modules.System as System
import Modules.Optimisation (sec0,sec1)
import Modules.System (Node(..))

import qualified EFA.Application.Utility as ModUt
import qualified EFA.Application.Absolute as EqGen
import qualified EFA.Application.Plot as PlotIO

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Graph.Draw as Draw

import EFA.Utility.Async (concurrentlyMany_)

import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.Signal.Signal as Sig

import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import EFA.Equation.Result (Result(Determined, Undetermined))

import qualified EFA.IO.TableParser as Table

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified System.IO as IO

import qualified Data.Map as Map


plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

colours :: [Colors.X11Color]
colours = [ Colors.White,	
            Colors.Gray90,	
            Colors.Gray80,	
            Colors.Gray70 ]

waterPower :: [Double]
waterPower = [0,0.2 .. 1]

gasPower :: [Double]
gasPower = [0, 0.1 .. 1]

powerScaleWater ::  Double
powerScaleWater = 5

powerScaleGas ::  Double
powerScaleGas = 1

powerScaleTransformer ::  Double
powerScaleTransformer = 1

powerScaleCoal ::  Double
powerScaleCoal = 1


varWaterPowerSig :: Sig.PTestRow [] Double
varWaterPowerSig = Sig.fromList waterPower

varGasPowerSig :: Sig.PTestRow [] Double
varGasPowerSig = Sig.fromList gasPower

varWaterPower', varGasPower' :: [[Double]]
(varWaterPower', varGasPower') = CT.varMat waterPower gasPower

varWaterPower :: Sig.PSignal2 [] [] Double
varWaterPower = Sig.fromList2 varWaterPower'

varGasPower :: Sig.PSignal2 [] [] Double
varGasPower = Sig.fromList2 varGasPower'

legendEnergy :: Int -> String
legendEnergy 0 = "eRest0"
legendEnergy 1 = "eRestLocal0"
legendEnergy 2 = "eGas0"
legendEnergy 3 = "eCoal0"
legendEnergy 4 = "eRest1"
legendEnergy 5 = "eRestLocal1"
legendEnergy 6 = "eGas1"
legendEnergy 7 = "eCoal1"
legendEnergy _ = "Undefined"

main :: IO ()
main = do

   IO.hSetEncoding IO.stdout IO.utf8

   tabEta <- Table.read "../simulation/maps/eta.txt"

   let etaWaterCharge :: Double -> Double
       etaWaterCharge = Sig.fromSample . Sig.interp1Lin "etaWaterCharge" (Sig.scale xs powerScaleWater) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (Map.lookup "storage" tabEta)

   let etaWaterDischarge :: Double -> Double
       etaWaterDischarge = Sig.fromSample . Sig.interp1Lin "etaWaterDischarge" (Sig.scale xs powerScaleWater) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (Map.lookup "storage" tabEta)

   let etaGas :: Double -> Double
       etaGas = Sig.fromSample . Sig.interp1Lin "etaGas" (Sig.scale xs powerScaleGas) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (Map.lookup "gas" tabEta)

   let etaCoal :: Double -> Double
       etaCoal = Sig.fromSample . Sig.interp1Lin "etaCoal" (Sig.scale xs powerScaleCoal) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (Map.lookup "coal" tabEta)

   let etaTransHL :: Double -> Double
       etaTransHL = Sig.fromSample . Sig.interp1Lin "etaTransHL" (Sig.scale xs powerScaleTransformer) (head ys) . Sig.toSample
         where xs :: Sig.PSignal [] Double
               ys :: [Sig.NSignal [] Double]
               (xs,ys) = CT.convertToSignal2D (Map.lookup "transformer" tabEta)

   let

     -- | Defining the given Variables for each Section

     envsCharge :: Sig.UTTestRow2 [] []
                   ((EqEnv.Complete
                     Node
                     (EqRec.Absolute (Result Double))
                     (EqRec.Absolute (Result Double))))

     envsCharge = Sig.fromList2 $ zipWith (zipWith (Optimisation.solveCharge etaWaterCharge etaCoal etaGas etaTransHL 1 1)) varWaterPower' varGasPower'


     envsDischarge :: Sig.UTTestRow2 [] []
                    ((EqEnv.Complete
                      Node
                      (EqRec.Absolute (Result Double))
                      (EqRec.Absolute (Result Double))))
     envsDischarge = Sig.fromList2 $ zipWith (zipWith (Optimisation.solveDischarge etaWaterDischarge etaCoal etaGas etaTransHL 1 1)) varWaterPower' varGasPower'


   -- Extract interesting values

     eGasCharge0 :: Sig.FTestRow2 [] [] Double
     eGasCharge0 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec0 Gas LocalNetwork)) envsCharge

     eCoalCharge0 :: Sig.FTestRow2 [] [] Double
     eCoalCharge0 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec0 Coal Network)) envsCharge

     eRestCharge0 :: Sig.FTestRow2 [] [] Double
     eRestCharge0 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec0 Rest Network)) envsCharge

     eRestLocalCharge0 :: Sig.FTestRow2 [] [] Double
     eRestLocalCharge0 = Sig.setType $Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec0 LocalRest LocalNetwork)) envsCharge

     eGasCharge1 :: Sig.FTestRow2 [] [] Double
     eGasCharge1 = Sig.setType $Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec1 Gas LocalNetwork)) envsCharge

     eCoalCharge1 :: Sig.FTestRow2 [] [] Double
     eCoalCharge1 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec1 Coal Network)) envsCharge

     eRestCharge1 :: Sig.FTestRow2 [] [] Double
     eRestCharge1 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec1 Rest Network)) envsCharge

     eRestLocalCharge1 :: Sig.FTestRow2 [] [] Double
     eRestLocalCharge1 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec1 LocalRest LocalNetwork)) envsCharge

     eGasDischarge0 :: Sig.FTestRow2 [] [] Double
     eGasDischarge0 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec0 Gas LocalNetwork)) envsDischarge

     eCoalDischarge0 :: Sig.FTestRow2 [] [] Double
     eCoalDischarge0 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec0 Coal Network)) envsDischarge

     eRestDischarge0 :: Sig.FTestRow2 [] [] Double
     eRestDischarge0 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec0 Rest Network)) envsDischarge

     eRestLocalDischarge0 :: Sig.FTestRow2 [] [] Double
     eRestLocalDischarge0 = Sig.setType $Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec0 LocalRest LocalNetwork)) envsDischarge

     eGasDischarge1 :: Sig.FTestRow2 [] [] Double
     eGasDischarge1 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec1 Gas LocalNetwork)) envsDischarge

     eCoalDischarge1 :: Sig.FTestRow2 [] [] Double
     eCoalDischarge1 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec1 Coal Network)) envsDischarge

     eRestDischarge1 :: Sig.FTestRow2 [] [] Double
     eRestDischarge1 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec1 Rest Network)) envsDischarge


     eRestLocalDischarge1 :: Sig.FTestRow2 [] [] Double
     eRestLocalDischarge1 = Sig.setType $ Sig.map (ModUt.lookupAbsEnergy (XIdx.energy sec1 LocalRest LocalNetwork)) envsDischarge

     etaSysCharge :: Sig.NTestRow2 [] [] Double
     etaSysCharge = eOut Sig../ eIn
       where
         eOut = (eRestCharge0 Sig..+ (Sig.makeDelta $ eRestCharge1 Sig..+
                                 (Sig.makeDelta $ eRestLocalCharge0 Sig..+ (Sig.makeDelta eRestLocalCharge1))))
         eIn = (eGasCharge0 Sig..+ (Sig.makeDelta $ eGasCharge1 Sig..+
                               (Sig.makeDelta $ eCoalCharge0  Sig..+ (Sig.makeDelta eCoalCharge1))))

     etaSysDischarge :: Sig.NTestRow2 [] [] Double
     etaSysDischarge = eOut Sig../ eIn
      where
        eOut = (eRestDischarge0 Sig..+ (Sig.makeDelta $ eRestDischarge1 Sig..+
                                        (Sig.makeDelta $ eRestLocalDischarge0 Sig..+ (Sig.makeDelta eRestLocalDischarge1))))
        eIn = (eGasDischarge0 Sig..+ (Sig.makeDelta $ eGasDischarge1 Sig..+
                                  (Sig.makeDelta $ eCoalDischarge0  Sig..+ (Sig.makeDelta eCoalDischarge1))))

     etaSysMax::    Sig.NTestRow2 [] [] Double
     etaSysMax = Sig.zipWith max etaSysCharge etaSysDischarge

{-
maxEtaSysState :: Sig.UTTestRow2 [] [] Double
maxEtaSysState = Sig.map fromIntegral $ Sig.sigMax2 etaSysCharge etaSysDischarge

etaLoeCharge::    Sig.NTestRow [] Double
etaLoeCharge =  Sig.map2 maximum (Sig.transpose2 $ Sig.changeSignalType etaSysCharge)

etaLoeDischarge :: Sig.NTestRow [] Double
etaLoeDischarge =  Sig.map2 maximum (Sig.transpose2 $ Sig.changeSignalType etaSysDischarge)

indexLoeCharge::    Sig.UTTestRow [] Int
indexLoeCharge =  Sig.map2 (\x -> Maybe.fromJust $ V.findIndex (maximum x == ) x) (Sig.transpose2 $ Sig.untype etaSysCharge)

indexLoeDischarge::    Sig.UTTestRow [] Int
indexLoeDischarge =  Sig.map2 (\x -> Maybe.fromJust $ V.findIndex (maximum x == ) x) (Sig.transpose2 $ Sig.untype etaSysDischarge)

indexLoeChargePlot::    Sig.UTTestRow [] Double
indexLoeChargePlot =  Sig.map fromIntegral indexLoeCharge

indexLoeDischargePlot::    Sig.UTTestRow [] Double
indexLoeDischargePlot =  Sig.map fromIntegral indexLoeDischarge

powerLoeCharge::    Sig.PTestRow [] Double
powerLoeCharge =  Sig.setType $ Sig.map (\x -> gasPower L.!! x) $ indexLoeCharge

powerLoeDischarge::    Sig.PTestRow [] Double
powerLoeDischarge =  Sig.setType $ Sig.map (\x -> gasPower L.!! x) $ indexLoeDischarge
-}





   concurrentlyMany_ $ [
     Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNamesOpt System.topologyOpt,

     putStrLn ("Number of possible flow states: " ++ show (length System.flowStatesOpt)),

     Draw.xterm $ Draw.flowTopologies (take 20 System.flowStatesOpt),

     Draw.xterm $ Draw.sequFlowGraph System.seqTopoOpt,

     Draw.xterm $ Draw.title "Optimise Charging00" $
     Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt $
     EqGen.solve System.seqTopoOpt $ Optimisation.givenCharging etaWaterCharge etaCoal etaGas etaTransHL 1 1 0 0,

     Draw.xterm $ Draw.title "Optimise Charging01" $
     Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt $
     EqGen.solve System.seqTopoOpt $ Optimisation.givenCharging etaWaterCharge etaCoal etaGas etaTransHL 1 1 0 1,

     Draw.xterm $ Draw.title "Optimise Charging10" $
     Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt $
     EqGen.solve System.seqTopoOpt $ Optimisation.givenCharging etaWaterCharge etaCoal etaGas etaTransHL 1 1 1 0,

     Draw.xterm $ Draw.title "Optimise Charging11" $
     Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt $
     EqGen.solve System.seqTopoOpt $ Optimisation.givenCharging etaWaterCharge etaCoal etaGas etaTransHL 1 1 1 1,

     Draw.xterm $ Draw.title "Optimise Discharging00" $
     Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt $
     EqGen.solve System.seqTopoOpt $ Optimisation.givenDischarging etaWaterCharge etaCoal etaGas etaTransHL 1 1 0 0,

     Draw.xterm $ Draw.title "Optimise Discharging01" $
     Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt $
     EqGen.solve System.seqTopoOpt $ Optimisation.givenDischarging etaWaterCharge etaCoal etaGas etaTransHL 1 1 0 1,

     Draw.xterm $ Draw.title "Optimise Discharging10" $
     Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt $
     EqGen.solve System.seqTopoOpt $ Optimisation.givenDischarging etaWaterCharge etaCoal etaGas etaTransHL 1 1 1 0,

     Draw.xterm $ Draw.title "Optimise Discharging11" $
     Draw.sequFlowGraphAbsWithEnv System.seqTopoOpt $
     EqGen.solve System.seqTopoOpt $ Optimisation.givenDischarging etaWaterCharge etaCoal etaGas etaTransHL 1 1 1 1,

     PlotIO.surface "Optimiere Laden - eRestCharge0" DefaultTerm.cons id varWaterPower varGasPower eRestCharge0,
     PlotIO.surface "Optimiere Laden - eRestCharge1" DefaultTerm.cons id varWaterPower varGasPower eRestCharge0,
     PlotIO.surface "Optimiere Laden - eRestLocalCharge0" DefaultTerm.cons id varWaterPower varGasPower eRestLocalCharge0,
     PlotIO.surface "Optimiere Laden - eRestLocalCharge1" DefaultTerm.cons id varWaterPower varGasPower eRestLocalCharge1,


     PlotIO.surface "System Efficiency Charging" DefaultTerm.cons id varWaterPower varGasPower etaSysDischarge,
     PlotIO.surface "System Efficiency Discharging" DefaultTerm.cons id varWaterPower varGasPower etaSysCharge,
     PlotIO.surface "System Efficiency Charging and Discharging" DefaultTerm.cons id varWaterPower varGasPower
       [PlotIO.label "Laden" etaSysCharge,
        PlotIO.label "Entladen" etaSysDischarge]]
{-
    PlotIO.surface "Maximum System Efficiency " DefaultTerm.cons id varWaterPower varGasPower etaSysMax,
    PlotIO.surface "Maximum System Efficiency " DefaultTerm.cons id varWaterPower varGasPower maxEtaSysState,
    PlotIO.xy "Efficiency on Loe" DefaultTerm.cons id varWaterPowerSig [etaLoeCharge, etaLoeDischarge],
    PlotIO.xy "Loe Index" DefaultTerm.cons id varWaterPowerSig [indexLoeChargePlot, indexLoeDischargePlot],
    PlotIO.xy "Loe GasPower" DefaultTerm.cons id varWaterPowerSig [powerLoeCharge, powerLoeDischarge]
    ]

-}

