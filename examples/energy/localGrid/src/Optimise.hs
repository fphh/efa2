module Main where

--import qualified Modules.Signals as Signals
import qualified Modules.System as System
import Modules.System (Node(..))

--import qualified EFA.Signal.Record as Record
import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Draw as Draw
--import EFA.IO.PLTImport (modelicaPLTImport)
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Vector as V
--import EFA.Signal.Data (Data(..), Nil)
--import EFA.Signal.Typ (Typ, F, T, A, Tt)
import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Utility as Utility
--import EFA.Signal.Sequence (makeSeqFlowTopology)
import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Graph.Draw as Draw
--import EFA.Graph(lefilter)
--import qualified EFA.Signal.Plot as Plot
--import EFA.Graph.Topology(isStructureEdge)
--import qualified EFA.Example.Index as XIdx
--import qualified EFA.Equation.Arithmetic as Arith
--import qualified EFA.Equation.Environment as Env
import qualified EFA.Signal.ConvertTable as Table
import qualified EFA.Example.Absolute as EqGen
import qualified EFA.Example.Index as XIdx
import qualified EFA.Graph.Topology.Index as TIdx
import qualified EFA.Equation.Environment as EqEnv
import qualified EFA.Equation.Record as EqRec
import EFA.Equation.Result (Result(..))
import EFA.Utility.Map (checkedLookup)
import qualified EFA.Signal.PlotIO as PlotIO

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Data.GraphViz.Attributes.Colors.X11 as Colors

import qualified EFA.Signal.ConvertTable as CT
import qualified EFA.IO.TableParser as Table

import qualified System.IO as IO
--import System.Environment (getEnv)
--import System.FilePath ((</>))

import qualified EFA.Utility.Stream as Stream

import Data.Monoid (mconcat, (<>))
import EFA.Example.Absolute ( (.=), (%=), (=.=) )
import EFA.Utility.Stream (Stream((:~)))

import qualified Data.List as L
--import qualified Data.Map as M
--import qualified Modules.Analysis as Analysis
--import Data.Tuple.HT (mapSnd)

import qualified Data.Maybe as Maybe


plotTerm :: DefaultTerm.T
plotTerm = DefaultTerm.cons

colours :: [Colors.X11Color]
colours = [ Colors.White,	
            Colors.Gray90,	
            Colors.Gray80,	
            Colors.Gray70 ]

gasPower :: [Double]
gasPower = [0, 0.1 .. 1]

waterPower :: [Double]
waterPower = [0,0.2 .. 1]

varXSig :: Sig.PTestRow [] Double
varXSig = Sig.fromList waterPower

varYSig :: Sig.PTestRow [] Double
varYSig = Sig.fromList gasPower

sec0, sec1 :: TIdx.Section
sec0 :~ sec1 :~ _ = Stream.enumFrom $ TIdx.Section 0

varX', varY' :: [[Double]]
(varX', varY') = Table.varMat waterPower gasPower

varX :: Sig.PSignal2 [] [] Double
varX = Sig.fromList2 varX'

varY :: Sig.PSignal2 [] [] Double
varY = Sig.fromList2 varY'

-- | Defining the given Variables

lookupEtaWaterCharge ::
  EqGen.Expression node s a v Double ->
  EqGen.Expression node s a v Double
lookupEtaWaterCharge = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin "lookupEtaWaterCharge" xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

lookupEtaWaterDisCharge ::
  EqGen.Expression node s a v Double ->
  EqGen.Expression node s a v Double
lookupEtaWaterDisCharge = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin "lookupEtaWaterDisCharge" xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

lookupEtaCoal ::
  EqGen.Expression node s a v Double ->
  EqGen.Expression node s a v Double
lookupEtaCoal = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin "lookupEtaCoal" xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

lookupEtaGas ::
  EqGen.Expression node s a v Double ->
  EqGen.Expression node s a v Double
lookupEtaGas = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin "lookupEtaGas" xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

lookupEtaTransformerHL ::
  EqGen.Expression node s a v Double ->
  EqGen.Expression node s a v Double
lookupEtaTransformerHL = EqGen.liftF f
  where f = Sig.fromSample . Sig.interp1Lin "lookupEtaTransformerHL" xs ys . Sig.toSample
        xs, ys :: Sig.UTSignal [] Double
        xs = Sig.fromList [0 .. 30]
        ys = Sig.fromList [0.50, 0.51 .. 0.8]

-- | Defining the given Variables for each Section

commonGiven :: EqGen.EquationSystem System.Node s Double Double
commonGiven =
   mconcat $
   (XIdx.dTime sec0 .= 1) :
   (XIdx.dTime sec1 .= 1) :
   (XIdx.storage TIdx.Initial Water .= 0) :
   (XIdx.energy sec0 Water Network  %= XIdx.energy sec1 Water Network) :
   []

givenOptimiseCharging :: Double -> Double -> Double -> Double -> EqGen.EquationSystem System.Node s Double Double
givenOptimiseCharging pRest pRestLocal pWater pGas =
   (commonGiven <>) $
   mconcat $

   -- Actual Section 0 Charhing to be varied and optimised
   (XIdx.power sec0 Rest Network .= pRest) :
   (XIdx.power sec0 LocalRest LocalNetwork .= pRestLocal) :
   (XIdx.power sec0 Network Water .= pWater) :
   (XIdx.power sec0 Gas LocalNetwork .= pGas) :
   ((EqGen.variable $ XIdx.eta sec0 Network Water) =.=
     lookupEtaWaterCharge (EqGen.variable $ XIdx.power sec0 Network Water)) :
   ((EqGen.variable $ XIdx.eta sec0 Coal Network) =.=
     lookupEtaCoal (EqGen.variable $ XIdx.power sec0 Network Coal)) :
   ((EqGen.variable $ XIdx.eta sec0 Gas LocalNetwork) =.=
     lookupEtaGas (EqGen.variable $ XIdx.power sec0 Gas LocalNetwork)) :
   ((EqGen.variable $ XIdx.eta sec0 Network LocalNetwork) =.=
     lookupEtaTransformerHL (EqGen.variable $ XIdx.power sec0 LocalNetwork Network)) :
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

givenOptimiseDischarging :: Double -> Double -> Double -> Double -> EqGen.EquationSystem System.Node s Double Double
givenOptimiseDischarging pRest pRestLocal pWater pGas =
   (commonGiven <>) $
   mconcat $

   -- Actual Section 1 discharging to be varied and optimised
   (XIdx.power sec1 Rest Network .= pRest) :
   (XIdx.power sec1 LocalRest LocalNetwork .= pRestLocal) :
   (XIdx.power sec1 Network Water .= pWater) :
   (XIdx.power sec1 Gas LocalNetwork .= pGas) :
   ((EqGen.variable $ XIdx.eta sec1 Water Network) =.=
     lookupEtaWaterDisCharge (EqGen.variable $ XIdx.power sec1 Network Water)) :
   ((EqGen.variable $ XIdx.eta sec1 Coal Network) =.=
     lookupEtaCoal (EqGen.variable $ XIdx.power sec1 Network Coal)) :
   ((EqGen.variable $ XIdx.eta sec1 Gas LocalNetwork) =.=
     lookupEtaGas (EqGen.variable $ XIdx.power sec1 Gas LocalNetwork)) :
   ((EqGen.variable $ XIdx.eta sec1 Network LocalNetwork) =.=
     lookupEtaTransformerHL (EqGen.variable $ XIdx.power sec1 LocalNetwork Network)) :
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

select :: [topo] -> [Int] -> SD.SequData topo
select ts = SD.fromList . map (ts !!)

seqTopo :: Flow.RangeGraph Node
seqTopo = Flow.mkSequenceTopology (select System.flowStatesOpt [4,0])

-- Solving Function for each Section

solveCharge :: Double ->               
               Double -> 
               Double -> 
               Double -> 
               EqEnv.Complete  
               Node
               (EqRec.Absolute (Result Double))
               (EqRec.Absolute (Result Double))
solveCharge w x y z = EqGen.solve seqTopo $ givenOptimiseCharging w x y z


solveDischarge :: Double ->               
               Double -> 
               Double -> 
               Double -> 
               EqEnv.Complete  
               Node
               (EqRec.Absolute (Result Double))
               (EqRec.Absolute (Result Double))
solveDischarge w x y z = EqGen.solve seqTopo $ givenOptimiseDischarging w x y z

-- Solve each Section
envsCharge :: Sig.UTTestRow2 [] [] 
               ((EqEnv.Complete  
               Node
               (EqRec.Absolute (Result Double))
               (EqRec.Absolute (Result Double))))

envsCharge = Sig.fromList2 $ zipWith (zipWith (solveCharge 1 1)) varX' varY'


envsDischarge :: Sig.UTTestRow2 [] []
                 ((EqEnv.Complete  
               Node
               (EqRec.Absolute (Result Double))
               (EqRec.Absolute (Result Double))))
envsDischarge = Sig.fromList2 $ zipWith (zipWith (solveDischarge 1 1)) varX' varY'


lookupAbsEnergy :: (Ord node, Show d, Show node) =>
                   TIdx.InSection TIdx.Energy node -> 
                   EqEnv.Complete node b (EqRec.Absolute (Result d)) ->                   
                   d
lookupAbsEnergy n env = case checkedLookup (EqEnv.energyMap $ EqEnv.signal env) n of
                  EqRec.Absolute (Determined x) -> x
                  EqRec.Absolute (Undetermined) -> error $ "not determined : " ++ show n 


lookupAbsPower :: (Ord node, Show d, Show node) =>
                   TIdx.InSection TIdx.Power node -> 
                   EqEnv.Complete node b (EqRec.Absolute (Result d)) ->
                   d
lookupAbsPower n env = case checkedLookup (EqEnv.powerMap $ EqEnv.signal env) n of
                  EqRec.Absolute (Determined x) -> x
                  EqRec.Absolute (Undetermined) -> error $ "not determined : " ++ show n 

lookupAbsEta :: (Ord node, Show d, Show node) =>
                   TIdx.InSection TIdx.Power node -> 
                   EqEnv.Complete node b (EqRec.Absolute (Result d)) ->                   
                   d
lookupAbsEta n env = case checkedLookup (EqEnv.powerMap $ EqEnv.signal env) n of
                  EqRec.Absolute (Determined x) -> x
                  EqRec.Absolute (Undetermined) -> error $ "not determined : " ++ show  n

-- Extract interesting values

eGasCharge0 :: Sig.FTestRow2 [] [] Double
eGasCharge0 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec0 Gas LocalNetwork)) envsCharge

eCoalCharge0 :: Sig.FTestRow2 [] [] Double
eCoalCharge0 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec0 Coal Network)) envsCharge

eRestCharge0 :: Sig.FTestRow2 [] [] Double
eRestCharge0 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec0 Rest Network)) envsCharge

eRestLocalCharge0 :: Sig.FTestRow2 [] [] Double
eRestLocalCharge0 = Sig.setType $Sig.map (lookupAbsEnergy (XIdx.energy sec0 LocalRest LocalNetwork)) envsCharge
        
eGasCharge1 :: Sig.FTestRow2 [] [] Double
eGasCharge1 = Sig.setType $Sig.map (lookupAbsEnergy (XIdx.energy sec1 Gas LocalNetwork)) envsCharge

eCoalCharge1 :: Sig.FTestRow2 [] [] Double
eCoalCharge1 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec1 Coal Network)) envsCharge

eRestCharge1 :: Sig.FTestRow2 [] [] Double
eRestCharge1 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec1 Rest Network)) envsCharge

eRestLocalCharge1 :: Sig.FTestRow2 [] [] Double
eRestLocalCharge1 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec1 LocalRest LocalNetwork)) envsCharge

eGasDischarge0 :: Sig.FTestRow2 [] [] Double
eGasDischarge0 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec0 Gas LocalNetwork)) envsDischarge

eCoalDischarge0 :: Sig.FTestRow2 [] [] Double
eCoalDischarge0 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec0 Coal Network)) envsDischarge

eRestDischarge0 :: Sig.FTestRow2 [] [] Double
eRestDischarge0 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec0 Rest Network)) envsDischarge

eRestLocalDischarge0 :: Sig.FTestRow2 [] [] Double
eRestLocalDischarge0 = Sig.setType $Sig.map (lookupAbsEnergy (XIdx.energy sec0 LocalRest LocalNetwork)) envsDischarge
        
eGasDischarge1 :: Sig.FTestRow2 [] [] Double
eGasDischarge1 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec1 Gas LocalNetwork)) envsDischarge

eCoalDischarge1 :: Sig.FTestRow2 [] [] Double
eCoalDischarge1 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec1 Coal Network)) envsDischarge

eRestDischarge1 :: Sig.FTestRow2 [] [] Double
eRestDischarge1 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec1 Rest Network)) envsDischarge


eRestLocalDischarge1 :: Sig.FTestRow2 [] [] Double
eRestLocalDischarge1 = Sig.setType $ Sig.map (lookupAbsEnergy (XIdx.energy sec1 LocalRest LocalNetwork)) envsDischarge

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


noLegend :: Int -> String
noLegend =  (const "")

legend :: Int -> String
legend 0 = "Laden"
legend 1 = "Entladen"
legend _ = "Undefined"

legendeEnergie 0 = "eRest0" 
legendeEnergie 1 = "eRestLocal0" 
legendeEnergie 2 = "eGas0" 
legendeEnergie 3 = "eCoal0" 
legendeEnergie 4 = "eRest1" 
legendeEnergie 5 = "eRestLocal1" 
legendeEnergie 6 = "eGas1" 
legendeEnergie 7 = "eCoal1" 

main :: IO ()
main = do

  IO.hSetEncoding IO.stdout IO.utf8
  
   tabPower <- Table.read "../simulation/maps/power.txt"

   let restPower = CT.convertToSignal2D 
                   (M.lookup "Rest" tabPower)
       localPower = CT.convertToSignal2D 
                   (M.lookup "RestLocal" tabPower)
                   
                   

 
  concurrentlyMany_ $ [
{-    Draw.xterm $ Draw.topologyWithEdgeLabels System.edgeNamesOpt System.topologyOpt,
    putStrLn ("Number of possible flow states: " ++ show (length System.flowStatesOpt)),
    Draw.xterm $ Draw.flowTopologies (take 20 System.flowStatesOpt),
    Draw.xterm $ Draw.sequFlowGraph seqTopo, -}
{-
    Draw.xterm $ Draw.title "Optimise Charging00" $ Draw.sequFlowGraphAbsWithEnv seqTopo $ EqGen.solve seqTopo $ givenOptimiseCharging 1 1 0 0,
    Draw.xterm $ Draw.title "Optimise Charging01" $ Draw.sequFlowGraphAbsWithEnv seqTopo $ EqGen.solve seqTopo $ givenOptimiseCharging 1 1 0 1,
    Draw.xterm $ Draw.title "Optimise Charging10" $ Draw.sequFlowGraphAbsWithEnv seqTopo $ EqGen.solve seqTopo $ givenOptimiseCharging 1 1 1 0,
    Draw.xterm $ Draw.title "Optimise Charging11" $ Draw.sequFlowGraphAbsWithEnv seqTopo $ EqGen.solve seqTopo $ givenOptimiseCharging 1 1 1 1] -}
{-    
    Draw.xterm $ Draw.title "Optimise Discharging00" $ Draw.sequFlowGraphAbsWithEnv seqTopo $ EqGen.solve seqTopo $ givenOptimiseDischarging 1 1 0 0,
    Draw.xterm $ Draw.title "Optimise Discharging01" $ Draw.sequFlowGraphAbsWithEnv seqTopo $ EqGen.solve seqTopo $ givenOptimiseDischarging 1 1 0 1,
    Draw.xterm $ Draw.title "Optimise Discharging10" $ Draw.sequFlowGraphAbsWithEnv seqTopo $ EqGen.solve seqTopo $ givenOptimiseDischarging 1 1 1 0,
    Draw.xterm $ Draw.title "Optimise Discharging11" $ Draw.sequFlowGraphAbsWithEnv seqTopo $ EqGen.solve seqTopo $ givenOptimiseDischarging 1 1 1 1]
-}    
{-    PlotIO.surface "Optimiere Laden - eRestCharge0" DefaultTerm.cons id (const "") varX varY eRestCharge0,
    PlotIO.surface "Optimiere Laden - eRestCharge1" DefaultTerm.cons id (const "") varX varY eRestCharge0,
    PlotIO.surface "Optimiere Laden - eRestLocalCharge0" DefaultTerm.cons id (const "") varX varY eRestLocalCharge0,
    PlotIO.surface "Optimiere Laden - eRestLocalCharge1" DefaultTerm.cons id (const "") varX varY eRestLocalCharge1,
-}                                                                                   
    
    PlotIO.surface "System Efficiency Charging" DefaultTerm.cons id (const "") varX varY etaSysDischarge,
    PlotIO.surface "System Efficiency Discharging" DefaultTerm.cons id (const "") varX varY etaSysCharge,
    PlotIO.surface "System Efficiency Charging and Discharging" DefaultTerm.cons id legend varX varY [etaSysCharge, etaSysDischarge],
    PlotIO.surface "Maximum System Efficiency " DefaultTerm.cons id noLegend varX varY etaSysMax,
    PlotIO.surface "Maximum System Efficiency " DefaultTerm.cons id noLegend varX varY maxEtaSysState,
    PlotIO.xy "Efficiency on Loe" DefaultTerm.cons id noLegend varXSig [etaLoeCharge, etaLoeDischarge], 
    PlotIO.xy "Loe Index" DefaultTerm.cons id noLegend varXSig [indexLoeChargePlot, indexLoeDischargePlot], 
    PlotIO.xy "Loe GasPower" DefaultTerm.cons id noLegend varXSig [powerLoeCharge, powerLoeDischarge]
    
    ]



