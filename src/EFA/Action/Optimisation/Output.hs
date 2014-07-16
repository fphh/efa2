{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
  
module EFA.Action.Optimisation.Output where

import qualified EFA.Action.Optimisation.Cube.Sweep.Plot as SweepPlot
import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep
import qualified EFA.Action.Optimisation.Cube.Sweep.Access as SweepAccess
import qualified EFA.Action.EenergyFlowAnalysis as EFA
import qualified EFA.Action.Simulation as Simulation
import qualified EFA.Flow.Part.Index as Idx
import qualified EFA.Flow.Draw as Draw
import qualified EFA.Action.Optimisation.Process as Process
import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Action.EtaFunctions as EtaFunctions
import qualified EFA.Action.DemandAndControl as DemandAndControl
import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Action.Optimisation.Signal.Plot as OptSignalPlot
import qualified EFA.Action.Optimisation.Sweep as Sweep

import qualified EFA.Action.Flow.Check as ActFlowCheck
import qualified EFA.Action.Flow.Optimality as ActFlowOpt

import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.Collection as Collection
import qualified EFA.Data.OD.Signal.Flow as SignalFlow

import qualified EFA.Data.Plot.D2.FlowSignal as PlotFSignal
import qualified EFA.Data.Plot.D2.Curve as PlotCurve
import qualified EFA.Data.Plot.D2 as PlotD2
import qualified EFA.Data.Plot.D3 as PlotD3
import qualified EFA.Data.Plot.D3.Cube as PlotCube
import qualified EFA.Signal.Vector as SV
import qualified EFA.Value.State as ValueState

import qualified EFA.Data.Axis.Strict as Strict
import qualified Data.GraphViz.Types.Canonical as Canonical

import qualified Data.Text.Lazy as LazyText

import qualified EFA.Data.ND.Cube.Map as CubeMap
import qualified EFA.Data.ND.Cube.Grid as CubeGrid

import qualified EFA.Data.ND as ND
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Data.Vector as DV
import qualified EFA.Value.Type as Type

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.PostScript as PS
import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Display as Display
import qualified EFA.Report.FormatValue as FormatValue
import qualified EFA.Graph.Topology.Node as Node
import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Equation.Result as Result

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Control.Functor.HT (void)

import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "EFA.Action.Optimisation.OutPut"

nc :: FunctionName -> Caller
nc = genCaller modul

data File = RelativeFilePath | AbsoluteFilePath

--1 alle Aktionen aller Pakete solln in eine Liste können
--2 typ-sicherheit für mögliche Aktionen (Print, Draw, ..)
--3 keine, ein, zwei aktionen 
--4 Terminal; FilePath 
--4 Optionen übergeben 

data Draw = DontDraw | Xterm
data Plot = Dflt | PNG FilePath | SVG FilePath | PS FilePath
data Print = DontPrint | StdOut | Print FilePath
  
data DrawOrPrint = DoP Draw Print
data PlotOrPrint = PoP Plot Print
  
plotAction ::
  Display.C gfx => Plot -> (t -> gfx) -> t -> [IO ()] 
plotAction Dflt toPlotData plot = [void $ Plot.plotSync DefaultTerm.cons  $ toPlotData plot]
plotAction (PNG file) toPlotData plot = [void $ Plot.plotSync (PNG.cons file)  $ toPlotData plot] 
plotAction (SVG file) toPlotData plot = [void $ Plot.plotSync (SVG.cons file) $ toPlotData plot] 
plotAction (PS file) toPlotData plot = [void $ Plot.plotSync (PS.cons file) $ toPlotData plot] 

drawAction :: 
  Draw -> (t -> Canonical.DotGraph LazyText.Text) -> t -> [IO ()]
drawAction DontDraw _ _ = []
drawAction Xterm toDotFunction diagram = [Draw.xterm $ toDotFunction diagram]

printAction :: Show a => Print -> a -> [IO ()]
printAction DontPrint _ = []
printAction StdOut x = [print x]

drawOrPrintAction ::
  Show a =>
  DrawOrPrint -> (a -> Canonical.DotGraph LazyText.Text) -> a -> [IO ()]
drawOrPrintAction (DoP dA pA) toDotFunction diagram= drawAction dA toDotFunction diagram ++ printAction pA diagram

plotOrPrintAction :: 
  (Show a, Display.C gfx) =>
  PlotOrPrint -> (a -> gfx) -> a -> [IO ()]
plotOrPrintAction (PoP dA pA) toPlotData plot = plotAction dA toPlotData plot ++ printAction pA plot


data SysCtrl = 
  SysDont | 
  SysDo {topo :: DrawOrPrint,
         labTopo :: DrawOrPrint,
         stateAnalysis :: Draw}

system ::
  (Show node,
  Node.C node) =>
  SysCtrl -> Process.System node -> [IO ()]
system sysAction system = 
    (drawOrPrintAction  (topo sysAction) Draw.topology  $ Process.accessTopology system) ++
    (drawOrPrintAction (labTopo sysAction) Draw.labeledTopology $ Process.accessLabledTopology system) ++
    (drawAction (stateAnalysis sysAction) (\ x -> Draw.flowTopologies $ StateAnalysis.advanced x) $ Process.accessTopology system)
    
data SysDataCtrl = SysDataDont | 
               SysDataDo {rawCurves :: PlotOrPrint,
                          etaFunctions :: Plot}

sysData ::
  (Ord a,
   Show node,
   Show a,
   Show (vec a),
   Arith.Constant a,
   Tuple.C a,
   Atom.C a,
   Type.ToDisplayUnit a,
   Type.GetDynamicType a,
   DV.Walker vec,
   DV.Storage vec a,
   DV.Storage vec (Interp.Val a),
   DV.Singleton vec,
   DV.Length vec,
   DV.FromList vec) =>
  SysDataCtrl ->
  Process.SystemData inst node vec a ->
  [IO ()]
sysData action sysDat = 
 (plotOrPrintAction (rawCurves action) (PlotD2.allInOne (PlotD2.labledFrame "EtaCurvesRaw") 
                                       PlotD2.plotInfo3lineTitles . PlotCurve.toPlotDataMap)
 (Process.accessRawEfficiencyCurves sysDat)) ++ 
 
 (plotAction (etaFunctions action) (PlotD2.allInOne (PlotD2.labledFrame "EtaFunctions") 
                                       PlotD2.plotInfo3lineTitles . PlotCurve.toPlotDataMap) 
 (EtaFunctions.toCurveMap (Process.accessFunctionPlotAxis sysDat) $ Process.accessFunctionMap sysDat))
 
 

data TestCtrl = 
  TestDont |
  TestDo {demandCycle :: Plot}

test ::(Ord a, Show node, Atom.C a, 
        Ord node,
        Arith.Constant a,
        Tuple.C a,
        Type.ToDisplayUnit a,
        Type.GetDynamicType a,
        DV.Walker sigVec,
        DV.Storage sigVec a,
        DV.Storage sigVec (ND.Data demDim a),
        DV.Singleton sigVec,
        DV.Length sigVec,
        DV.FromList sigVec) =>
  TestCtrl ->
  Process.TestSet  node inst demDim sigVec a ->
  [DemandAndControl.DemandVar node] ->
  [IO ()]
 
test testCtrl testData demandVars =  
  (plotAction (demandCycle testCtrl) (\x -> PlotD2.allInOne (PlotD2.labledFrame "DemandCycle")  (\ _ _ -> id) 
   $ OptSignalPlot.plotDemandCycle x demandVars) (Process.accessDemandCycle testData))


data OptiSetCtrl = OptiSetDont | OptiSetDo {variation :: Plot}

optiSet ::
  (Eq (demVec a),
   Eq (srchVec a),
   Ord node,
   Ord a,
   Show node,
   Arith.Constant a,
   Atom.C a,
   DV.Walker demVec,
   DV.Storage demVec (Collection.Collection (DemandAndControl.Var node) 
                      (CubeMap.Cube (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a (Interp.Val a))),
   DV.Storage demVec (CubeMap.Cube (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a (Interp.Val a)),
   DV.Storage demVec (Interp.Val a),
   DV.LookupUnsafe demVec (Collection.Collection (DemandAndControl.Var node) 
                           (CubeMap.Cube (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a (Interp.Val a))),
   DV.LookupUnsafe srchVec (Interp.Val a),
   PlotCube.ToPlotData CubeMap.Cube demDim (DemandAndControl.Var node) demVec a (Interp.Val a),
   PlotCube.ToPlotData CubeMap.Cube srchDim (DemandAndControl.Var node) srchVec a (Interp.Val a)) =>
  Caller ->
  OptiSetCtrl ->
  Process.OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  [IO ()]
optiSet caller ctrl optiSet = let
  legend = Map.fromList $ zip [0..] $ 
           map DemandAndControl.unDemandVar (Process.accessDemandVars optiSet) ++ 
           map DemandAndControl.unControlVar (Process.accessControlVars optiSet)
--  legendContr = Map.fromList $ zip [0..] $ Process.accessControlVars optiSet
  in
  (plotAction (variation ctrl) (PlotD3.allInOne (PlotD3.labledFrame "DemandVariation") 
                                (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) . fst .
                                       SweepPlot.plotVariation caller ) 
   (Process.accessVariation optiSet)) ++
  
  (plotAction (variation ctrl) (PlotD3.allInOne (PlotD3.labledFrame "SearchVariation") 
                               -- (\ _ _ -> id) . snd .
                                (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) . snd .
                                       SweepPlot.plotVariation caller ) 
   (Process.accessVariation optiSet))

data SweepCtrl = SweepDont | 
                 SweepDo {drawFlow :: Draw, 
                          plotState :: Plot,
                          plotStatus :: Plot}
                          
--TODO:: Type-Safe Indx (using inst)
sweep ::
  (Show a,DV.Storage vec a, DV.Length vec,
   Node.C node,
   FormatValue.FormatValue a,
   DV.Storage srchVec (Interp.Val a),
   DV.LookupUnsafe vec (TopoQty.Section node (Result.Result 
                                              (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)))),
   DV.FromList srchVec) =>
  CubeGrid.LinIdx ->
  SweepCtrl ->
  Process.SweepResults node inst dim srchDim vec srchVec a ->
  [IO ()]
sweep linIdxDem ctrl sweep = 
  let 
    demGrid = CubeMap.getGrid $ CubeSweep.unFlowResult $ Process.accessSweepFlow sweep
    fromLin linIdx = CubeGrid.fromLinear demGrid linIdx
    toLin idx = CubeGrid.toLinear demGrid idx
  in 
   (drawAction (drawFlow ctrl)  
    (Draw.title ("LinIdx: " ++ show linIdxDem ++ "Idx: " ++ (show $ fromLin linIdxDem)) . 
     Draw.flowSection  Draw.optionsDefault) 
   (flip CubeMap.lookupLinUnsafe linIdxDem $ CubeSweep.unFlowResult $ Process.accessSweepFlow sweep))
   

data EvalCtrl = EvalDont | 
                 EvalDo { plotEta :: Plot }
                          
evalSweep :: forall node vec srchVec srchDim dim label inst a b.
  (Ord a,
   Show node,
   Arith.Constant a,
   Atom.C a,
   DV.Walker vec,
   DV.Walker srchVec,
   DV.Storage vec (Interp.Val a),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus,
                   ActFlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec a,
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       ActFlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage vec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                     ActFlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage srchVec (CubeGrid.DimIdx srchDim,
                       CubeMap.Cube (Sweep.Demand inst) dim (DemandAndControl.Var node) vec a (ActFlowCheck.EdgeFlowStatus,
                                                                                               ActFlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                    ActFlowOpt.OptimalityMeasure (Interp.Val a)))),
   DV.LookupUnsafe srchVec (ActFlowCheck.EdgeFlowStatus,
                            ActFlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.LookupUnsafe vec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                          ActFlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Length srchVec,
   DV.FromList srchVec,
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) vec a (Interp.Val a)) =>
  Caller ->
  CubeGrid.Grid (Sweep.Search inst) srchDim label srchVec a ->
  EvalCtrl ->
  Process.SweepEvaluation node inst dim srchDim vec srchVec a ->
  [IO ()]
evalSweep caller srchGrid ctrl sweep = let  
  newCaller = caller |> nc "evalSweep"
  in 
   (plotAction (plotEta ctrl)
    (PlotD3.allInOne (PlotD3.labledFrame "SweepEta") 
      (\ _ _ -> LineSpec.title "") . 
      SweepPlot.plotEvalSweepStackValue newCaller srchGrid (ActFlowOpt.unEta2Optimise . ActFlowOpt.getEta . snd)) 
     (Process.accessSweepOptimality sweep))
   
 {-  (plotAction (plotEta ctrl)
    (PlotD3.allInOne (PlotD3.labledFrame "FlowState") 
      (\ _ _ -> LineSpec.title "") .  
      SweepPlot.plotStates newCaller srchGrid) 
      (Process.accessSweepOptimality sweep)) -}
   
   

-- data EvalCtrl = EvalDont | EvalDo {variation :: Plot}
 
-- eval ctrl eval =                            
  
                           
data OptiCtrl = OptiDont | 
                OptiDo 
                {plotOptEtaPerState :: Plot, 
                 plotEtaOptPerState :: Plot,
                 plotOptIndexPerState :: Plot}

optPerState ::
  (Ord a,
   DV.Storage vec (CubeGrid.LinIdx,
                   (ActFlowCheck.EdgeFlowStatus,
                    ActFlowOpt.OptimalityValues a)),
   DV.Storage vec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    ActFlowOpt.OptimalityValues a))),
   DV.Storage vec (Maybe (CubeGrid.LinIdx,
                          (ActFlowCheck.EdgeFlowStatus,
                           ActFlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage vec a,
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var t) vec a a,
   Show t,
   Arith.Constant a,
   Atom.C a,
   DV.Walker vec,
   DV.Storage vec (CubeGrid.LinIdx,
                   (ActFlowCheck.EdgeFlowStatus,
                    ActFlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage vec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    ActFlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage vec (Interp.Val a),
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var t) vec a (Interp.Val a)) =>
  Caller ->
  OptiCtrl ->
  Process.OptimisationPerState t t1 dim srchDim vec srchVec sigVec a ->
  [IO ()]
optPerState caller ctrl opt = 
  let newCaller = caller |> nc "optPerState"
      states = Map.fromList $ zip [0..] $
               SweepAccess.getAllStates $ CubeSweep.unOptimalChoicePerState $ 
               Process.accessOptimalChoicePerState opt
  in (plotAction (plotOptEtaPerState ctrl) 
     (PlotD3.allInOne (PlotD3.labledFrame "Optimal Eta-Objective Per State") 
      (\ idx _ -> LineSpec.title $ show $ states Map.! idx) . 
      SweepPlot.plotOptimalOptimalityValuePerState newCaller (ActFlowOpt.getOptEtaVal . snd . snd)) 
     (Process.accessOptimalChoicePerState opt))  ++  
    
     (plotAction (plotEtaOptPerState ctrl) 
     (PlotD3.allInOne (PlotD3.labledFrame "Optimal Eta Per State") 
      (\ idx _ -> LineSpec.title $ show $ states Map.! idx) . 
      SweepPlot.plotOptimalOptimalityValuePerState newCaller (ActFlowOpt.getOptEtaVal . snd . snd)) 
     (Process.accessOptimalChoicePerState opt)) ++
                               
     (plotAction (plotOptIndexPerState ctrl) 
     (PlotD3.allInOne (PlotD3.labledFrame "Optimal Index Per State") 
      (\ idx _ -> LineSpec.title $ show $ states Map.! idx) . 
      SweepPlot.plotOptimalOptimalityValuePerState newCaller 
      (\(CubeGrid.LinIdx idx,_) -> Interp.Inter $ Arith.fromInteger $ fromIntegral idx)) 
     (Process.accessOptimalChoicePerState opt))


data OpCtrl = OpDont | OpDo 
                       {plotOptimalControlSignals :: Plot, 
                        plotOptimalStoragePowers :: Plot}  

optimalOperation ::
  (Ord a,
   Show id,
   Arith.Constant a,
   Tuple.C a,
   Atom.C a,
   Type.ToDisplayUnit a,
   Type.GetDynamicType a,
   DV.Walker vec,
   DV.Storage vec (Maybe (Interp.Val a)),
   DV.Storage vec (Interp.Val a),
   DV.Storage vec a,
   DV.Singleton vec,
   DV.Length vec,
   DV.FromList vec) =>
  OpCtrl ->
  Process.OptimalOperation id inst vec a ->
  [IO ()]
optimalOperation ctrl opt = let
  legend = Map.fromList $ zip [0..] $ Map.keys $ 
           OptSignal.unOptimalControlSignals $ Process.accessOptimalControlSignals opt
  in 
   (plotAction (plotOptimalControlSignals ctrl) 
    (PlotD2.allInOne (PlotD2.labledFrame "Optimal ControlSignals")
     (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) .
     PlotFSignal.plotSignalMap) 
    (OptSignal.unOptimalControlSignals $ Process.accessOptimalControlSignals opt)) ++ 
   
   (plotAction (plotOptimalControlSignals ctrl) 
    (PlotD2.allInOne (PlotD2.labledFrame "Optimal Storage Powers")
     (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) .
     PlotFSignal.plotSignalMap) 
    (Map.map (SignalFlow.map (Maybe.fromMaybe (Interp.Invalid ["plotOptimalOperation"]))) $
      OptSignal.unOptimalStoragePowers $ Process.accessOptimalStoragePowers opt))
   

data SimCtrl = 
  SimDont |   
  SimDo 
  {drawSimulationFlowGraph :: Draw,
   plotSimulationPowers :: Plot, 
   drawSequenceFlowGraph :: Draw,               
   drawStateFlowGraph :: Draw}    

simulation :: 
  (Ord a,
   Show a,
   Show node,
   SV.Storage sigVec (Interp.Val a),
   SV.FromList sigVec,
   Arith.Constant a,
   Node.C node,
   Tuple.C a,
   Atom.C a,
   Type.ToDisplayUnit a,
   Type.GetDynamicType a,
   FormatValue.FormatValue a,
   FormatValue.FormatValue (sigVec (Interp.Val a)),
   DV.Walker sigVec,
   DV.Storage sigVec (Interp.Val a),
   DV.Storage sigVec a,
   DV.Singleton sigVec,
   DV.Length sigVec,
   DV.FromList sigVec) =>
  SimCtrl ->
  Process.SimulationAndAnalysis node inst sigVec a ->
 [IO ()]
simulation ctrl sim =  let
  legend = Map.fromList $ zip [0..] $ SignalFlow.getHRecordKeys $ 
           Simulation.accessPowerRecord $ Process.accessSimulation sim
  in 
  
  (drawAction (drawSimulationFlowGraph ctrl) (Draw.flowSection Draw.optionsDefault) 
      (Simulation.accessFlowResult $ Process.accessSimulation sim)) ++
  
  (plotAction (plotSimulationPowers ctrl)
   (PlotD2.allInOne (PlotD2.labledFrame "Simulation Power Signals")
     (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) .
     PlotFSignal.plotHRecord) 
      (Simulation.accessPowerRecord $ Process.accessSimulation sim)) ++
  
 (drawAction (drawSequenceFlowGraph ctrl) (Draw.seqFlowGraph Draw.optionsDefault) 
      (EFA.accessSeqFlowGraph $ Process.accessAnalysis sim)) ++
  
 (drawAction (drawStateFlowGraph ctrl) (Draw.stateFlowGraph Draw.optionsDefault) 
      (EFA.accessStateFlowGraph $ Process.accessAnalysis sim))
