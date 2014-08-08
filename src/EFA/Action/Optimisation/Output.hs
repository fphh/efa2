{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
  
module EFA.Action.Optimisation.Output where
import qualified EFA.Action.Optimisation.Cube.Sweep.Plot as SweepPlot
import qualified EFA.Action.Optimisation.Cube.Sweep.Draw as SweepDraw
--import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep
--import qualified EFA.Action.Optimisation.Cube.Sweep.Access as SweepAccess
import qualified EFA.Action.EenergyFlowAnalysis as EFA
import qualified EFA.Action.Simulation as Simulation
import qualified EFA.Flow.Part.Index as Idx
import qualified EFA.Flow.Draw as Draw
import qualified EFA.Action.Optimisation.Process as Process
--import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Action.Optimisation.Loop as Loop

import qualified EFA.Action.EtaFunctions as EtaFunctions
import qualified EFA.Action.DemandAndControl as DemandAndControl
--import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Action.Optimisation.Signal.Plot as OptSignalPlot
import qualified EFA.Action.Optimisation.Signal.Access as OptSignalAccess

import qualified EFA.Action.Optimisation.Sweep as Sweep

import qualified EFA.Action.Flow.Check as ActFlowCheck
import qualified EFA.Action.Flow.Optimality as FlowOpt

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
--import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom
--import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Display as Display
import qualified EFA.Report.FormatValue as FormatValue
import qualified EFA.Graph.Topology.Node as Node
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
--import qualified EFA.Action.Flow.Optimality as ActFlowOpt
import qualified EFA.Flow.Sequence.Algorithm as SeqAlgo

import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Equation.Result as Result

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import Control.Functor.HT (void)
import EFA.Utility.Async (concurrentlyMany_)

import EFA.Utility(Caller,
                   --merror,
                   (|>),
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
data Plot = DontPlot | Dflt | PNG FilePath | SVG FilePath | PS FilePath
data Print = DontPrint | StdOut | Print FilePath
  
data DrawOrPrint = DoP Draw Print
data PlotOrPrint = PoP Plot Print
  
plotAction ::
  Display.C gfx => Plot -> (t -> gfx) -> t -> [IO ()] 
plotAction Dflt toPlotData plot = [void $ Plot.plotSync DefaultTerm.cons  $ toPlotData plot]
plotAction (PNG file) toPlotData plot = [void $ Plot.plotSync (PNG.cons file)  $ toPlotData plot] 
plotAction (SVG file) toPlotData plot = [void $ Plot.plotSync (SVG.cons file) $ toPlotData plot] 
plotAction (PS file) toPlotData plot = [void $ Plot.plotSync (PS.cons file) $ toPlotData plot] 
plotAction DontPlot _ _ = []
drawAction :: 
  Draw -> (t -> [Canonical.DotGraph LazyText.Text]) -> t -> [IO ()]
drawAction DontDraw _ _ = []
drawAction Xterm toDotFunction diagram = map Draw.xterm $ toDotFunction diagram

printAction :: Show a => Print -> a -> [IO ()]
printAction (Print _) _ = error "PrintAction Print to File not implemented yet"
printAction DontPrint _ = []
printAction StdOut x = [print x]


drawOrPrintAction ::
  Show a =>
  DrawOrPrint -> (a -> [Canonical.DotGraph LazyText.Text]) -> a -> [IO ()]
drawOrPrintAction (DoP dA pA) toDotFunction diagram = drawAction dA toDotFunction diagram ++ printAction pA diagram


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
system sysAction sys = 
    (drawOrPrintAction  (topo sysAction) (\x -> [Draw.topology x])  $ Process.accessTopology sys) ++
    (drawOrPrintAction (labTopo sysAction) (\x -> [Draw.labeledTopology x]) $ Process.accessLabledTopology sys) ++
    (drawAction (stateAnalysis sysAction) (\ x -> [Draw.flowTopologiesAbsolute (Process.accessTopology sys) $ 
                                                   StateAnalysis.bruteForce x]) $ Process.accessTopology sys)
    
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
        DV.Singleton sigVec,DV.Storage sigVec (SignalFlow.TimeStep a),
        DV.Length sigVec,
        DV.FromList sigVec) =>
  TestCtrl ->
  Process.TestSet  node inst demDim sigVec a ->
  [DemandAndControl.DemandVar node] ->
  [IO ()]
 
test testCtrl testData demandVars =  
  (plotAction (demandCycle testCtrl) (flip OptSignalPlot.plotDemandCycle demandVars) (Process.accessDemandCycle testData))


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
optiSet caller ctrl oSet = let
  legend = Map.fromList $ zip [0..] $ 
           map DemandAndControl.unDemandVar (Process.accessDemandVars oSet) ++ 
           map DemandAndControl.unControlVar (Process.accessControlVars oSet)
--  legendContr = Map.fromList $ zip [0..] $ Process.accessControlVars oSet
  in
  (plotAction (variation ctrl) (PlotD3.allInOne (PlotD3.labledFrame "DemandVariation") 
                                (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) . fst .
                                       SweepPlot.plotVariation caller ) 
   (Process.accessVariation oSet)) ++
  
  (plotAction (variation ctrl) (PlotD3.allInOne (PlotD3.labledFrame "SearchVariation") 
                               -- (\ _ _ -> id) . snd .
                                (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) . snd .
                                       SweepPlot.plotVariation caller ) 
   (Process.accessVariation oSet))

data SweepCtrl = SweepDont | 
                 SweepDo {drawFlow :: Draw, 
                          plotState :: Plot,
                          plotStatus :: Plot, 
                          plotFlowVariables :: Plot}
                          
sweep ::
  (Show a,DV.Storage demVec a, DV.Length demVec,
   Node.C node,ND.Dimensions dim,DV.Storage srchVec (Interp.Val a),
   FormatValue.FormatValue a,
   DV.LookupUnsafe demVec (TopoQty.Section node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))),
   DV.FromList srchVec,
   Eq (demVec a),
   DV.Zipper srchVec,
   DV.Zipper demVec,
   DV.Walker srchVec,
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec a),
   DV.Storage demVec (TopoQty.Section node (CubeMap.Data (Sweep.Search inst) srchDim srchVec a)),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Maybe Idx.AbsoluteState)),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus),
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage srchVec (Maybe Idx.AbsoluteState),
   DV.LookupUnsafe srchVec a,
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) demVec a a,
   Ord a,
   Show node,
   Show (idx0 node),TopoQty.Lookup idx0,
   Arith.Constant a,
   Atom.C a,
   DV.Walker demVec,
   DV.Storage demVec (TopoQty.Section node (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)),
   DV.Storage demVec (Interp.Val a),
   DV.Storage srchVec a,
   DV.LookupUnsafe srchVec (Interp.Val a),
   DV.Length srchVec,
   ND.Dimensions srchDim,
   PlotCube.ToPlotData  CubeMap.Cube dim (DemandAndControl.Var node) demVec a (Interp.Val a)) =>
  Caller ->
  [idx0 node] ->
  [node] ->
   CubeGrid.Grid (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a ->
  SweepCtrl ->
  Process.SweepResults node inst dim srchDim demVec srchVec a ->
  [IO ()]
sweep caller keyList stoList searchGrid ctrl swp = let
  newCaller = caller |> nc "sweep"
  in
   (drawAction (drawFlow ctrl)  
    (SweepDraw.drawDemandSelection newCaller "SweepFlow DemandEdges" (CubeGrid.Dim [ND.fromList newCaller [Strict.Idx 3,Strict.Idx 7]]))
    (Process.accessSweepFlow swp)) ++
   
   (plotAction (plotFlowVariables ctrl)
    (SweepPlot.plotSweepFlowValues newCaller "FlowVariables" searchGrid 
     TopoQty.lookup 
     CubeGrid.All keyList)
     (Process.accessSweepFlow swp)) ++ 
    
   (plotAction (plotFlowVariables ctrl)
    (SweepPlot.plotSweepFlowValuesPerState newCaller "FlowVariables Per State" searchGrid 
     TopoQty.lookup CubeGrid.All keyList $ Process.accessSweepFlowStatus swp)
     (Process.accessSweepFlow swp))
   
   
   
--   (plotAction (plotFlowVariables ctrl)
--    (SweepPlot.plotStoragePowers newCaller "FlowVariables" searchGrid 
--     CubeGrid.All stoList)
--     (Process.accessSweepEndNodePowers swp))

data EvalCtrl = EvalDont | 
                 EvalDo { plotEta :: Plot,  
                          plotEtaAt :: Plot }
                          
evalSweep ::
  (Ord a,
   Show node,
   Arith.Constant a,
   Atom.C a,
   DV.Walker vec,
   DV.Zipper vec,
   DV.Walker srchVec,
   DV.Storage vec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Maybe Idx.AbsoluteState)),
   DV.Storage vec (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus),
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage srchVec (Maybe Idx.AbsoluteState),
   DV.Storage srchVec (Interp.Val a),
   DV.Storage vec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)),
   DV.Storage vec (Interp.Val a),
   DV.Storage vec (ActFlowCheck.EdgeFlowStatus,
                   FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec a,
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage vec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                     FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage srchVec (CubeGrid.DimIdx srchDim,
                       CubeMap.Cube (Sweep.Demand inst) dim (DemandAndControl.Var node) vec a (ActFlowCheck.EdgeFlowStatus,
                                                                                               FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage vec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                    FlowOpt.OptimalityMeasure (Interp.Val a)))),
   DV.LookupUnsafe srchVec (ActFlowCheck.EdgeFlowStatus,
                            FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.LookupUnsafe vec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                          FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Length srchVec,ND.Dimensions srchDim,
   DV.Storage vec (Interp.Val (Interp.Val a)),
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) vec a (Interp.Val (Interp.Val a)),
   DV.FromList srchVec,
   DV.Storage vec (Maybe Idx.AbsoluteState),
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) vec a (Interp.Val a)) =>
  Caller ->
  CubeGrid.Grid (Sweep.Search inst) srchDim label srchVec a ->
  EvalCtrl ->
  Process.SweepEvaluation node inst dim srchDim vec srchVec a ->
  [IO ()]
evalSweep caller srchGrid ctrl swp = let  
  newCaller = caller |> nc "evalSweep"
  in 
   (plotAction (plotEta ctrl)
    (SweepPlot.plotDemandSweepValue newCaller  "Eta" srchGrid (FlowOpt.unEta2Optimise . FlowOpt.getEta . snd) CubeGrid.All) 
     (Process.accessSweepOptimality swp)) ++
   
   (plotAction (plotEta ctrl)
    (SweepPlot.plotStates newCaller "State" srchGrid )
     (Process.accessSweepOptimality swp)) ++
 
   (plotAction (plotEta ctrl)
    (SweepPlot.plotDemandSweepValue newCaller  "Loss" srchGrid (FlowOpt.unLoss2Optimise . FlowOpt.getLoss . snd) CubeGrid.All) 
     (Process.accessSweepOptimality swp))
   
data OptiCtrl = OptiDont | 
                OptiDo 
                {plotOptimality :: Plot,
                 plotOptEtaPerState :: Plot, 
                 plotEtaOptPerState :: Plot,
                 plotOptIndexPerState :: Plot, 
                 plotOptimalSignalPerState :: Plot}

optPerState ::
  (Ord a,DV.Storage
         vec (ActFlowCheck.EdgeFlowStatus,
              FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage vec (CubeMap.Data (Sweep.Search t1) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                   FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage srchVec a,
   DV.LookupUnsafe srchVec (ActFlowCheck.EdgeFlowStatus,
                            FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Length srchVec,
   ND.Dimensions srchDim,
   DV.Storage vec (CubeGrid.LinIdx,
                   (ActFlowCheck.EdgeFlowStatus,
                    FlowOpt.OptimalityValues a)),
   DV.Storage vec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    FlowOpt.OptimalityValues a))),
   DV.Storage vec (Maybe (CubeGrid.LinIdx,
                          (ActFlowCheck.EdgeFlowStatus,
                           FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage vec a,
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var t) vec a a,
   Show t,
   Arith.Constant a,
   Atom.C a,
   DV.Walker vec,
   DV.Storage vec (CubeGrid.LinIdx,
                   (ActFlowCheck.EdgeFlowStatus,
                    FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage vec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage vec (Interp.Val a),
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var t) vec a (Interp.Val a), 
   Tuple.C a,
   Type.ToDisplayUnit a,
   Type.GetDynamicType a,
   DV.Walker sigVec,
   DV.Storage sigVec (SignalFlow.TimeStep a),
   DV.Storage sigVec a,
   DV.Storage sigVec (Interp.Val a),
   DV.Singleton sigVec,
   DV.Length sigVec,
   DV.FromList sigVec, 
   DV.Storage sigVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage sigVec (ValueState.Map (Interp.Val a))) =>
  Caller ->
  CubeGrid.Grid (Sweep.Search inst) srchDim label srchVec a ->
  OptiCtrl ->
  Process.OptimisationPerState t t1 dim srchDim vec srchVec sigVec a ->
  [IO ()]
optPerState caller srchGrid ctrl opt = 
  let newCaller = caller |> nc "optPerState"
  in (plotAction (plotOptimality ctrl)
     (SweepPlot.plotDemandSweepValue newCaller  "EtaBasedOptimalityValue" srchGrid (FlowOpt.getOptEtaVal . snd) CubeGrid.All) 
     (Process.accessObjectiveFunctionValues opt)) ++

     (plotAction (plotOptEtaPerState ctrl) 
     (SweepPlot.plotOptimalOptimalityValuePerState newCaller "Optimal Eta-Objective Per State" (FlowOpt.getOptEtaVal . snd . snd)) 
     (Process.accessOptimalChoicePerState opt))  ++  
    
     (plotAction (plotEtaOptPerState ctrl)
      (SweepPlot.plotOptimalOptimalityValuePerState newCaller "Optimal Eta Per State" (FlowOpt.getOptEtaVal . snd . snd)) 
     (Process.accessOptimalChoicePerState opt)) ++
                               
     (plotAction (plotOptIndexPerState ctrl)
      (SweepPlot.plotOptimalOptimalityValuePerState newCaller "Optimal Index Per State"
      (\(CubeGrid.LinIdx idx,_) -> Interp.Inter $ Arith.fromInteger $ fromIntegral idx)) 
     (Process.accessOptimalChoicePerState opt)) ++ 
     
     (plotAction (plotOptimalSignalPerState ctrl)
      (OptSignalPlot.plotOptimalSignals "Optimal Signals per State" . OptSignalAccess.optimalityPerStateSignalToSignalMap FlowOpt.getEtaVal)
     (Process.accessOptimalSignalsPerState opt))



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
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.Storage vec (Interp.Val a),
   DV.Storage vec a,
   DV.Singleton vec,
   DV.Length vec,
   DV.FromList vec) =>
  OpCtrl ->
  Process.OptimalOperation id inst vec a ->
  [IO ()]
optimalOperation ctrl opt = 
   (plotAction (plotOptimalControlSignals ctrl) 
   (OptSignalPlot.plotOptimalSignals "Optimal ControlSignals")
    (Process.accessOptimalControlSignals opt)) ++ 
   
   (plotAction (plotOptimalControlSignals ctrl) 
   (OptSignalPlot.plotOptimalSignals "Optimal Storage Powers")
   (Map.map (SignalFlow.map (Maybe.fromMaybe (Interp.Invalid ["plotOptimalOperation"]))) $
      Process.accessOptimalStoragePowers opt))
   

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
   DV.Storage sigVec (SignalFlow.TimeStep a),
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
   SV.Walker sigVec,
   DV.FromList sigVec) =>
  SimCtrl ->
  Process.SimulationAndAnalysis node inst sigVec a ->
 [IO ()]
simulation ctrl sim =  let
  legend = Map.fromList $ zip [0..] $ SignalFlow.getHRecordKeys $ 
           Simulation.accessPowerRecord $ Process.accessSimulation sim
  in 
  
  (drawAction (drawSimulationFlowGraph ctrl) 
   (\x -> [Draw.title "Flow Result from Simulation" $ Draw.flowSection Draw.optionsDefault x]) 
      (Simulation.accessFlowResult $ Process.accessSimulation sim)) ++
  
  (plotAction (plotSimulationPowers ctrl)
   (PlotD2.allInOne (PlotD2.labledFrame "Simulation Power Signals")
     (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) .
     PlotFSignal.plotHRecord) 
      (Simulation.accessPowerRecord $ Process.accessSimulation sim)) ++
  
 (drawAction (drawSequenceFlowGraph ctrl) 
  (\x -> [Draw.title "Sequence Flow Graph from Simulation" $ Draw.seqFlowGraph Draw.optionsDefault $ SeqAlgo.accumulate x]) 
      (EFA.accessSeqFlowGraph $ Process.accessAnalysis sim)) ++
  
 (drawAction (drawStateFlowGraph ctrl) 
  (\x -> [Draw.title "State Flow Graph from Simulation" $ Draw.stateFlowGraph Draw.optionsDefault x]) 
      (EFA.accessStateFlowGraph $ Process.accessAnalysis sim))

loopsIO ::
  (Ord a,
   Show a,
   Show node,
   SV.Walker sigVec,
   SV.Storage sigVec (Interp.Val a),
   SV.FromList sigVec,
   Arith.Constant a,
   Node.C node,
   Atom.C a,
   Tuple.C a,
   Type.ToDisplayUnit a,
   Type.GetDynamicType a,
   FormatValue.FormatValue (sigVec (Interp.Val a)),
   FormatValue.FormatValue a,
   DV.Zipper demVec,
   DV.Walker sigVec,
   DV.Walker srchVec,
   DV.Walker demVec,
   DV.Storage sigVec (Maybe (Interp.Val a)),
   DV.Storage sigVec (ValueState.Map (Interp.Val a)),
   DV.Storage sigVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage sigVec (Interp.Val a),
   DV.Storage sigVec a,
   DV.Storage sigVec (SignalFlow.TimeStep a),
   DV.Storage demVec (ValueState.Map (CubeGrid.LinIdx,
                                      (ActFlowCheck.EdgeFlowStatus,
                                       FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage demVec (CubeGrid.LinIdx,
                      (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec a,
   DV.Storage demVec (Maybe (CubeGrid.LinIdx,
                             (ActFlowCheck.EdgeFlowStatus,
                              FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage demVec (ValueState.Map (CubeGrid.LinIdx,
                                      (ActFlowCheck.EdgeFlowStatus,
                                       FlowOpt.OptimalityValues a))),
   DV.Storage demVec (CubeGrid.LinIdx,
                      (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityValues a)),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec (ActFlowCheck.EdgeFlowStatus,
                      FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage demVec (Maybe Idx.AbsoluteState),
   DV.Storage demVec (Interp.Val (Interp.Val a)),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                       FlowOpt.OptimalityMeasure (Interp.Val a)))),
   DV.Storage srchVec (CubeGrid.DimIdx srchDim,
                       CubeMap.Cube (Sweep.Demand inst) dim (DemandAndControl.Var node) demVec a (ActFlowCheck.EdgeFlowStatus,
                                                                                                  FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                        FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec a,
   DV.Storage demVec (ActFlowCheck.EdgeFlowStatus,
                      FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage demVec (Interp.Val a),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)),
   DV.Storage srchVec (Interp.Val a),
   DV.Storage srchVec (Maybe Idx.AbsoluteState),
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Maybe Idx.AbsoluteState)),
   DV.Singleton sigVec,
   DV.LookupUnsafe srchVec (ActFlowCheck.EdgeFlowStatus,
                            FlowOpt.OptimalityValues (Interp.Val a)),
   DV.LookupUnsafe demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                             FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.LookupUnsafe srchVec (ActFlowCheck.EdgeFlowStatus,
                            FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Length sigVec,
   DV.Length srchVec,
   DV.FromList sigVec,
   DV.FromList srchVec,
   ND.Dimensions srchDim,
   PlotCube.ToPlotData CubeMap.Cube demDim (DemandAndControl.Var node) demVec a a,
   PlotCube.ToPlotData CubeMap.Cube demDim (DemandAndControl.Var node) demVec a (Interp.Val a),
   PlotCube.ToPlotData CubeMap.Cube demDim (DemandAndControl.Var node) demVec a (Interp.Val (Interp.Val a)), 
   DV.Storage srchVec (CubeGrid.DimIdx srchDim,
                       CubeMap.Cube (Sweep.Demand inst) demDim (DemandAndControl.Var node) demVec a (ActFlowCheck.EdgeFlowStatus,
                                                                                                   FlowOpt.OptimalityMeasure (Interp.Val a)))) =>
 
 EvalCtrl ->
 OptiCtrl ->
 OpCtrl ->
 SimCtrl ->
 Process.OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
 [Loop.EtaLoopItem t0 t1 (Process.Res node inst demDim srchDim demVec srchVec sigVec a)] ->
 IO ()  
loopsIO evalCtrl optCtrl opCtrl simCtrl optiSet etaLoop = mapM_ (etaLoopItemIO evalCtrl optCtrl opCtrl simCtrl optiSet) etaLoop  


etaLoopItemIO ::
  (Ord a,
   Show node,
   Show a,
   SV.Walker sigVec,
   SV.Storage sigVec (Interp.Val a),
   SV.FromList sigVec,
   Arith.Constant a,
   Node.C node,
   Atom.C a,
   Tuple.C a,
   Type.ToDisplayUnit a,
   Type.GetDynamicType a,
   FormatValue.FormatValue a,
   FormatValue.FormatValue (sigVec (Interp.Val a)),
   DV.Zipper demVec,
   DV.Walker demVec,
   DV.Walker srchVec,
   DV.Walker sigVec,
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Maybe Idx.AbsoluteState)),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus),
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage srchVec (Maybe Idx.AbsoluteState),
   DV.Storage srchVec (Interp.Val a),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)),
   DV.Storage demVec (Interp.Val a),
   DV.Storage demVec (ActFlowCheck.EdgeFlowStatus,
                   FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec a,
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                     FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage srchVec (CubeGrid.DimIdx srchDim,
                       CubeMap.Cube (Sweep.Demand inst) demDim (DemandAndControl.Var node) demVec a (ActFlowCheck.EdgeFlowStatus,
                                                                                                FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                    FlowOpt.OptimalityMeasure (Interp.Val a)))),
   DV.Storage demVec (Interp.Val (Interp.Val a)),
   DV.Storage demVec (Maybe Idx.AbsoluteState),
   DV.Storage demVec (ActFlowCheck.EdgeFlowStatus,
                   FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                     FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec (CubeGrid.LinIdx,
                   (ActFlowCheck.EdgeFlowStatus,
                    FlowOpt.OptimalityValues a)),
   DV.Storage demVec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    FlowOpt.OptimalityValues a))),
   DV.Storage demVec (Maybe (CubeGrid.LinIdx,
                          (ActFlowCheck.EdgeFlowStatus,
                           FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage demVec a,
   DV.Storage demVec (CubeGrid.LinIdx,
                   (ActFlowCheck.EdgeFlowStatus,
                    FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage sigVec (SignalFlow.TimeStep a),
   DV.Storage sigVec a,
   DV.Storage sigVec (Interp.Val a),
   DV.Storage sigVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage sigVec (ValueState.Map (Interp.Val a)),
   DV.Storage sigVec (Maybe (Interp.Val a)),
   DV.Singleton sigVec,
   DV.LookupUnsafe srchVec (ActFlowCheck.EdgeFlowStatus,
                            FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.LookupUnsafe demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                          FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.LookupUnsafe srchVec (ActFlowCheck.EdgeFlowStatus,
                            FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Length srchVec,
   DV.Length sigVec,
   DV.FromList srchVec,
   DV.FromList sigVec,
   ND.Dimensions srchDim,
   PlotCube.ToPlotData CubeMap.Cube demDim (DemandAndControl.Var node) demVec a (Interp.Val (Interp.Val a)),
   PlotCube.ToPlotData CubeMap.Cube demDim (DemandAndControl.Var node) demVec a (Interp.Val a),
   PlotCube.ToPlotData CubeMap.Cube demDim (DemandAndControl.Var node) demVec a a) =>
  EvalCtrl ->
  OptiCtrl ->
  OpCtrl ->
  SimCtrl ->
  Process.OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  Loop.EtaLoopItem t t1 (Process.Res node inst demDim srchDim demVec srchVec sigVec a) ->
  IO ()
etaLoopItemIO evalCtrl optCtrl opCtrl simCtrl optiSet' (Loop.EtaLoopItem cnt _etaSys _lifeCycleMap balLoop) = do
  print cnt
  mapM_ (balanceLoopItemIO evalCtrl optCtrl opCtrl simCtrl optiSet') balLoop

balanceLoopItemIO ::
  (Ord a,
   Show a,
   Show node,
   SV.Walker sigVec,
   SV.Storage sigVec (Interp.Val a),
   SV.FromList sigVec,
   Arith.Constant a,
   Node.C node,
   Atom.C a,
   Tuple.C a,
   Type.ToDisplayUnit a,
   Type.GetDynamicType a,
   FormatValue.FormatValue (sigVec (Interp.Val a)),
   FormatValue.FormatValue a,
   DV.Zipper demVec,
   DV.Walker sigVec,
   DV.Walker srchVec,
   DV.Walker demVec,
   DV.Storage sigVec (Maybe (Interp.Val a)),
   DV.Storage sigVec (ValueState.Map (Interp.Val a)),
   DV.Storage sigVec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage sigVec (Interp.Val a),
   DV.Storage sigVec a,
   DV.Storage sigVec (SignalFlow.TimeStep a),
   DV.Storage demVec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage demVec (CubeGrid.LinIdx,
                   (ActFlowCheck.EdgeFlowStatus,
                    FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec a,
   DV.Storage demVec (Maybe (CubeGrid.LinIdx,
                          (ActFlowCheck.EdgeFlowStatus,
                           FlowOpt.OptimalityValues (Interp.Val a)))),
   DV.Storage demVec (ValueState.Map (CubeGrid.LinIdx,
                                   (ActFlowCheck.EdgeFlowStatus,
                                    FlowOpt.OptimalityValues a))),
   DV.Storage demVec (CubeGrid.LinIdx,
                   (ActFlowCheck.EdgeFlowStatus,
                    FlowOpt.OptimalityValues a)),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                     FlowOpt.OptimalityValues (Interp.Val a))),
   DV.Storage demVec (ActFlowCheck.EdgeFlowStatus,
                   FlowOpt.OptimalityValues (Interp.Val a)),
   DV.Storage demVec (Maybe Idx.AbsoluteState),
   DV.Storage demVec (Interp.Val (Interp.Val a)),
   DV.Storage demVec (Result.Result (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                                    FlowOpt.OptimalityMeasure (Interp.Val a)))),
   DV.Storage srchVec (CubeGrid.DimIdx srchDim,
                       CubeMap.Cube (Sweep.Demand inst) dim (DemandAndControl.Var node) demVec a (ActFlowCheck.EdgeFlowStatus,
                                                                                                FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                     FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.Storage srchVec (ActFlowCheck.EdgeFlowStatus,
                       FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage srchVec a,
   DV.Storage demVec (ActFlowCheck.EdgeFlowStatus,
                   FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Storage demVec (Interp.Val a),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Interp.Val a)),
   DV.Storage srchVec (Interp.Val a),
   DV.Storage srchVec (Maybe Idx.AbsoluteState),
   DV.Storage srchVec ActFlowCheck.EdgeFlowStatus,
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec ActFlowCheck.EdgeFlowStatus),
   DV.Storage demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (Maybe Idx.AbsoluteState)),
   DV.Singleton sigVec,
   DV.LookupUnsafe srchVec (ActFlowCheck.EdgeFlowStatus,
                            FlowOpt.OptimalityValues (Interp.Val a)),
   DV.LookupUnsafe demVec (CubeMap.Data (Sweep.Search inst) srchDim srchVec (ActFlowCheck.EdgeFlowStatus,
                                                                          FlowOpt.OptimalityMeasure (Interp.Val a))),
   DV.LookupUnsafe srchVec (ActFlowCheck.EdgeFlowStatus,
                            FlowOpt.OptimalityMeasure (Interp.Val a)),
   DV.Length sigVec,
   DV.Length srchVec,
   DV.FromList sigVec,
   DV.FromList srchVec,
   ND.Dimensions srchDim,
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) demVec a a,
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) demVec a (Interp.Val a),
   PlotCube.ToPlotData CubeMap.Cube dim (DemandAndControl.Var node) demVec a (Interp.Val (Interp.Val a))) =>
  EvalCtrl ->
  OptiCtrl ->
  OpCtrl ->
  SimCtrl ->
  Process.OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  Loop.BalanceLoopItem t t1 (Process.Res node inst dim srchDim demVec srchVec sigVec a) ->
  IO ()
balanceLoopItemIO evalCtrl optCtrl opCtrl simCtrl optiSet (Loop.BalanceLoopItem cnt _node _force _step _bal _bestPair result) = do
  let  (Process.Res sweepEval perState optOperation simEfa) = result
  concurrentlyMany_ $ 
  
--    [putStrLn $ Loop.disp cnt]
--    sweep  (nc "balanceLoopItemIO") flowVars [Water] (Process.accessSearchGrid optiSet) sweepCtrl sweep  
    evalSweep (nc "balanceLoopItemIO") (Process.accessSearchGrid optiSet) evalCtrl sweepEval
    ++ optPerState  (nc "balanceLoopItemIO") (Process.accessSearchGrid optiSet) optCtrl perState  
    ++ optimalOperation opCtrl optOperation
    ++ simulation simCtrl simEfa
