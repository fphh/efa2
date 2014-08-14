{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
  
module EFA.Action.Optimisation.Output where
import qualified EFA.Action.Optimisation.Cube.Sweep.Plot as SweepPlot
import qualified EFA.Action.Optimisation.Cube.Sweep.Draw as SweepDraw
--import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep
--import qualified EFA.Action.Optimisation.Cube.Sweep.Access as SweepAccess
import qualified EFA.Action.EnergyFlowAnalysis as EFA
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

--import qualified EFA.Action.Utility as ActUt

import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.List as List
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

data Titles = Experim String
            | StatForc String 
            | NLoop String  
            | BLoop String  
            | ProcStep String  
            | Diagram String  
            | Info String

data Draw = DrwNot | DrwXterm | DrwPDF |DrwPNG | DrwPS
data Plot = DontPlot | Dflt | PNG | SVG  | PS 
data Print = DontPrint | StdOut | Print 
  
data DrawOrPrint = DoP Draw Print
data PlotOrPrint = PoP Plot Print
  
plotAction ::
  Display.C gfx => Plot -> FilePath -> (t -> gfx) -> t -> [IO ()] 
plotAction Dflt _ toPlotData plot = [void $ Plot.plotSync DefaultTerm.cons  $ toPlotData plot]
plotAction (PNG) file toPlotData plot = [void $ Plot.plotSync (PNG.cons $ file ++".png")  $ toPlotData plot] 
plotAction (SVG) file toPlotData plot = [void $ Plot.plotSync (SVG.cons $ file ++".svg") $ toPlotData plot] 
plotAction (PS) file toPlotData plot = [void $ Plot.plotSync (PS.cons $ file ++".ps") $ toPlotData plot] 
plotAction DontPlot _ _ _ = []

drawAction :: 
  Draw -> FilePath -> (t -> [Canonical.DotGraph LazyText.Text]) -> t ->  [IO ()]
drawAction DrwNot _ _ _ = []
drawAction DrwXterm _ toDotFunction diagram = map Draw.xterm $ toDotFunction diagram
drawAction (DrwPDF) file toDotFunction diagram = map (Draw.pdf $ file++".pdf") $ toDotFunction diagram 
drawAction (DrwPS) file toDotFunction diagram = map (Draw.eps $ file++".eps") $ toDotFunction diagram 
drawAction (DrwPNG) file toDotFunction diagram = map (Draw.png $ file++".png") $ toDotFunction diagram 

printAction :: Show a => Print -> FilePath -> a -> [IO ()]
printAction (Print) _ _ = error "PrintAction Print to File not implemented yet"
printAction DontPrint _ _ = []
printAction StdOut x _ = [print x]


drawOrPrintAction ::
  Show a =>
  DrawOrPrint -> FilePath -> (a -> [Canonical.DotGraph LazyText.Text]) -> a -> [IO ()]
drawOrPrintAction (DoP dA pA) file toDotFunction diagram = 
  drawAction dA file toDotFunction diagram ++ printAction pA file diagram


plotOrPrintAction :: 
  (Show a, Display.C gfx) =>
  PlotOrPrint -> FilePath -> (a -> gfx) -> a -> [IO ()]
plotOrPrintAction (PoP dA pA) file toPlotData plot = 
  plotAction dA file toPlotData plot ++ printAction pA file plot


makeTitle :: [String] -> String
makeTitle titles = List.intercalate " - " titles

makeFileName :: FilePath -> [String] -> FilePath
makeFileName [] _ = error "empty file Path"
makeFileName basePath titles | last basePath == '/' = replaceBlancs $ basePath ++ List.intercalate "_" titles
makeFileName basePath titles = replaceBlancs $ basePath ++ "/" ++ List.intercalate "_" titles

replaceBlancs :: String -> String  
replaceBlancs xs = map f xs   
  where f ' ' = '_'
        f x = x 

data SysCtrl = 
  SysDont | 
  SysDo {topo :: DrawOrPrint,
         labTopo :: DrawOrPrint,
         stateAnalysis :: Draw}

system ::
  (Show node,
  Node.C node) =>
  FilePath ->
  [String] ->
  SysCtrl -> 
  Process.System node -> [IO ()]
system path titles sysAction sys = 
  let 
    f str = makeFileName path $ titles ++ [str]
    g str = makeTitle $ titles ++ [str]
  in  
    (drawOrPrintAction (topo sysAction) (f "topology")
     (\x -> [Draw.title (g "topology") $ Draw.topology x])  $ Process.accessTopology sys) ++
    
    (drawOrPrintAction (labTopo sysAction) (f "labledTopology")
     (\x -> [Draw.title (g "labledTopology") $ Draw.labeledTopology x]) $ Process.accessLabledTopology sys) ++
    
    (drawAction (stateAnalysis sysAction)  (f "stateAnalysis")
     (\ x -> [Draw.title (g "stateAnalysis") $ Draw.flowTopologiesAbsolute (Process.accessTopology sys) $ 
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
  FilePath ->
  [String] ->
  SysDataCtrl ->
  Process.SystemData inst node vec a ->
  [IO ()]
sysData path titles action sysDat = 
  let 
    f str = makeFileName path $ titles ++ [str]
    g str = makeTitle $ titles ++ [str]
  in  
 (plotOrPrintAction (rawCurves action) (f "EtaCurvesRaw")
  (PlotD2.allInOne (PlotD2.labledFrame (g "EtaCurvesRaw")) 
   PlotD2.plotInfo3lineTitles . PlotCurve.toPlotDataMap)
  (Process.accessRawEfficiencyCurves sysDat)) ++ 
 
 (plotAction (etaFunctions action) (f "EtaFunctions")
  (PlotD2.allInOne (PlotD2.labledFrame (g "EtaFunctions")) 
   PlotD2.plotInfo3lineTitles . PlotCurve.toPlotDataMap) 
  (EtaFunctions.toCurveMap (Process.accessFunctionPlotAxis sysDat) $ Process.accessFunctionMap sysDat))
 
 

data TestCtrl = 
  TestDont |
  TestDo {demandCycle :: Plot}

test ::
  (Ord a, Show node, Atom.C a, 
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
  FilePath ->
  [String] ->
  TestCtrl ->
  Process.TestSet  node inst demDim sigVec a ->
  [DemandAndControl.DemandVar node] ->
  [IO ()]
 
test path titles testCtrl testData demandVars = 
  let 
--    f str = makeFileName path $ titles ++ [str]
    g str = makeTitle $ titles ++ [str]
  in
  (plotAction (demandCycle testCtrl) (g "DemandCycle")
   (flip OptSignalPlot.plotDemandCycle demandVars) 
   (Process.accessDemandCycle testData))


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
  FilePath ->
  [String] ->
  OptiSetCtrl ->
  Process.OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  [IO ()]
optiSet caller path titles ctrl oSet = let
  legend = Map.fromList $ zip [0..] $ 
           map DemandAndControl.unDemandVar (Process.accessDemandVars oSet) ++ 
           map DemandAndControl.unControlVar (Process.accessControlVars oSet)
  f str = makeFileName path $ titles ++ [str]
  g str = makeTitle $ titles ++ [str]
  in
  (plotAction (variation ctrl) (f "DemandVariation")
   (PlotD3.allInOne (PlotD3.labledFrame (g "DemandVariation")) 
    (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) . fst .
    SweepPlot.plotVariation caller ) 
   (Process.accessVariation oSet)) ++
  
  (plotAction (variation ctrl) (f "SearchVariation")
   (PlotD3.allInOne (PlotD3.labledFrame (g "SearchVariation")) 
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
  FilePath ->
  [String] ->
  [idx0 node] ->
   CubeGrid.Grid (Sweep.Search inst) srchDim (DemandAndControl.Var node) srchVec a ->
  SweepCtrl ->
  Process.SweepResults node inst dim srchDim demVec srchVec a ->
  [IO ()]
sweep caller path titles keyList searchGrid ctrl swp = let
  newCaller = caller |> nc "sweep"
  f str = makeFileName path $ titles ++ [str]
  g str = makeTitle $ titles ++ [str]
  in
   (drawAction (drawFlow ctrl)  (f "Flow DemandEdges")
    (SweepDraw.drawDemandSelection newCaller (g "Flow DemandEdges") (CubeGrid.Dim [ND.fromList newCaller [Strict.Idx 3,Strict.Idx 7]]))
    (Process.accessSweepFlow swp)) ++
   
   (plotAction (plotFlowVariables ctrl) (f "Flow DemandEdges")
    (SweepPlot.plotSweepFlowValues newCaller (g "FlowVariables") searchGrid 
     TopoQty.lookup 
     CubeGrid.All keyList)
     (Process.accessSweepFlow swp)) ++ 
    
   (plotAction (plotFlowVariables ctrl) (f "Flow DemandEdges")
    (SweepPlot.plotSweepFlowValuesPerState newCaller (g "FlowVariables Per State") searchGrid 
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
  FilePath ->
  [String] ->
  CubeGrid.Grid (Sweep.Search inst) srchDim label srchVec a ->
  EvalCtrl ->
  Process.SweepEvaluation node inst dim srchDim vec srchVec a ->
  [IO ()]
evalSweep _ _ _ _ EvalDont _ = []  
evalSweep caller path titles srchGrid ctrl swp = let  
  newCaller = caller |> nc "evalSweep"
  f str = makeFileName path $ titles ++ [str]
  g str = makeTitle $ titles ++ [str]
  in 
   (plotAction (plotEta ctrl) (f "EtaSys")
    (SweepPlot.plotDemandSweepValue newCaller (g "EtaSys") srchGrid (FlowOpt.unEta2Optimise . FlowOpt.getEta . snd) CubeGrid.All) 
     (Process.accessSweepOptimality swp)) ++
   
   (plotAction (plotEta ctrl) (f "State")
    (SweepPlot.plotStates newCaller (g "State") srchGrid )
     (Process.accessSweepOptimality swp)) ++
 
   (plotAction (plotEta ctrl) (f "LossSys")
    (SweepPlot.plotDemandSweepValue newCaller  (g "LossSys") srchGrid (FlowOpt.unLoss2Optimise . FlowOpt.getLoss . snd) CubeGrid.All) 
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
  FilePath ->
  [String] ->
  CubeGrid.Grid (Sweep.Search inst) srchDim label srchVec a ->
  OptiCtrl ->
  Process.OptimisationPerState t t1 dim srchDim vec srchVec sigVec a ->
  [IO ()]
optPerState _ _ _ _ OptiDont _  = []
optPerState caller path titles srchGrid ctrl opt = 
  let newCaller = caller |> nc "optPerState"
      f str = makeFileName path $ titles ++ [str]
      g str = makeTitle $ titles ++ [str]
      
  in (plotAction (plotOptimality ctrl) (f "EtaBasedOptimalityValue")
     (SweepPlot.plotDemandSweepValue newCaller  (g "EtaBasedOptimalityValue") srchGrid 
      (FlowOpt.getOptEtaVal . snd) CubeGrid.All) 
     (Process.accessObjectiveFunctionValues opt)) ++

     (plotAction (plotOptEtaPerState ctrl) (f "Optimal Eta-Objective Per State")
     (SweepPlot.plotOptimalOptimalityValuePerState newCaller (g "Optimal Eta-Objective Per State") 
      (FlowOpt.getOptEtaVal . snd . snd)) 
     (Process.accessOptimalChoicePerState opt))  ++  
    
     (plotAction (plotEtaOptPerState ctrl) (f "Optimal Eta Per State")
      (SweepPlot.plotOptimalOptimalityValuePerState newCaller (g "Optimal Eta Per State")
       (FlowOpt.getOptEtaVal . snd . snd)) 
     (Process.accessOptimalChoicePerState opt)) ++
                               
     (plotAction (plotOptIndexPerState ctrl) (f "Optimal Index Per State")
      (SweepPlot.plotOptimalOptimalityValuePerState newCaller (g "Optimal Index Per State")
      (\(CubeGrid.LinIdx idx,_) -> Interp.Inter $ Arith.fromInteger $ fromIntegral idx)) 
     (Process.accessOptimalChoicePerState opt)) ++ 
     
     (plotAction (plotOptimalSignalPerState ctrl) (f "Optimal Index Per State")
      (OptSignalPlot.plotOptimalSignals "Optimal Signals per State" . 
       OptSignalAccess.optimalityPerStateSignalToSignalMap FlowOpt.getEtaVal)
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
  FilePath ->
  [String] ->
  OpCtrl ->
  Process.OptimalOperation id inst vec a ->
  [IO ()]
optimalOperation _ _ OpDont _ = []
optimalOperation path titles ctrl opt = let
  f str = makeFileName path $ titles ++ [str]
  g str = makeTitle $ titles ++ [str]

  in
   (plotAction (plotOptimalControlSignals ctrl) (f "ControlSignals")
   (OptSignalPlot.plotOptimalSignals (g "ControlSignals"))
    (Process.accessOptimalControlSignals opt)) ++ 
   
   (plotAction (plotOptimalControlSignals ctrl) (f "Storage Powers")
   (OptSignalPlot.plotOptimalSignals (g "Storage Powers"))
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
  FilePath ->
  [String] ->
  SimCtrl ->
  Process.SimulationAndAnalysis node inst sigVec a ->
 [IO ()]
simulation _ _ SimDont _ = [] 
simulation path titles ctrl sim =  
  let
    legend = Map.fromList $ zip [0..] $ SignalFlow.getHRecordKeys $ 
           Simulation.accessPowerRecord $ Process.accessSimulation sim
    
    f str = makeFileName path $ titles ++ [str]
    g str = makeTitle $ titles ++ [str]
  in 
  
  (drawAction (drawSimulationFlowGraph ctrl) (f "Flow Result")
   (\x -> [Draw.title (g "Flow Result") $ Draw.flowSection Draw.optionsDefault x]) 
      (Simulation.accessFlowResult $ Process.accessSimulation sim)) ++
  
  (plotAction (plotSimulationPowers ctrl) (f "Power Signals")
   (PlotD2.allInOne (PlotD2.labledFrame  (g "Power Signals"))
     (\ idx _ -> LineSpec.title $ show $ legend Map.! idx) .
     PlotFSignal.plotHRecord) 
      (Simulation.accessPowerRecord $ Process.accessSimulation sim)) ++
  
  (drawAction (drawSequenceFlowGraph ctrl) (f "Sequence Flow Graph")
   (\x -> [Draw.title (g "Sequence Flow Graph") $ Draw.seqFlowGraph Draw.optionsDefault $ SeqAlgo.accumulate x]) 
      (EFA.accessSeqFlowGraph $ Process.accessAnalysis sim)) ++
  
  (drawAction (drawStateFlowGraph ctrl)  (f "State Flow Graph")
   (\x -> [Draw.title (g "State Flow Graph") $ Draw.stateFlowGraph Draw.optionsDefault x]) 
      (EFA.accessStateFlowGraph $ Process.accessAnalysis sim))

loopsIO ::
  (Ord a, Show t0,
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
 FilePath ->
 [String] ->
 EvalCtrl ->
 OptiCtrl ->
 OpCtrl ->
 SimCtrl ->
 Process.System node ->
 Process.OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
 [Loop.EtaLoopItem t0 t1 t2  
  (Process.SweepEvaluation node inst demDim srchDim demVec srchVec a) 
  (Process.Res node inst demDim srchDim demVec srchVec sigVec a)] ->
 IO ()  
loopsIO path titles evalCtr optCtr opCtr simCtr sys optiS etaLoop = 
  mapM_ (etaLoopItemIO path titles evalCtr optCtr opCtr simCtr sys optiS) etaLoop  


etaLoopItemIO ::
  (Ord a,
   Show t,
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
  FilePath ->
  [String] ->
  EvalCtrl ->
  OptiCtrl ->
  OpCtrl ->
  SimCtrl ->
  Process.System node ->
  Process.OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  Loop.EtaLoopItem t t1 t2
  (Process.SweepEvaluation node inst demDim srchDim demVec srchVec a) 
  (Process.Res node inst demDim srchDim demVec srchVec sigVec a) ->
  IO ()
etaLoopItemIO path titles evalCtr optCtr opCtr simCtr sys optiS (Loop.EtaLoopItem cnt _etaSys _lifeCycleMap sweepEval balLoop) = do
  let newTitles = titles ++ [show cnt]
  print cnt
  concurrentlyMany_ $ evalSweep (nc "balanceLoopItemIO") path
                          newTitles (Process.accessSearchGrid optiS) evalCtr sweepEval
  mapM_ (balanceLoopItemIO path newTitles optCtr opCtr simCtr sys optiS) balLoop   
 
balanceLoopItemIO ::
  (Ord a,
   Show t,
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
  FilePath ->
  [String] ->
  OptiCtrl ->
  OpCtrl ->
  SimCtrl ->
  Process.System node ->
  Process.OptiSet node inst demDim srchDim demVec srchVec sigVec a ->
  Loop.BalanceLoopItem t t1 (Process.Res node inst dim srchDim demVec srchVec sigVec a) ->
  IO ()
balanceLoopItemIO path titles optCtr opCtr simCtr _sys optiS (Loop.BalanceLoopItem cnt _node _force _step _bal _bestPair result) = do
  let  (Process.Res perState optOperation simEfa) = result
       newTitles = titles ++ [show cnt]
  concurrentlyMany_ $ 
   optPerState  (nc "balanceLoopItemIO") path newTitles (Process.accessSearchGrid optiS) optCtr perState  
    ++ optimalOperation path newTitles opCtr optOperation
    ++ simulation path newTitles simCtr simEfa
