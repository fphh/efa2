{-# LANGUAGE FlexibleContexts #-}
  
module EFA.Action.Optimisation.Output where

import qualified EFA.Flow.Draw as Draw
import qualified EFA.Action.Optimisation.Process as Process
import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Action.EtaFunctions as EtaFunctions
import qualified EFA.Action.DemandAndControl as DemandAndControl
import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Action.Optimisation.Signal.Plot as OptSignalPlot
--import qualified EFA.Data.Plot.D2.FlowSignal as PlotFSignal
import qualified EFA.Data.Plot.D2.FlowSignal as PlotFSignal
import qualified EFA.Data.Plot.D2.Curve as PlotCurve
import qualified EFA.Data.Plot.D2 as PlotD2
import qualified EFA.Data.Axis.Strict as Strict
import qualified Data.GraphViz.Types.Canonical as Canonical
import qualified Data.Text.Lazy as LazyText

import qualified EFA.Data.ND as ND
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Data.Vector as DV
import qualified EFA.Value.Type as Type

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.PostScript as PS
import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Frame as Frame


import Control.Functor.HT (void)

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
  
  
plotAction Dflt toPlotData plot = [void $ Plot.plotSync DefaultTerm.cons  $ toPlotData plot]
plotAction (PNG file) toPlotData plot = [void $ Plot.plotSync (PNG.cons file)  $ toPlotData plot] 
plotAction (SVG file) toPlotData plot = [void $ Plot.plotSync (SVG.cons file) $ toPlotData plot] 
plotAction (PS file) toPlotData plot = [void $ Plot.plotSync (PS.cons file) $ toPlotData plot] 

drawAction DontDraw _ _ = []
drawAction Xterm toDotFunction diagram = [Draw.xterm $ toDotFunction diagram]

printAction DontPrint _ = []
printAction StdOut x = [print x]

-- drawOrPrintAction :: DrawOrPrint -> Canonical.DotGraph LazyText.Text -> [IO ()]
drawOrPrintAction (DoP dA pA) toDotFunction diagram= drawAction dA toDotFunction diagram ++ printAction pA diagram

plotOrPrintAction (PoP dA pA) toPlotData plot = plotAction dA toPlotData plot ++ printAction pA plot


data SysOpts = 
  SysDont | 
  SysDo {topo :: DrawOrPrint,
         labTopo :: DrawOrPrint,
         stateAnalysis :: Draw}

system sysAction system = 
    (drawOrPrintAction  (topo sysAction) Draw.topology  $ Process.accessTopology system) ++
    (drawOrPrintAction (labTopo sysAction) Draw.labeledTopology $ Process.accessLabledTopology system) ++
    (drawAction (stateAnalysis sysAction) (\ x -> Draw.flowTopologies $ StateAnalysis.advanced x) $ Process.accessTopology system)
    
data SysData = SysDataDont | 
               SysDataDo {rawCurves :: PlotOrPrint,
                          etaFunctions :: Plot}

sysData action sysDat = 
 (plotOrPrintAction (rawCurves action) (PlotD2.allInOne (PlotD2.labledFrame "EtaCurvesRaw") 
                                       PlotD2.plotInfo3lineTitles . PlotCurve.toPlotDataMap)
 (Process.accessRawEfficiencyCurves sysDat)) ++ 
 
 (plotAction (etaFunctions action) (PlotD2.allInOne (PlotD2.labledFrame "EtaFunctions") 
                                       PlotD2.plotInfo3lineTitles . PlotCurve.toPlotDataMap) 
 (EtaFunctions.toCurveMap (Process.accessFunctionPlotAxis sysDat) $ Process.accessFunctionMap sysDat))
 
 

data TestOpt = 
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
  TestOpt ->
  Process.TestSet  node inst demDim sigVec a ->
  [DemandAndControl.DemandVar node] ->
  [IO ()]
 
test testOpts testData demandVars =  
  (plotAction (demandCycle testOpts) (\x -> PlotD2.allInOne (PlotD2.labledFrame "DemandCycle")  (\ _ _ -> id) 
   $ OptSignalPlot.plotDemandCycle x demandVars) (Process.accessDemandCycle testData))

{-
data OptiSet = OptiSetDont | OptiSetDontDo {variation :: DrawOrPlot}

optiSet opts optiSet = 
  (drawOrPlot (variation opts) ( ) (accessVariation optiSet)
-}  

--    [draw  (Draw.topology $ Process.accessTopology system) $ Xterm]
--    [Draw.xterm $ Draw.topology $ Process.accessTopology system]
---     draw (labTopo sysAction) $ Draw.labeledTopology $ Process.accessTopology system]
--  term $ Draw.flowTopologies $ StateAnalysis.advanced $ Params.systemTopology params 
--  draw $ Draw.labeledTopology $ Process.accessLabledTopology system

   
{-
printSystem :: Process.System -> IO()
printSystem Process.System 
       
  
  topologyWithStates ::
  Node.C node =>
  (Canonical.DotGraph
   LazyText.Text -> IO ())
  -> Params.System node a -> IO ()
topologyWithStates term params = concurrentlyMany_ [
  
labledTopology = term $ Draw.labeledTopology $ Params.labeledTopology params,
  term $ Draw.flowTopologies $ StateAnalysis.advanced $ Params.systemTopology params ]
-}
  