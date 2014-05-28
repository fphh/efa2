{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

--import qualified EFA.Data.OrdData as OrdData
import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Vector as DV

import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep
import qualified EFA.Action.Optimisation.Sweep as Sweep
import qualified EFA.Action.Flow.Topology as ActFlowTopo
import EFA.Utility(Caller,
                   --merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
import qualified EFA.Report.Format as Format
import EFA.Application.Optimisation.Params (EtaAssignMap, Name(Name))

import qualified EFA.Data.Collection as Collection
import qualified EFA.Data.Plot.Collection as PlotCollection
import qualified EFA.Data.Plot.D3 as PlotD3 -- TODO -- import Orphan instance
import qualified EFA.Value.Type as Type
import qualified EFA.Flow.Draw as Draw
--import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified EFA.Data.ND.Cube.Map as CubeMap 
import qualified EFA.Data.ND.Cube.Grid as Grid 
--import qualified EFA.Data.Plot as DataPlot 
import qualified EFA.Data.Plot.D3.Cube as CubePlot 

import qualified  EFA.Action.Optimisation.Cube.Solve as CubeSolve
import qualified EFA.IO.TableParserTypes as TPT
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Flow.Topology.Index as TopoIdx

--import EFA.Utility.Async (concurrentlyMany_)
--import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Data.ND as ND
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Application.Utility as AppUt

--import Text.Printf (printf)
import qualified EFA.Application.Optimisation.Params as Params
--import qualified EFA.Application.Type as Type
--import Text.Printf (--printf,
                --    PrintfArg)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
--import qualified Data.GraphViz.Types.Canonical as Canonical
import qualified EFA.Equation.Result as Result

--import qualified EFA.Report.FormatValue as FormatValue
--import qualified EFA.Equation.Arithmetic as Arith

--import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified EFA.IO.TableParser as Table

--import qualified Data.Text.Lazy as LazyText
import qualified EFA.Signal.ConvertTable as CT

import qualified Data.Map as Map
import Data.GraphViz.Attributes.Colors.X11 (X11Color(DarkSeaGreen2))
                                                     -- Lavender))
--import Data.Maybe as Maybe

data Base

modul :: ModuleName
modul = ModuleName "Modules.Setting"

nc :: FunctionName -> Caller
nc = genCaller modul


data Node =
     Coal
   | Gas
   | Water
   | Network
   | LocalNetwork
   | Rest
   | LocalRest
   deriving (Eq, Ord, Enum, Show)

storage, coal, gas, transformer, local, rest :: Name
storage     = Name "storage"
coal        = Name "coal"
gas         = Name "gas"
transformer = Name "transformer"
local       = Name "local"
rest        = Name "rest"

instance Node.C Node where
   display Network = Format.literal "High Voltage"
   display LocalNetwork = Format.literal "Low Voltage"
   display Rest = Format.literal "Residual HV"
   display LocalRest = Format.literal "Residual LV"
   display x = Node.displayDefault x

   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault
   typ t =
      case t of
         Coal -> Node.AlwaysSource
         Gas -> Node.Source
         Water -> Node.storage
         Network -> Node.Crossing
         Rest -> Node.AlwaysSink
         LocalNetwork -> Node.Crossing
         LocalRest -> Node.AlwaysSink
         
topology :: Topo.Topology Node
topology = Topo.plainFromLabeled labeledTopology

labeledTopology :: Topo.LabeledTopology Node
labeledTopology = AppUt.topologyFromLabeledEdges edgeList

edgeList :: AppUt.LabeledEdgeList Node
edgeList = [(Coal, Network, "Coal\\lPlant", "Coal","ElCoal"),
               (Water, Network, "Water\\lPlant","Water","ElWater"),

               (Network, Rest,"100%","toResidualHV","toResidualHV"),

               (Network, LocalNetwork, "Trans-\\lformer", "HighVoltage", "LowVoltage"),
               (Gas, LocalNetwork,"Gas\\lPlant","Gas","ElGas"),
               (LocalNetwork, LocalRest, "100%", "toResidualLV", "toResidualLV")]

scaleTableEta :: Map.Map Params.Name (Double, Double)
scaleTableEta = Map.fromList $
  (storage,     (1, 0.9)) :
  (gas,         (1, 0.7)) :
  (transformer, (3, 0.95)) :
  (coal,        (7, 0.45)) :
  (local,       (3, 1)) :
  (rest,        (3, 1)) :
  []
  
etaAssign ::
   node -> node -> name ->
   (TopoIdx.Position node, (name, name))
etaAssign from to name =
   (TopoIdx.Position from to, (name, name))

etaAssignMap :: EtaAssignMap Node
etaAssignMap = Map.fromList $
   etaAssign Network Water storage :
   etaAssign Network Coal coal :
   etaAssign LocalNetwork Gas gas :
   etaAssign LocalNetwork Network transformer :
   etaAssign LocalRest LocalNetwork local :
   etaAssign Rest Network rest :
   []
   
etaMap :: TPT.Map Double -> Map.Map Params.Name (Params.EtaFunction Double Double)
etaMap tabEta = Map.map Params.EtaFunction $
         Map.mapKeys Params.Name $
         CT.makeEtaFunctions2D
            (Map.mapKeys Params.unName scaleTableEta)
            tabEta

demandGrid :: Grid.Grid (Sweep.Demand Base) ND.Dim2 (TopoIdx.Position Node) [] Double 
demandGrid = Grid.create (nc "Main") [(TopoIdx.ppos LocalRest LocalNetwork,Type.P,[-2..2]),
                    (TopoIdx.ppos Rest Network,Type.P,[-4..4])]

searchGrid :: Grid.Grid (Sweep.Search Base) ND.Dim2 (TopoIdx.Position Node) [] Double 
searchGrid = Grid.create (nc "Main") [(TopoIdx.ppos LocalNetwork Gas,Type.P,[-2.5,-1.5,1.5,2.5]),
                    (TopoIdx.ppos Network Water,Type.P,[-4.5,-3.5,3.5,4.5])]

given :: CubeSweep.Given Base 
         ND.Dim2 ND.Dim2  
         (TopoIdx.Position Node) [] [] Double
given = CubeSweep.generateGiven (nc "Main") demandGrid searchGrid


main :: IO()
main = do

  tabEta <- Table.read "eta.txt"
{-  
  let flow = ActOpt.solve topology etaAssignMap (etaMap tabEta) given 
      flow :: FlowTopo.Section Node (Result.Result (MyDoubleCube))
             
--  let powers = ActOpt.getPowers grid flow         
--  let _topo = FlowTopo.topology  flow  
  
  print given 
  print $ Collection.mapData $ CubeMap.lookupLin (nc "Main") (Grid.LinIdx 0)
-}
--  print given
  
--  let result = CubeMap.map (CubeSolve.solve topology etaAssignMap (etaMap tabEta)) given
  let result = CubeSweep.solve topology etaAssignMap (etaMap tabEta) given    
  
--  print result    
  let Just flow_00 = CubeMap.lookupMaybe (ND.Data $ map Strict.Idx [0,0]) result
  let powers = CubeMap.map (\ flow -> CubeSolve.getPowers searchGrid flow) result
      
  let powerResult = CubeSweep.getDemandSweepPowers (\(Result.Determined (CubeMap.Data x)) -> Result.Determined (DV.maximum x)) result :: Collection.Collection (TopoIdx.Position Node) (Result.Result (CubeMap.Cube (Sweep.Demand Base) ND.Dim2 (TopoIdx.Position Node) [] Double Double))    
      powerResult2 = Collection.mapOrdAndData (\(Result.Determined x) -> x) (\(Result.Determined x) -> x) $ Collection.filter (Result.isDetermined) powerResult :: Collection.Collection (TopoIdx.Position Node) (CubeMap.Cube (Sweep.Demand Base) ND.Dim2 (TopoIdx.Position Node) [] Double Double)
  
  let p_CoalDemand = CubeMap.map (\collection -> flip CubeMap.lookupLinUnsafe (Grid.LinIdx 0) $ (\(Just x) -> x) $
                                  Collection.lookup (TopoIdx.ppos Coal Network) collection) powers
--  print powerResult2
  
  let etaSys = ActFlowTopo.etaSys flow_00
  print "SystemEfficiency"    
  print etaSys     
  
  const Draw.xterm "simulationGraphsSequence"
    $ Draw.bgcolour DarkSeaGreen2
    $ Draw.title "Sequence Flow Graph from Simulation"
    $ Draw.flowSection Draw.optionsDefault flow_00
    
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "P_Coal") PlotD3.plotInfo3lineTitles $ PlotD3.toPlotData (nc "plot") 
    (Just "Test") p_CoalDemand
    
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Result") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") 
    (Just "Power") powerResult2

{-
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Hallo") PlotD3.plotInfo3lineTitles $ PlotD3.toPlotData (nc "plot") (Just "Test") p_lowVoltage
  
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Collection") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") (Just "Collection") demand

--  PlotD3.eachIO DefaultTerm.cons (PlotD3.labledFrame "Collection") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") (Just "Collection") demand
 
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Collection") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") (Just "Collection") powers
-}