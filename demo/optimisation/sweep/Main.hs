{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

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

import qualified  EFA.Action.Optimisation as ActOpt
import qualified EFA.IO.TableParserTypes as TPT
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Flow.Topology.Index as TopoIdx

--import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Flow.Topology.Quantity as FlowTopo
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
import qualified EFA.Equation.Arithmetic as Arith

--import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D

import qualified EFA.IO.TableParser as Table

--import qualified Data.Text.Lazy as LazyText
import qualified EFA.Signal.ConvertTable as CT

import qualified Data.Map as Map
import Data.GraphViz.Attributes.Colors.X11 (X11Color(DarkSeaGreen2))
                                                     -- Lavender))

modul :: ModuleName
modul = ModuleName "Modules.Setting"

nc :: FunctionName -> Caller
nc = genCaller modul

data Base

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

grid :: Grid.Grid Base ND.Dim2 (TopoIdx.Position Node) [] Double 
grid = Grid.create (nc "Main") [(TopoIdx.ppos LocalRest LocalNetwork,Type.P,[-200..200]),
                    (TopoIdx.ppos Rest Network,Type.P,[-400..400])]

p_lowVoltage :: CubeMap.Cube Base ND.Dim2 (TopoIdx.Position Node) [] Double Double    
p_lowVoltage = CubeMap.generateWithGrid (\(ND.Data [x,_]) -> x) grid
                    
p_midVoltage ::  CubeMap.Cube Base ND.Dim2 (TopoIdx.Position Node) [] Double Double    
p_midVoltage = CubeMap.generateWithGrid (\(ND.Data [_,y]) -> y) grid

p_water ::  CubeMap.Cube Base ND.Dim2 (TopoIdx.Position Node) [] Double Double    
p_water = CubeMap.map (\ _ -> Arith.one)  p_midVoltage 

p_gas ::  CubeMap.Cube Base ND.Dim2 (TopoIdx.Position Node) [] Double Double    
p_gas = CubeMap.map (\ _ -> Arith.one)  p_midVoltage 

demand :: Collection.Collection (TopoIdx.Position Node) (CubeMap.Cube Base ND.Dim2 (TopoIdx.Position Node) [] Double Double) 
demand = Collection.fromList (nc "Main.hs") [(TopoIdx.ppos LocalNetwork Gas ,p_gas),
                                             (TopoIdx.ppos Network Water ,p_water),
                                             (TopoIdx.ppos LocalRest LocalNetwork,p_lowVoltage), 
                                             (TopoIdx.ppos Rest Network,p_midVoltage)]
  
main :: IO()
main = do

  tabEta <- Table.read "eta.txt"
  
  let flow = ActOpt.solve topology etaAssignMap (etaMap tabEta)  demand 
      flow :: FlowTopo.Section Node (Result.Result (CubeMap.Data Base ND.Dim2 [] Double))
             
  let powers = ActOpt.getPowers grid flow         
  let _topo = FlowTopo.topology  flow  
  
--  print pl1   
  print powers
--  print $ map (\(_,cube) ->  CubeMap.lookupLin (nc "Main") cube (Grid.LinIdx 0)) $ Collection.toList powers


{-
  const Draw.xterm "simulationGraphsSequence"
    $ Draw.bgcolour DarkSeaGreen2
    $ Draw.title "Sequence Flow Graph from Simulation"
    $ Draw.flowSection Draw.optionsDefault flow


  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Hallo") PlotD3.plotInfo3lineTitles $ PlotD3.toPlotData (nc "plot") (Just "Test") p_lowVoltage
  
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Collection") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") (Just "Collection") demand

--  PlotD3.eachIO DefaultTerm.cons (PlotD3.labledFrame "Collection") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") (Just "Collection") demand
 
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Collection") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") (Just "Collection") powers
-}