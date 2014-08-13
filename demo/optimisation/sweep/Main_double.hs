{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import qualified EFA.Action.Optimisation.Output as OP
import qualified EFA.Action.EnergyFlowAnalysis as EFA
import qualified EFA.Action.Optimisation.Process as Process
import qualified EFA.Action.Optimisation.Loop as Loop

import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Action.DemandAndControl as DemandAndControl
--import qualified EFA.Value.State as ValueState
import qualified EFA.Data.Interpolation as Interp
import qualified EFA.Data.OD.Curve as Curve
import qualified EFA.Action.EtaFunctions as EtaFunctions
--import qualified EFA.Data.OrdData as OrdData
import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.Vector as DV
--import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

--import qualified EFA.Data.Plot.D2 as PlotD2
--import qualified EFA.Data.Plot.D2.Curve as PlotCurve

--import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep
--import qualified EFA.Action.Optimisation.Sweep as Sweep
--import qualified EFA.Action.Optimisation.Flow.Topology as 
import qualified EFA.Action.Flow.Topology.LifeCycle as TopoLifeCycle
import qualified EFA.Action.Flow.Topology.ScaleMap as TopoScaleMap
--import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
--import qualified EFA.Action.Flow.Topology.Check as FlowTopoCheck
import qualified EFA.Action.Flow.Balance as Balance

--import qualified EFA.Action.Flow as ActFlow
import qualified EFA.Action.Flow.Optimality as FlowOpt


--import qualified EFA.Data.OD.Signal.Time as SignalTime
import qualified EFA.Data.OD.Signal.Flow as SignalFlow

--import qualified EFA.Data.OD.Signal.Record as SignalRecord
--import EFA.Utility.Async (concurrentlyMany_)

import EFA.Utility(Caller,
                   --merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
import qualified EFA.Report.Format as Format
import EFA.Application.Optimisation.Params (Name(Name))

--import qualified EFA.Data.Collection as Collection
--import qualified EFA.Data.Plot.Collection as PlotCollection
--import qualified EFA.Data.Plot.D3 as PlotD3 -- TODO -- import Orphan instance
import qualified EFA.Value.Type as Type
--import qualified EFA.Flow.Draw as Draw
--import qualified Graphics.Gnuplot.LineSpecification as LineSpec

--import qualified EFA.Data.ND.Cube.Map as CubeMap 
--import qualified EFA.Data.ND.Cube.Grid as CubeGrid 
--import qualified EFA.Data.Plot as DataPlot 
--import qualified EFA.Data.Plot.D3.Cube as CubePlot 

--import qualified  EFA.Action.Optimisation.Cube.Solve as CubeSolve
--import qualified EFA.IO.TableParserTypes as TPT
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Action.Flow.StateFlow.Optimality as StateFlowOpt

--import EFA.Utility.Async (concurrentlyMany_)
--import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Data.ND as ND
--import qualified EFA.Graph.Topology as Topo
--import qualified EFA.Graph as Graph
import qualified EFA.Application.Utility as AppUt

--import Text.Printf (printf)
--import qualified EFA.Application.Optimisation.Params as Params
--import qualified EFA.Application.Type as Type
--import Text.Printf (--printf,
                --    PrintfArg)

--import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
--import qualified Data.GraphViz.Types.Canonical as Canonical
--import qualified EFA.Equation.Result as Result

--import qualified EFA.Report.FormatValue as FormatValue
--import qualified EFA.Equation.Arithmetic as Arith

--import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
--import qualified EFA.Action.EtaFunctions as EtaFunctions

import qualified EFA.IO.TableParser as Table

--import qualified Data.Text.Lazy as LazyText
--import qualified EFA.Signal.ConvertTable as CT

import qualified Data.Map as Map
--import Data.GraphViz.Attributes.Colors.X11 (X11Color(DarkSeaGreen2))
                                                     -- Lavender))
--import Data.Maybe as Maybe




data Base

modul :: ModuleName
modul = ModuleName "Demo.Optimisation.Sweep"

nc :: FunctionName -> Caller
nc = genCaller modul

caller :: Caller
caller = nc "main"

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
         Coal -> Node.Source
         Gas -> Node.Source
         Water -> Node.storage
         Network -> Node.Crossing
         Rest -> Node.AlwaysSink
         LocalNetwork -> Node.Crossing
         LocalRest -> Node.AlwaysSink
         
edgeList :: AppUt.LabeledEdgeList Node
edgeList = [(Coal, Network, "Coal\\lPlant", "Coal","ElCoal"),
               (Water, Network, "Water\\lPlant","Water","ElWater"),

               (Network, Rest,"100%","toResidualHV","toResidualHV"),

               (Network, LocalNetwork, "Trans-\\lformer", "HighVoltage", "LowVoltage"),
               (Gas, LocalNetwork,"Gas\\lPlant","Gas","ElGas"),
               (LocalNetwork, LocalRest, "100%", "toResidualLV", "toResidualLV")]


etaAssignMap :: EtaFunctions.EtaAssignMap Node Double
etaAssignMap = EtaFunctions.EtaAssignMap $ Map.fromList $
   (TopoIdx.Position Network Water,
    (EtaFunctions.DownStream, EtaFunctions.Duplicate ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 1 0.9], "storage")))) : 
   (TopoIdx.Position Network Coal ,
    (EtaFunctions.DownStream, EtaFunctions.Single ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 7 0.45], "coal")))) : 
   (TopoIdx.Position LocalNetwork Gas,
    (EtaFunctions.DownStream, EtaFunctions.Single ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 1 0.7], "gas")))) : 
   (TopoIdx.Position LocalNetwork Network,
    (EtaFunctions.DownStream,EtaFunctions.Duplicate ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 3 0.95], "transformer")))) : 
   (TopoIdx.Position LocalRest LocalNetwork,
    (EtaFunctions.DownStream,EtaFunctions.Single ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 10 1], "local")))) : 
   (TopoIdx.Position Rest Network,
    (EtaFunctions.DownStream,EtaFunctions.Single ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 10 1], "rest")))) : 
   []

efaParams :: EFA.EFAParams Node (Interp.Val Double)
efaParams = EFA.EFAParams (EFA.InitStorageState $ Map.fromList [(Water,Interp.Inter 1.0)])
                             (EFA.InitStorageSeq $ Map.fromList [(Water,Interp.Inter 1.0)])

controlVars :: [DemandAndControl.ControlVar Node]
controlVars = [DemandAndControl.ControlPower $ TopoIdx.Power (TopoIdx.Position LocalNetwork Gas), 
               DemandAndControl.ControlPower $ TopoIdx.Power (TopoIdx.Position Network Water)]

demandVars :: [DemandAndControl.DemandVar Node]
demandVars = [DemandAndControl.DemandPower $ TopoIdx.Power (TopoIdx.Position LocalRest LocalNetwork), 
               DemandAndControl.DemandPower $ TopoIdx.Power (TopoIdx.Position Rest Network)]

storageList :: [Node]              
storageList = [Water]              

demandCycle :: OptSignal.DemandCycle Node Base ND.Dim2 [] Double Double
demandCycle = SignalFlow.fromList (nc "Main") "Time" Type.T $ map f [(0,ND.fromList (nc "Main") [0.3,0.5]),
                                                             (1,ND.fromList (nc "Main") [0.2,0.4]),
                                                             (2,ND.fromList (nc "Main") [0.21,0.42]),
                                                             (3,ND.fromList (nc "Main") [0.3,0.4]),
                                                             (4,ND.fromList (nc "Main") [0.2,0.4]),
                                                             (5,ND.fromList (nc "Main") [0.22,0.4]),
                                                             (6,ND.fromList (nc "Main") [0.3,0.45]),
                                                             (7,ND.fromList (nc "Main") [0.2,0.4]),
                                                             (8,ND.fromList (nc "Main") [0.7,0.7]),
                                                             (9,ND.fromList (nc "Main") [0.2,0.8]),
                                                             (10,ND.fromList (nc "Main") [0.2,0.4]),
                                                             (11,ND.fromList (nc "Main") [0.7,0.1]),
                                                             (12,ND.fromList (nc "Main") [0.2,0.4]),
                                                             (13,ND.fromList (nc "Main") [0.8,0.4]),
                                                             (14,ND.fromList (nc "Main") [0.2,0.9]),
                                                             (15,ND.fromList (nc "Main") [0.8,0.4]),
                                                             (16,ND.fromList (nc "Main") [0.9,0.5]),
                                                             (17,ND.fromList (nc "Main") [0.5,0.4]),
                                                             (18,ND.fromList (nc "Main") [0.23,0.4]),
                                                             (19,ND.fromList (nc "Main") [0.25,0.6]),
                                                             (20,ND.fromList (nc "Main") [0.28,0.4]),
                                                             (21,ND.fromList (nc "Main") [0.2,0.4]),
                                                             (22,ND.fromList (nc "Main") [0.29,0.4]),
                                                             (23,ND.fromList (nc "Main") [0.2,0.4])]
   where f (x,xs) = (SignalFlow.TimeStep x 1.0,xs) 

demandVariation :: [(DemandAndControl.Var Node,Type.Dynamic,[Double])]
demandVariation  = 
  [(DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos LocalRest LocalNetwork,Type.P,[0.01, 0.11 .. 1.01]),  
   -- .. 2.01]), -- [-1.1,-0.6..(-0.1)]),
   (DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos Rest Network,Type.P,[0.01,0.11 .. 1.01])] 
  -- .. 2.01])] -- [-1.1,-0.6..(-0.1)])]

searchVariation :: [(DemandAndControl.Var Node,Type.Dynamic,[Double])]
searchVariation = 
  [(DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos LocalNetwork Gas,Type.P,[0.01,0.21 .. 0.91]),
   (DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos Network Water,Type.P,[-0.91,-0.71 .. -0.01]++[0.01,0.21 .. 0.91])]  
  --  .. 0.91])]  

states :: [Idx.AbsoluteState]
states = map Idx.AbsoluteState [0,18,54,72]

initialLifeCycleMap :: FlowOpt.LifeCycleMap Node (Interp.Val Double)
initialLifeCycleMap = FlowOpt.LifeCycleMap $ Map.fromList $ zip (map Idx.AbsoluteState [0,1,18,27,28,45,54,72]) 
               $ repeat $  -- [0,1,18,27,28,45,54,72]
               Map.fromList [(Water,(FlowOpt.GenerationEfficiency (Interp.Inter 0.3), FlowOpt.UsageEfficiency (Interp.Inter 0.3)))] 


showFunctionAxis ::  Strict.Axis Base String [] Double
showFunctionAxis = Strict.Axis "Power" Type.P $ DV.fromList $ [-12,-11.9 .. -0.1] ++ [0,0.1..12]                      


-- OutPut Settings
sysCtrl :: OP.SysCtrl
sysCtrl = OP.SysDo {OP.topo = OP.DoP OP.Xterm OP.StdOut, OP.labTopo =  OP.DoP OP.Xterm OP.StdOut , OP.stateAnalysis = OP.Xterm}

testCtrl :: OP.TestCtrl
testCtrl = OP.TestDo {OP.demandCycle = OP.Dflt}

sysDataCtrl :: OP.SysDataCtrl
sysDataCtrl = OP.SysDataDo {OP.rawCurves = OP.PoP OP.Dflt OP.StdOut, OP.etaFunctions = OP.Dflt}

optiSetCtrl :: OP.OptiSetCtrl
optiSetCtrl = OP.OptiSetDo {OP.variation = OP.Dflt }

evalCtrl :: OP.EvalCtrl
evalCtrl = OP.EvalDont
  {-OP.EvalDo { OP.plotEta = OP.Dflt, 
                       OP.plotEtaAt = OP.Dflt }-}

sweepCtrl :: OP.SweepCtrl
sweepCtrl = OP.SweepDont 
          {-  OP.SweepDo {OP.drawFlow = OP.Xterm,
                         OP.plotState = OP.Dflt,
                         OP.plotStatus = OP.Dflt, 
                         OP.plotFlowVariables = OP.Dflt} -- OP.DontPlot} -- OP.Dflt}-}
optCtrl :: OP.OptiCtrl            
optCtrl = OP.OptiDont 
          {- OP.OptiDo
                    {OP.plotOptimality = OP.Dflt,
                     OP.plotOptEtaPerState = OP.Dflt, 
                     OP.plotEtaOptPerState= OP.Dflt, 
                     OP.plotOptIndexPerState= OP.Dflt, 
                     OP.plotOptimalSignalPerState =OP.Dflt } -}

opCtrl :: OP.OpCtrl
opCtrl = OP.OpDont {-
         OP.OpDo {OP.plotOptimalControlSignals = OP.Dflt, 
                  OP.plotOptimalStoragePowers = OP.Dflt}-}

simCtrl :: OP.SimCtrl         
simCtrl = OP.SimDo {OP.drawSimulationFlowGraph = OP.Xterm,
                    OP.plotSimulationPowers = OP.Dflt,
                    OP.drawSequenceFlowGraph = OP.Xterm, 
                    OP.drawStateFlowGraph = OP.Xterm} -- ,OP.Xterm}

balanceForcingMap :: Balance.Forcing Node (Interp.Val Double)
balanceForcingMap = Balance.ForcingMap $ Map.fromList [(Water, Balance.ChargeDrive (Interp.Inter (-1.0)))]

balanceForcingMap1 :: Balance.Forcing Node (Interp.Val Double)
balanceForcingMap1 = Balance.ForcingMap $ Map.fromList [(Water, Balance.ChargeDrive (Interp.Inter 0.1))]

initialBalanceMap :: Balance.Balance Node Double
initialBalanceMap = Balance.Balance $ Map.fromList [(Water, 0.5)]

initialBalanceMap' :: Balance.Balance Node (Interp.Val Double)
initialBalanceMap' = Balance.Balance $ Map.fromList [(Water, Interp.Inter (0.5))]

etaLoopParams ::
 Loop.EtaLoopParams Node (Interp.Val Double) (FlowOpt.LifeCycleMap Node (Interp.Val Double))
etaLoopParams = 
  Loop.EtaLoopParams
  {Loop.accessMaxEtaIterations = Loop.MaxEtaIterations 4, 
   Loop.accInitialEvalParam = initialLifeCycleMap,
   Loop.accGlobalLifeCycleMap = FlowOpt.GlobalLifeCycleMap $ 
     Map.fromList [(Water, (FlowOpt.GenerationEfficiency (Interp.Inter 0.5), FlowOpt.UsageEfficiency (Interp.Inter 0.5)))]                                              
}

balanceLoopParams :: Loop.BalanceLoopParams Node (Interp.Val Double)  
balanceLoopParams =  
  Loop.BalanceLoopParams 
  {Loop.accessMaxIterationsPerStorage = Balance.MaxIterationsPerStorage 20, 
   Loop.accessMaxIterations = Balance.MaxIterations 20,
   Loop.accessThreshold = Balance.Threshold (Interp.Inter 0.01) , 
   Loop.accessInitialForcing = balanceForcingMap, 
   Loop.accessInitialStep = Balance.ForcingMap $ Map.fromList [(Water, Balance.ChargeDrive (Interp.Inter (0.001)))], 
   Loop.accessInitialSto = Water}
          

bestPair :: Balance.BestForcingPair (Interp.Val Double)
bestPair = 
  Balance.rememberBestBalanceForcing (nc "Main") (Balance.BestForcingPair (Nothing, Nothing)) (balanceForcingMap, initialBalanceMap') Water

main :: IO()
main = do
  
  print bestPair
  
  let system = Process.buildSystem edgeList
  
  let testSet = Process.buildTestSet demandCycle initialBalanceMap 
      
  tabEta <- Table.read "eta.txt"
  let rawEtaCurves = Curve.curvesfromParseTableMap (nc "etaCurves") tabEta 
                     :: Curve.Map String Base String  [] Double Double
                        
                        
  let systemData = Process.buildSystemData rawEtaCurves etaAssignMap showFunctionAxis             
      
  let optiSet = Process.buildOptiSet caller demandVariation searchVariation demandCycle states   
       :: Process.OptiSet Node Base ND.Dim2 ND.Dim2 [] [] [] Double
          
  let sweep = Process.makeSweep caller system systemData optiSet     
        :: Process.SweepResults Node Base ND.Dim2 ND.Dim2 [] [] Double
                        
  let evalFunction = TopoLifeCycle.calcEtaLossSys
--  let evalFunction2 = TopoScaleMap.calcEtaLossSys    
                                                             
  let evalMethod=StateFlowOpt.N_SFG_EQ_N_STATE
      
  let updateEvalParam = StateFlowOpt.updateOneStorageLifeCycleEfficiencies 
                           caller (Process.accessTopology system) evalMethod  (Loop.accGlobalLifeCycleMap etaLoopParams) . 
                           EFA.accessStateFlowGraph . Process.accessAnalysis . Process.accSimEfa
  
  let loop = Process.loop (nc "Main") system systemData testSet optiSet efaParams sweep 
             storageList etaLoopParams balanceLoopParams evalFunction updateEvalParam
             

  print loop
  OP.loopsIO evalCtrl optCtrl opCtrl simCtrl system optiSet loop
 
  
  
  
  