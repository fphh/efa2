{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where
import qualified EFA.Action.Optimisation.Output as OP
import qualified EFA.Action.EenergyFlowAnalysis as EFA
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
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

import qualified EFA.Data.Plot.D2 as PlotD2
import qualified EFA.Data.Plot.D2.Curve as PlotCurve

import qualified EFA.Action.Optimisation.Cube.Sweep as CubeSweep
import qualified EFA.Action.Optimisation.Sweep as Sweep
--import qualified EFA.Action.Optimisation.Flow.Topology as 

--import qualified EFA.Action.Flow.Topology.Optimality as FlowTopoOpt
import qualified EFA.Action.Flow.Topology.Check as FlowTopoCheck
import qualified EFA.Action.Flow.Balance as Balance

--import qualified EFA.Action.Flow as ActFlow
import qualified EFA.Action.Flow.Optimality as FlowOpt


--import qualified EFA.Data.OD.Signal.Time as SignalTime
import qualified EFA.Data.OD.Signal.Flow as SignalFlow

--import qualified EFA.Data.OD.Signal.Record as SignalRecord
import EFA.Utility.Async (concurrentlyMany_)

import EFA.Utility(Caller,
                   --merror,(|>),
                   ModuleName(..),FunctionName, genCaller)
import qualified EFA.Report.Format as Format
import EFA.Application.Optimisation.Params (Name(Name))

import qualified EFA.Data.Collection as Collection
import qualified EFA.Data.Plot.Collection as PlotCollection
import qualified EFA.Data.Plot.D3 as PlotD3 -- TODO -- import Orphan instance
import qualified EFA.Value.Type as Type
import qualified EFA.Flow.Draw as Draw
--import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified EFA.Data.ND.Cube.Map as CubeMap 
import qualified EFA.Data.ND.Cube.Grid as CubeGrid 
--import qualified EFA.Data.Plot as DataPlot 
import qualified EFA.Data.Plot.D3.Cube as CubePlot 

import qualified  EFA.Action.Optimisation.Cube.Solve as CubeSolve
--import qualified EFA.IO.TableParserTypes as TPT
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Action.Flow.StateFlow.Optimality as StateFlowOpt

--import EFA.Utility.Async (concurrentlyMany_)
import qualified EFA.Flow.Topology.Quantity as TopoQty
import qualified EFA.Data.ND as ND
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph
import qualified EFA.Application.Utility as AppUt

--import Text.Printf (printf)
--import qualified EFA.Application.Optimisation.Params as Params
--import qualified EFA.Application.Type as Type
--import Text.Printf (--printf,
                --    PrintfArg)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
--import qualified Data.GraphViz.Types.Canonical as Canonical
import qualified EFA.Equation.Result as Result

--import qualified EFA.Report.FormatValue as FormatValue
--import qualified EFA.Equation.Arithmetic as Arith

--import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
--import qualified EFA.Action.EtaFunctions as EtaFunctions

import qualified EFA.IO.TableParser as Table

--import qualified Data.Text.Lazy as LazyText
--import qualified EFA.Signal.ConvertTable as CT

import qualified Data.Map as Map
import Data.GraphViz.Attributes.Colors.X11 (X11Color(DarkSeaGreen2))
                                                     -- Lavender))
--import Data.Maybe as Maybe




data Base

modul :: ModuleName
modul = ModuleName "Demo.Optimisation.Sweep"

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
  [(DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos LocalRest LocalNetwork,Type.P,[0.01, 0.11 .. 1.01]),  -- .. 2.01]), -- [-1.1,-0.6..(-0.1)]),
   (DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos Rest Network,Type.P,[0.01,0.11 .. 1.01])] -- .. 2.01])] -- [-1.1,-0.6..(-0.1)])]

searchVariation :: [(DemandAndControl.Var Node,Type.Dynamic,[Double])]
searchVariation = 
  [(DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos LocalNetwork Gas,Type.P,[0.01,0.21 .. 0.91]),
   (DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos Network Water,Type.P,[-0.91,-0.71 .. -0.01]++[0.01,0.21 .. 0.91])]  --  .. 0.91])]  


lifeCycleMap :: FlowOpt.LifeCycleMap Node (Interp.Val Double)
lifeCycleMap = FlowOpt.LifeCycleMap $ Map.fromList $ zip (map Idx.AbsoluteState [0,1,18,27,28,45,54,72]) $ repeat $ 
               Map.fromList [(Water,(FlowOpt.GenerationEfficiency (Interp.Inter 0.3), FlowOpt.UsageEfficiency (Interp.Inter 0.3)))] 


showFunctionAxis ::  Strict.Axis Base String [] Double
showFunctionAxis = Strict.Axis "Power" Type.P $ DV.fromList $ [-12,-11.9 .. -0.1] ++ [0,0.1..12]                      


caller = nc "Main"

-- OutPut Settings

sysCtrl = OP.SysDo {OP.topo = OP.DoP OP.Xterm OP.StdOut, OP.labTopo =  OP.DoP OP.Xterm OP.StdOut , OP.stateAnalysis = OP.Xterm}
testCtrl = OP.TestDo {OP.demandCycle = OP.Dflt}
sysDataCtrl = OP.SysDataDo {OP.rawCurves = OP.PoP OP.Dflt OP.StdOut, OP.etaFunctions = OP.Dflt}
optiSetCtrl = OP.OptiSetDo {OP.variation = OP.Dflt }
evalCtrl = OP.EvalDo { OP.plotEta = OP.Dflt, 
                       OP.plotEtaAt = OP.Dflt }

sweepCtrl = OP.SweepDo {OP.drawFlow = OP.Xterm,
                        OP.plotState = OP.Dflt,
                        OP.plotStatus = OP.Dflt, 
                        OP.plotFlowVariables = OP.Dflt} -- OP.DontPlot} -- OP.Dflt}
            
optCtrl = OP.OptiDo {OP.plotOptimality = OP.Dflt,
                     OP.plotOptEtaPerState = OP.Dflt, 
                     OP.plotEtaOptPerState= OP.Dflt, 
                     OP.plotOptIndexPerState= OP.Dflt, 
                     OP.plotOptimalSignalPerState =OP.Dflt }

opCtrl = OP.OpDo {OP.plotOptimalControlSignals = OP.Dflt, 
                  OP.plotOptimalStoragePowers = OP.Dflt}
         
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

etaLoopParams = 
  Loop.EtaLoopParams
  {Loop.accessMaxEtaIterations = Loop.MaxEtaIterations 1, 
   Loop.accLifeCycleMethod = StateFlowOpt.N_SFG_EQ_N_STATE,
   Loop.accGlobalLifeCycleMap = FlowOpt.GlobalLifeCycleMap $ 
     Map.fromList [(Water, (FlowOpt.GenerationEfficiency (Interp.Inter 0.5), FlowOpt.UsageEfficiency (Interp.Inter 0.5)))]                                              
}
  
balanceLoopParams =  
  Loop.BalanceLoopParams 
  {Loop.accessMaxIterationsPerStorage = Balance.MaxIterationsPerStorage 20, 
   Loop.accessMaxIterations = Balance.MaxIterations 20,
   Loop.accessThreshold = Balance.Threshold (Interp.Inter 0.01) , 
   Loop.accessInitialForcing = balanceForcingMap, -- Balance.ForcingMap $ Map.fromList [(Water, Balance.ChargeDrive (Interp.Inter 0.5))], 
   Loop.accessInitialStep = Balance.ForcingMap $ Map.fromList [(Water, Balance.ChargeDrive (Interp.Inter (0.001)))], 
   Loop.accessInitialSto = Water}
          

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
      
  let optiSet = Process.buildOptiSet demandVariation searchVariation demandCycle    
       :: Process.OptiSet Node Base ND.Dim2 ND.Dim2 [] [] [] Double
          
  let sweep = Process.makeSweep system systemData optiSet     
        :: Process.SweepResults Node Base ND.Dim2 ND.Dim2 [] [] Double
                        
{-  let evalSweep = Process.evaluateSweep caller lifeCycleMap sweep
        :: Process.SweepEvaluation Node Base ND.Dim2 ND.Dim2 [] [] Double
      
  let optPerState = Process.optimisationPerState testSet optiSet sweep evalSweep  storageList balanceForcingMap controlVars
--        :: Process.OptimisationPerState node inst demDim srchDim demVec srchVec sigVec a
     
  let optimalOperation = Process.optimalOperation optPerState
      
   
  let simEfa = Process.simulateAndAnalyse caller system efaParams systemData demandVars optimalOperation demandCycle    
          :: Process.SimulationAndAnalysis Node Base [] Double
             
  let (Process.OptimalOperation _ _ stoPowers balance)  = optimalOperation
--  let (Process.OptimalOperation _ _ stoPowers balance1)  = optimalOperation1
  let (Process.SimulationAndAnalysis _ (EFA.EnergyFlowAnalysis rec _ _)) = simEfa
-}      
  let loop = Process.loop (nc "Main") system systemData testSet optiSet efaParams sweep 
             lifeCycleMap storageList etaLoopParams balanceLoopParams  
             
--   loop caller system systemData testSet optiSet efaParams sweepResults initialLifeCycleMap storageList controlVars etaParams balParams          
--  caller system systemData testSet optiSet efaParams sweepResults initialLifeCycleMap storageList controlVars etaParams balParams
  
--  concurrentlyMany_ $ OP.system sysCtrl system
--  concurrentlyMany_ $ OP.test testCtrl testSet demandVars
--  concurrentlyMany_ $ OP.sysData sysDataCtrl systemData
--  concurrentlyMany_ $ OP.optiSet (nc "Main") optiSetCtrl optiSet
  let flowVars = [--TopoIdx.Power (TopoIdx.Position Water Network)] 
                  --TopoIdx.Power (TopoIdx.Position Gas LocalNetwork), 
                  TopoIdx.Power (TopoIdx.Position Coal Network)] 
                  --TopoIdx.Power (TopoIdx.Position Rest Network),
                  --TopoIdx.Power (TopoIdx.Position Network LocalNetwork)]
                  --TopoIdx.Power (TopoIdx.Position LocalRest LocalNetwork)]                      
      
  let flowVars2 = [--TopoIdx.Eta (TopoIdx.Position Coal Network),                
                  --TopoIdx.Eta (TopoIdx.Position Water Network)]
                  --TopoIdx.Eta (TopoIdx.Position Gas LocalNetwork)] 
                  TopoIdx.Eta (TopoIdx.Position Network LocalNetwork)]
                  --TopoIdx.Power (TopoIdx.Position LocalRest LocalNetwork)]                      
{-   
  concurrentlyMany_ $ 
    OP.sweep  (nc "Main") flowVars [Water] (Process.accessSearchGrid optiSet) sweepCtrl sweep  
    ++ OP.evalSweep (nc "Main") (Process.accessSearchGrid optiSet) evalCtrl evalSweep 
    ++ OP.optPerState  (nc "Main") (Process.accessSearchGrid optiSet) optCtrl optPerState  
    ++ OP.optimalOperation opCtrl optimalOperation
    ++ OP.simulation simCtrl simEfa
-}  
--  concurrentlyMany_ $ OP.simulation simCtrl (Process.accSimEfa $ Loop.getLastResult loop)

--  print stoPowers
--  print balance
--  print balance1
--  print $ Process.accessOptimalStoragePowers optimalOperation
  
--  print $ Process.accessOptimalStoragePowers optimalOperation1
  
--  print $ Process.accessSweepEndNodePowers sweep
--  print $ Process.accessSweepOptimality evalSweep
  print loop
  OP.loopsIO evalCtrl optCtrl opCtrl simCtrl optiSet loop
--  print $  Process.accessOptimalStateSignals optPerState
--  print $  Process.accessOptimalStateChoice optimalOperation
--  print $ Process.accessOptimalStateChoice $ Process.accOptOperation $ Loop.getLastResult loop 
--  print $ EFA.accessStateFlowGraph $ Process.accessAnalysis $ Process.accSimEfa $ Loop.getLastResult loop 
--  print $ demandCycle    
--  print $ Process.accessOptimalControlSignals $ Process.accOptOperation $ Loop.getLastResult loop 
--  print $ EFA.accessSeqFlowRecord $ Process.accessAnalysis $ Process.accSimEfa $ Loop.getLastResult loop
--  concurrentlyMany_ $ OP.simulation simCtrl (Process.accSimEfa $ Loop.getLastResult loop)

 {-
 Flow.HRecord Axis {getLabel = "Time", getType = T, getVec = [TimeStep {getMidTime = 0.0, getTimeStep = 1.0},TimeStep {getMidTime = 1.0, getTimeStep = 1.0},TimeStep {getMidTime = 2.0, getTimeStep = 1.0},TimeStep {getMidTime = 3.0, getTimeStep = 1.0},TimeStep {getMidTime = 4.0, getTimeStep = 1.0},TimeStep {getMidTime = 5.0, getTimeStep = 1.0},TimeStep {getMidTime = 6.0, getTimeStep = 1.0},TimeStep {getMidTime = 7.0, getTimeStep = 1.0},TimeStep {getMidTime = 8.0, getTimeStep = 1.0},TimeStep {getMidTime = 9.0, getTimeStep = 1.0},TimeStep {getMidTime = 10.0, getTimeStep = 1.0},TimeStep {getMidTime = 11.0, getTimeStep = 1.0},TimeStep {getMidTime = 12.0, getTimeStep = 1.0},TimeStep {getMidTime = 13.0, getTimeStep = 1.0},TimeStep {getMidTime = 14.0, getTimeStep = 1.0},TimeStep {getMidTime = 15.0, getTimeStep = 1.0},TimeStep {getMidTime = 16.0, getTimeStep = 1.0},TimeStep {getMidTime = 17.0, getTimeStep = 1.0},TimeStep {getMidTime = 18.0, getTimeStep = 1.0},TimeStep {getMidTime = 19.0, getTimeStep = 1.0},TimeStep {getMidTime = 20.0, getTimeStep = 1.0},TimeStep {getMidTime = 21.0, getTimeStep = 1.0},TimeStep {getMidTime = 22.0, getTimeStep = 1.0},TimeStep {getMidTime = 23.0, getTimeStep = 1.0}]} fromList [(Position Coal Network,Data {getVector = [Inter 2.0511095386685536,Inter 1.4226499217668174,Inter 1.329356201839421,Inter 0.7790545041221236,Inter 1.4226499217668174,Inter 1.4226499217668174,Inter 1.3193703371703114,Inter 1.4226499217668174,Inter 1.7530450479045454,Inter 9.932204745805729,Inter 1.4226499217668174,Inter 2.544675394622395,Inter 1.4226499217668174,Inter 10.933795621105487,Inter 0.34827868043772553,Inter 10.933795621105487,Inter 2.1404764046074702,Inter 11.456367676304202,Invalid ["\"\\\"coal\\\"\"@-5.665531335149864e-2"],Inter 4.613461991538875,Invalid ["\"\\\"coal\\\"\"@-8.466465053763438e-2"],Invalid ["\"\\\"coal\\\"\"@-1.7880990845449633e-2"],Inter 2.721967688909635,Invalid ["\"\\\"coal\\\"\"@-3.363162878787878e-2"]]}),(Position Gas LocalNetwork,Data {getVector = [Inter 1.220175173663546,Inter 1.1440426793123892,Inter 1.2244897959183674,Inter 1.1440426793123892,Inter 1.1440426793123892,Inter 1.1859838274932617,Inter 1.1987381703470033,Inter 1.1440426793123892,Inter 1.2244897959183676,Inter 1.2244897959183674,Inter 1.1440426793123892,Inter 1.2244897959183672,Inter 1.1440426793123892,Inter 0.17316017316017274,Inter 0.8580655082269723,Inter 0.17316017316017274,Inter 1.1440426793123895,Inter 0.11904761904761907,Inter 1.2244897959183676,Inter 1.190226352870634,Inter 1.2244897959183676,Inter 1.070701192900786,Inter 1.1440426793123892,Inter 1.1070327411540157,Inter 1.1440426793123892]}),(Position Water Network,Data {getVector = [Inter 0.1111111111111111,Inter (-1.4849999999999954e-2),Inter (-1.4849999999999954e-2),Inter 0.1111111111111111,Inter (-1.4849999999999954e-2),Inter (-1.4849999999999954e-2),Inter 0.1111111111111111,Inter (-1.4849999999999954e-2),Inter 0.8013395526850855,Inter (-0.7646265560165976),Inter (-1.4849999999999954e-2),Inter (-1.5272727272727223e-2),Inter (-1.4849999999999954e-2),Inter (-0.7646265560165976),Inter 0.8168394596292805,Inter (-0.7646265560165976),Inter 0.8085418075276088,Inter (-0.7646265560165976),Inter 0.1111111111111111,Inter (-1.4849999999999954e-2),Inter 0.4141414141414141,Inter 0.1111111111111111,Inter (-1.4849999999999954e-2),Inter 0.1111111111111111,Inter (-1.4849999999999954e-2)]}),(Position Network Coal,Data {getVector = [Inter 0.1063188134687333,Inter 7.046356715606289e-2,Inter 6.541095890410947e-2,Inter 3.6905770248808985e-2,Inter 7.046356715606289e-2,Inter 7.046356715606289e-2,Inter 6.487406716417911e-2,Inter 7.046356715606289e-2,Inter 8.890640096618352e-2,Inter 1.2363701923076926,Inter 7.046356715606289e-2,Inter 0.13690640096618342,Inter 7.046356715606289e-2,Inter 2.274212625282049,Inter 1.6031474820143887e-2,Inter 2.274212625282049,Inter 0.11169024545929251,Inter 1.9563527239150507,Inter (-5.665531335149864e-2),Inter 0.2951376146788988,Inter (-8.466465053763438e-2),Inter (-1.7880990845449633e-2),Inter 0.14846802016985122,Inter (-3.363162878787878e-2)]}),(Position Network Water,Data {getVector = [Inter 1.0e-2,Inter (-0.10999999999999976),Inter (-0.10999999999999976),Inter 1.0e-2,Inter (-0.10999999999999976),Inter (-0.10999999999999976),Inter 1.0e-2,Inter (-0.10999999999999976),Inter 0.5359999999999999,Inter (-0.9099999999999999),Inter (-0.10999999999999976),Inter (-0.11199999999999975),Inter (-0.10999999999999976),Inter (-0.9100000000000001),Inter 0.624,Inter (-0.9100000000000001),Inter 0.5740000000000005,Inter (-0.9099999999999999),Inter 1.0e-2,Inter (-0.10999999999999976),Inter 8.2e-2,Inter 1.0e-2,Inter (-0.10999999999999976),Inter 1.0e-2,Inter (-0.10999999999999976)]}),(Position Network LocalNetwork,Data {getVector = [Inter (-0.3836811865312667),Inter (-0.4395364328439369),Inter (-0.4645890410958903),Inter (-0.35309422975119104),Inter (-0.4395364328439369),Inter (-0.4395364328439369),Inter (-0.3751259328358209),Inter (-0.4395364328439369),Inter (-7.509359903381649e-2),Inter (-0.4736298076923075),Inter (-0.4395364328439369),Inter (-7.509359903381634e-2),Inter (-0.4395364328439369),Inter 0.964212625282049,Inter (-0.2599685251798562),Inter 0.964212625282049,Inter 0.18569024545929308,Inter 0.6463527239150508,Inter (-0.44665531335149866),Inter (-0.4148623853211009),Inter (-0.4026646505376344),Inter (-0.40788099084544965),Inter (-0.36153197983014856),Inter (-0.4236316287878788)]}),(Position Network Rest,Data {getVector = [Inter 0.5,Inter 0.4,Inter 0.42,Inter 0.4,Inter 0.4,Inter 0.4,Inter 0.45,Inter 0.4,Inter 0.7,Inter 0.8000000000000002,Inter 0.4,Inter 0.1,Inter 0.4,Inter 0.4,Inter 0.9000000000000001,Inter 0.4,Inter 0.5,Inter 0.4,Inter 0.4,Inter 0.6,Inter 0.4,Inter 0.4,Inter 0.4,Inter 0.4]}),(Position LocalNetwork Gas,Data {getVector = [Inter 0.808,Inter 0.772,Inter 0.8099999999999999,Inter 0.772,Inter 0.772,Inter 0.792,Inter 0.798,Inter 0.772,Inter 0.81,Inter 0.8099999999999999,Inter 0.772,Inter 0.8099999999999998,Inter 0.772,Inter 1.5999999999999945e-2,Inter 0.5580000000000002,Inter 1.5999999999999945e-2,Inter 0.7720000000000002,Inter 1.0e-2,Inter 0.81,Inter 0.794,Inter 0.81,Inter 0.7360000000000001,Inter 0.772,Inter 0.754,Inter 0.772]}),(Position LocalNetwork Network,Data {getVector = [Inter (-0.508),Inter (-0.5720000000000001),Inter (-0.5999999999999999),Inter (-0.47200000000000003),Inter (-0.5720000000000001),Inter (-0.5720000000000001),Inter (-0.49800000000000005),Inter (-0.5720000000000001),Inter (-0.1100000000000001),Inter (-0.6099999999999999),Inter (-0.5720000000000001),Inter (-0.10999999999999988),Inter (-0.5720000000000001),Inter 0.7840000000000003,Inter (-0.35800000000000015),Inter 0.7840000000000003,Inter 0.1279999999999999,Inter 0.49,Inter (-0.5800000000000001),Inter (-0.544),Inter (-0.53),Inter (-0.536),Inter (-0.48200000000000004),Inter (-0.554)]}),(Position LocalNetwork LocalRest,Data {getVector = [Inter 0.3,Inter 0.2,Inter 0.21000000000000002,Inter 0.3,Inter 0.2,Inter 0.22,Inter 0.3,Inter 0.2,Inter 0.7,Inter 0.2,Inter 0.2,Inter 0.7,Inter 0.2,Inter 0.8000000000000002,Inter 0.2,Inter 0.8000000000000002,Inter 0.9000000000000001,Inter 0.5,Inter 0.23,Inter 0.25,Inter 0.28,Inter 0.2,Inter 0.29,Inter 0.2]}),(Position Rest Network,Data {getVector = [Inter 0.5,Inter 0.4,Inter 0.42,Inter 0.4,Inter 0.4,Inter 0.4,Inter 0.45,Inter 0.4,Inter 0.7,Inter 0.8,Inter 0.4,Inter 0.1,Inter 0.4,Inter 0.4,Inter 0.9,Inter 0.4,Inter 0.5,Inter 0.4,Inter 0.4,Inter 0.6,Inter 0.4,Inter 0.4,Inter 0.4,Inter 0.4]}),(Position LocalRest LocalNetwork,Data {getVector = [Inter 0.3,Inter 0.2,Inter 0.21,Inter 0.3,Inter 0.2,Inter 0.22,Inter 0.3,Inter 0.2,Inter 0.7,Inter 0.2,Inter 0.2,Inter 0.7,Inter 0.2,Inter 0.8,Inter 0.2,Inter 0.8,Inter 0.9,Inter 0.5,Inter 0.23,Inter 0.25,Inter 0.28,Inter 0.2,Inter 0.29,Inter 0.2]})]


-}
