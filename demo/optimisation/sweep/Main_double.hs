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

--import EFA.Utility.Async (concurrentlyMany_)
--import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Data.ND as ND
import qualified EFA.Graph.Topology as Topo
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
         Coal -> Node.AlwaysSource
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
    EtaFunctions.Duplicate ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 1 0.9], "storage"))) : 
   (TopoIdx.Position Network Coal ,
    EtaFunctions.Duplicate ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 7 0.45], "coal"))) : 
   (TopoIdx.Position LocalNetwork Gas,
    EtaFunctions.Duplicate ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 1 0.7], "gas"))) : 
   (TopoIdx.Position LocalNetwork Network,
    EtaFunctions.Duplicate ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 3 0.95], "transformer"))) : 
   (TopoIdx.Position LocalRest LocalNetwork,
    EtaFunctions.Duplicate ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 1 1], "local"))) : 
   (TopoIdx.Position Rest Network,
    EtaFunctions.Duplicate ((Interp.Linear,Interp.ExtrapNone),([Curve.Scale 1 1], "rest"))) : 
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
demandCycle = OptSignal.DemandCycle $ SignalFlow.fromList (nc "Main") "Time" Type.T [(0,ND.fromList (nc "Main") [0.3,0.5]),
                                                                                     (1,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (2,ND.fromList (nc "Main") [0.21,0.42]),
                                                                                     (3,ND.fromList (nc "Main") [2,0.4]),
                                                                                     (4,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (5,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (6,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (7,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (8,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (9,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (10,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (11,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (12,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (13,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (14,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (15,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (16,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (17,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (18,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (19,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (20,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (21,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (22,ND.fromList (nc "Main") [0.2,0.4]),
                                                                                     (23,ND.fromList (nc "Main") [0.2,0.4])]


{-local = [0.1,0.6 .. 6.2]
rest = [0.1,0.3 .. 2.1]

water = [0.1, 0.2 .. 0.8]
gas =   [0.1, 0.2 .. 0.8]
-}

demandVariation :: [(DemandAndControl.Var Node,Type.Dynamic,[Double])]
demandVariation  = 
  [(DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos LocalRest LocalNetwork,Type.P,[0.1,0.6..6.2]), -- [-1.1,-0.6..(-0.1)]),
   (DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos Rest Network,Type.P,[0.1,0.2..2.1])] -- [-1.1,-0.6..(-0.1)])]

searchVariation :: [(DemandAndControl.Var Node,Type.Dynamic,[Double])]
searchVariation = 
  [(DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos LocalNetwork Gas,Type.P,[0.1,0.2..0.8]),
   (DemandAndControl.Power $ TopoIdx.Power $ TopoIdx.ppos Network Water,Type.P,[0.1,0.2..0.8])]  


lifeCycleMap :: FlowOpt.LifeCycleMap Node (Interp.Val Double)
lifeCycleMap = FlowOpt.LifeCycleMap $ Map.fromList $ zip (map Idx.AbsoluteState [335,616,598]) $ replicate 3 $
               Map.fromList [(Water,(FlowOpt.GenerationEfficiency $ Interp.Inter 1.0, FlowOpt.UsageEfficiency $ Interp.Inter 1.0))] 


showFunctionAxis ::  Strict.Axis Base String [] Double
showFunctionAxis = Strict.Axis "Power" Type.P $ DV.fromList [-100 .. 100]                      


caller = nc "Main"

-- OutPut Settings

sysCtrl = OP.SysDo {OP.topo = OP.DoP OP.Xterm OP.StdOut, OP.labTopo =  OP.DoP OP.Xterm OP.StdOut , OP.stateAnalysis = OP.Xterm}
testCtrl = OP.TestDo {OP.demandCycle = OP.Dflt}
sysDataCtrl = OP.SysDataDo {OP.rawCurves = OP.PoP OP.Dflt OP.StdOut, OP.etaFunctions = OP.Dflt}
optiSetCtrl = OP.OptiSetDo {OP.variation = OP.Dflt }
sweepCtrl = OP.SweepDo {OP.drawFlow = OP.Xterm}
optCtrl = OP.OptiDo {OP.plotOptEtaPerState = OP.Dflt, 
                     OP.plotEtaOptPerState= OP.Dflt, 
                     OP.plotOptIndexPerState= OP.Dflt}

opCtrl = OP.OpDo {OP.plotOptimalControlSignals = OP.Dflt, 
                  OP.plotOptimalStoragePowers = OP.Dflt}
simCtrl = OP.SimDo {OP.drawSimulationFlowGraph = OP.Xterm,
                    OP.plotSimulationPowers = OP.Dflt,
                    OP.drawSequenceFlowGraph = OP.Xterm, 
                    OP.drawStateFlowGraph = OP.Xterm}

balanceForcingMap :: Balance.Forcing Node (Interp.Val Double)
balanceForcingMap = Balance.ForcingMap $ Map.fromList [(Water, Balance.ChargeDrive (Interp.Inter 0.5))]

initialBalanceMap :: Balance.Balance Node Double
initialBalanceMap = Balance.Balance $ Map.fromList [(Water, 0.5)]

initialBalanceMap' :: Balance.Balance Node (Interp.Val Double)
initialBalanceMap' = Balance.Balance $ Map.fromList [(Water, Interp.Inter (0.5))]

etaLoopParams = 
  Loop.EtaLoopParams
  {Loop.accessMaxEtaIterations = Loop.MaxEtaIterations 5}
  
balanceLoopParams =  
  Loop.BalanceLoopParams 
  {Loop.accessMaxIterationsPerStorage = Balance.MaxIterationsPerStorage 10 , 
   Loop.accessMaxIterations = Balance.MaxIterations 100,
   Loop.accessThreshold = Balance.Threshold (Interp.Inter 0.1) , 
   Loop.accessInitialForcing = Balance.ForcingMap $ Map.fromList [(Water, Balance.ChargeDrive (Interp.Inter 0))], 
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
                        
  let evalSweep = Process.evaluateSweep caller lifeCycleMap sweep
      
  let optPerState = Process.optimisationPerState testSet optiSet sweep evalSweep  storageList balanceForcingMap controlVars
--        :: Process.OptimisationPerState node inst demDim srchDim demVec srchVec sigVec a
     
  let optimalOperation = Process.optimalOperation optPerState
      
  let simEfa = Process.simulateAndAnalyse caller system efaParams systemData demandVars optimalOperation demandCycle    
          :: Process.SimulationAndAnalysis Node Base [] Double
             
  let (Process.OptimalOperation _ _ stoPowers balance)  = optimalOperation
  let (Process.SimulationAndAnalysis _ (EFA.EnergyFlowAnalysis rec _ _)) = simEfa
      
  let loop = Process.loop (nc "Main") testSet optiSet sweep evalSweep storageList controlVars etaLoopParams balanceLoopParams  
  
{-  
  concurrentlyMany_ $ OP.system sysCtrl system
  concurrentlyMany_ $ OP.test testCtrl testSet demandVars
  concurrentlyMany_ $ OP.sysData sysDataCtrl systemData
  concurrentlyMany_ $ OP.optiSet (nc "Main") optiSetCtrl optiSet
  concurrentlyMany_ $ OP.sweep (CubeGrid.LinIdx 0) sweepCtrl sweep
  concurrentlyMany_ $ OP.optPerState  (nc "Main") optCtrl optPerState
  concurrentlyMany_ $ OP.optimalOperation opCtrl optimalOperation
  concurrentlyMany_ $ OP.simulation simCtrl simEfa
-}  
--  print stoPowers
  print balance
  print loop
--  print rec
  
{-  let Just flow_00 = CubeMap.lookupMaybe (ND.Data $ map Strict.Idx [0,0]) sweepCube
  let powers = CubeMap.map (\ flow -> CubeSolve.getPowers searchGrid flow) sweepCube
      
  -- TODO :: Same as Control Signals ?     
  let powerResult = CubeSweep.getDemandSweepPowers (condenseResult) sweepCube :: Collection.Collection (TopoIdx.Position Node) (Result.Result (CubeMap.Cube (Sweep.Demand Base) ND.Dim2 (TopoIdx.Position Node) [] Double (Interp.Val Double)))    
      
      condenseResult (Result.Determined (CubeMap.Data x)) = Result.Determined (DV.maximum x)
      condenseResult Result.Undetermined = Result.Undetermined

      powerResult2 = Collection.getDetermined powerResult :: Collection.Collection (TopoIdx.Position Node) (CubeMap.Cube (Sweep.Demand Base) ND.Dim2 (TopoIdx.Position Node) [] Double (Interp.Val Double))
  
  let p_CoalDemand = CubeMap.map (\collection -> flip CubeMap.lookupLinUnsafe (Grid.LinIdx 0) $
                                  Collection.lookup (nc "main") (TopoIdx.ppos Coal Network) collection) powers
  let status = CubeSweep.getFlowStatus (nc "main") sweepCube
  let endNodeValues = CubeSweep.getEndNodeFlows sweepCube 
  let optimalityMeasure =  CubeSweep.calculateOptimalityMeasure (nc "main") lifeCycleMap endNodeValues status
    
  let objectiveFunctionValues = CubeSweep.objectiveFunctionValues (nc "main") balanceForcingMap endNodeValues optimalityMeasure
      
  let optimisationResultPerState = CubeSweep.findMaximumEtaPerState  (nc "main") objectiveFunctionValues
  
--  let etaSys = FlowTopoOpt.getEtaValues (nc "main") flow_00 
  let absState = FlowTopoCheck.getFlowStatus (nc "Main") flow_00
      
  let supportSignal = OptSignal.getSupportPoints (nc "Main") demandGrid demandCycle 

  let supportSignalLinIdx = SignalFlow.map (Grid.getSupportingPointLinearIndices (nc "main")  demandGrid) supportSignal
  let supportSignalObjFuncValues =  SignalFlow.map (CubeMap.lookupSupportingPoints (nc "main") objectiveFunctionValues) supportSignal
--  let supportPointOpt = CubeSweep.getOptimalSuportPoints supportSignalObjFuncValues
  let optimalStateSignals = OptSignal.optimalStateSignals (nc "main") optimisationResultPerState supportSignal demandCycle
  let optimalStateSignal = OptSignal.findOptimalStatesUsingMaxEta (nc "main") OptSignal.StateForcingOff optimalStateSignals
      
  let optimalFlowCube = CubeSweep.unresultOptimalFlowPerStateCube (nc "main") $ 
                        CubeSweep.getOptimalFlowPerStateCube (nc "main") optimisationResultPerState sweepCube
      
  let optimalControlSignalsPerState = OptSignal.interpolateControlSignalsPerState (nc "main") Interp.Linear 
                                       optimalFlowCube supportSignal demandCycle controlVars 
                                       
  let optimalStoragePowersPerState = OptSignal.interpolateStoragePowersPerState (nc "main") Interp.Linear 
                              optimalFlowCube supportSignal demandCycle storageList                                   
                              
  let optimalControlSignals = OptSignal.generateOptimalControl optimalStateSignal optimalControlSignalsPerState                       
  let optimalStorageSignals = OptSignal.generateOptimalStorageSignals optimalStateSignal optimalStoragePowersPerState
      
  let balance = OptSignal.getBalance optimalStorageSignals 
        
--  print given
--  print sweepCube    
--  print absState    
--  print lifeCycleMap
--  print etaValues
  print optimisationResultPerState
  print "--supportSignal--"
  print supportSignal
  print "--supportSignalLinIdx--"
  print supportSignalLinIdx
  print "--supportSignalObjFuncValues--"
  print supportSignalObjFuncValues
  print "" 
  print "--optimalStateSignals--"
  print optimalStateSignals
  
  print "--optimalStateSignal--"
  print optimalStateSignal
  
  print "optimalControlSignalsPerState"
  print optimalControlSignalsPerState
  
  print "storagePowersPerState"
  print optimalStoragePowersPerState
  
  print "optimalControlSignals"
  print optimalControlSignals
  
  print "optimalStorageSignals"
  print optimalStorageSignals
  
  print "Balance"
  print balance
  
  
--  print supportPointOpt
  
  const Draw.xterm "simulationGraphsSequence"
    $ Draw.bgcolour DarkSeaGreen2
    $ Draw.title "Sequence Flow Graph from Simulation"
    $ Draw.flowSection Draw.optionsDefault flow_00
    
   
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "P_Coal") PlotD3.plotInfo3lineTitles $ PlotD3.toPlotData (nc "plot") 
    (Just "Test") p_CoalDemand
    
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Result") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") 
    (Just "Power") powerResult2

--  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Result") PlotD3.plotInfo3lineTitles $ PlotD3.toPlotData (nc "plot") 
--    (Just "EtaSys") etaResult
{-
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Hallo") PlotD3.plotInfo3lineTitles $ PlotD3.toPlotData (nc "plot") (Just "Test") p_lowVoltage
  
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Collection") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") (Just "Collection") demand

--  PlotD3.eachIO DefaultTerm.cons (PlotD3.labledFrame "Collection") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") (Just "Collection") demand
 
  PlotD3.allInOneIO DefaultTerm.cons (PlotD3.labledFrame "Collection") PlotD3.plotInfo3lineTitles $ PlotCollection.toD3PlotData (nc "plot") (Just "Collection") powers
-}

-}