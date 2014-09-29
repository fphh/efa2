module Main where

import qualified EFA.Equation.Arithmetic as Arith 
import qualified Graphics.Gnuplot.Advanced as Plot
import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Flow.SequenceState.Index as Idx
import EFA.Action.Flow.Optimality hiding (modul,nc)
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.Balance as Balance

import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Data.OD.Signal.Flow as SignalFlow
import qualified EFA.Value.Type as Type
import qualified EFA.Data.Interpolation as Interp
import Control.Functor.HT (void)
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified EFA.Action.Optimisation.Signal.Plot as OptSignalPlot
import qualified EFA.Action.Optimisation.Signal.Access as OptSignalAccess
import qualified EFA.Data.Plot.D2 as PlotD2
import qualified EFA.Data.Plot.D2.FlowSignal as PlotFlowSignal

import qualified EFA.Value.State as ValueState

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)

modul :: ModuleName
modul = ModuleName "Demo.Optimisation.StateForcing"

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
            
optimalStateSignals :: OptSignal.OptimalityPerStateSignal Node Base [] Double (Interp.Val Double)
optimalStateSignals = SignalFlow.Signal 
  Strict.Axis {Strict.getLabel = "Time", Strict.getType = Type.T, Strict.getVec = 
                  [SignalFlow.TimeStep {SignalFlow.getMidTime = 0.5, SignalFlow.getTimeStep = 1.0},
                   SignalFlow.TimeStep {SignalFlow.getMidTime = 1.5, SignalFlow.getTimeStep = 1.0}, 
                   SignalFlow.TimeStep {SignalFlow.getMidTime = 2.5, SignalFlow.getTimeStep = 1.0},
                   SignalFlow.TimeStep {SignalFlow.getMidTime = 3.5, SignalFlow.getTimeStep = 1.0}
                   ]} 
  SignalFlow.Data {SignalFlow.getVector = [
  ValueState.Map (Map.fromList [(Just (Idx.AbsoluteState {Idx.unAbsoluteState = 0}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 9}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 7}}) 
                  (TotalBalanceForce (Interp.Inter (0)))),
                 (Just (Idx.AbsoluteState {Idx.unAbsoluteState = 300}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 0.1}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 2.0}}) 
                  (TotalBalanceForce (Interp.Inter (0)))),
                  (Just (Idx.AbsoluteState {Idx.unAbsoluteState = 500}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 0.1}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 2.0}}) 
                  (TotalBalanceForce (Interp.Inter (0))))]),
  ValueState.Map (Map.fromList [(Just (Idx.AbsoluteState {Idx.unAbsoluteState = 0}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 3.2}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 5.0}}) 
                  (TotalBalanceForce (Interp.Inter (0)))),
                 (Just (Idx.AbsoluteState {Idx.unAbsoluteState = 300}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 0.9}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 2.0}}) 
                  (TotalBalanceForce (Interp.Inter (0)))),                             
                  (Just (Idx.AbsoluteState {Idx.unAbsoluteState = 500}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 0.9}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 2.0}}) 
                  (TotalBalanceForce (Interp.Inter (0))))]),                             
  ValueState.Map (Map.fromList [(Just (Idx.AbsoluteState {Idx.unAbsoluteState = 0}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 2.9}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 5.0}}) 
                  (TotalBalanceForce (Interp.Inter (0)))),
                 (Just (Idx.AbsoluteState {Idx.unAbsoluteState = 300}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 0.9}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 2.0}}) 
                  (TotalBalanceForce (Interp.Inter (0)))),                             
                 (Just (Idx.AbsoluteState {Idx.unAbsoluteState = 500}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 2.9}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 2.0}}) 
                  (TotalBalanceForce (Interp.Inter (0))))]),                             
  ValueState.Map (Map.fromList [(Just (Idx.AbsoluteState {Idx.unAbsoluteState = 0}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Invalid [""]}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 5.0}}) 
                  (TotalBalanceForce (Interp.Inter (0)))),
                 (Just (Idx.AbsoluteState {Idx.unAbsoluteState = 300}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 0.9}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 2.0}}) 
                  (TotalBalanceForce (Interp.Inter (0)))),                             
                 (Just (Idx.AbsoluteState {Idx.unAbsoluteState = 500}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 0.9}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 2.0}}) 
                  (TotalBalanceForce (Interp.Inter (0))))])]}                              
   

(stateChoiceF,condSigF) = OptSignal.findOptimalStatesUsingMaxEta (nc "main") OptSignal.StateForcingOn optimalStateSignals
(stateChoiceU,condSigU) = OptSignal.findOptimalStatesUsingMaxEta (nc "main") OptSignal.StateForcingOff optimalStateSignals


optimalStateSignal2 =  SignalFlow.map (ValueState.getBest Interp.compareMaxWithInvalid) conditionedSignal 

----------------------------
caller = nc "Main"

etaOptSignal = SignalFlow.map (ValueState.map FlowOpt.getOptEtaVal) optimalStateSignals 
    
etaOptSignalMax = SignalFlow.map (Maybe.fromMaybe err2 . snd) stateChoiceU 
    
    
diffSignal = SignalFlow.zipWith (\opt optMax -> ValueState.map (\x -> optMax Arith.~- x) opt) etaOptSignal etaOptSignalMax              
    
minDifferencePerState = Maybe.fromMaybe err $ SignalFlow.foldl f (Nothing) diffSignal
    
f (Nothing) y = Just y
    
f (Just x) y = Just $ ValueState.minWith (Interp.compareMinWithInvalid) x y
    
err = merror caller modul "findOptimalStatesUsingMaxEta" "empty Signal"
    
err2 = merror caller modul "findOptimalStatesUsingMaxEta" "no State found"
    
conditionedSignal = SignalFlow.map (\ m -> ValueState.zipWith (Arith.~+) m minDifferencePerState) etaOptSignal 

plotIO = Plot.plotSync DefaultTerm.cons

main = do
  
--  plotIO $ OptSignalPlot.plotOptimalSignals "" $ OptSignalAccess.optimalityPerStateSignalToSignalMap FlowOpt.getEtaVal optimalStateSignals
  plotIO $ PlotD2.allInOne (PlotD2.labledFrame "Max") (\_ _ -> id) $ PlotFlowSignal.toPlotData (Just "Max") etaOptSignalMax
  plotIO $ OptSignalPlot.plotOptimalStateChoice ("State Choice Forced") FlowOpt.getEtaVal (stateChoiceF,optimalStateSignals,condSigF)
  plotIO $ OptSignalPlot.plotOptimalStateChoice ("State Choice Unforced") FlowOpt.getEtaVal (stateChoiceU,optimalStateSignals,condSigU)
--  plotIO $ OptSignalPlot.plotOptimalStateChoice ("State Choice Unforced") FlowOpt.getEtaVal (stateChoiceU,optimalStateSignals,condSigU)
    
  
  
--  print optimalStateSignal
  
  print "-----------EtaPerState------------------" 
  
  print etaOptSignal
  
  print "-----------MaxEta------------------" 
  
--  print maxSig
  
  print "-----------Diff------------------" 
  
  print diffSignal
  
  print "-----------MinDiff------------------" 
  
  print minDifferencePerState


  print "-----------CondSig------------------" 
  
  print conditionedSignal
  
  print "-----------Test------------------" 
  
--  print $ optimalStateSignal
  print $ optimalStateSignal2 