module Main where

import qualified EFA.Equation.Arithmetic as Arith 

import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Flow.SequenceState.Index as Idx
import EFA.Action.Flow.Optimality hiding (modul,nc)
import qualified EFA.Action.Flow.Optimality as FlowOpt
import qualified EFA.Action.Flow.Balance as Balance

import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Data.OD.Signal.Flow as SignalFlow
import qualified EFA.Value.Type as Type
import qualified EFA.Data.Interpolation as Interp

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
  Strict.Axis {Strict.getLabel = "Time", Strict.getType = Type.T, Strict.getVec = [SignalFlow.TimeStep {SignalFlow.getMidTime = 0.0, SignalFlow.getTimeStep = 1.0},SignalFlow.TimeStep {SignalFlow.getMidTime = 1.0, SignalFlow.getTimeStep = 1.0}]} 
  SignalFlow.Data {SignalFlow.getVector = [
  ValueState.Map (Map.fromList [(Just (Idx.AbsoluteState {Idx.unAbsoluteState = 0}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 9}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 7}}) 
                  (TotalBalanceForce (Interp.Inter (0)))),
                 (Just (Idx.AbsoluteState {Idx.unAbsoluteState = 55}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 0.1}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 2.0}}) 
                  (TotalBalanceForce (Interp.Inter (0))))]),
  ValueState.Map (Map.fromList [(Just (Idx.AbsoluteState {Idx.unAbsoluteState = 0}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 3.2}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 5.0}}) 
                  (TotalBalanceForce (Interp.Inter (0)))),
                 (Just (Idx.AbsoluteState {Idx.unAbsoluteState = 55}),
                  OptimalityValues (OptimalityMeasure {getEta = EtaSys {unEta2Optimise = Interp.Inter 0.9}, 
                                                       getLoss = LossSys {unLoss2Optimise = Interp.Inter 2.0}}) 
                  (TotalBalanceForce (Interp.Inter (0))))])]}                              
  

optimalStateSignal = OptSignal.findOptimalStatesUsingMaxEta (nc "main") OptSignal.StateForcingOn optimalStateSignals

optimalStateSignal2 =  SignalFlow.map (ValueState.getBest Interp.compareMaxWithInvalid) conditionedSignal 

----------------------------
caller = nc "Main"

etaOptSignal = SignalFlow.map (ValueState.map FlowOpt.getOptEtaVal) optimalStateSignals -- optimalitySignalPerState
    
maxSig = OptSignal.findOptimalStatesUsingMaxEta (caller |> nc "findOptimalStatesUsingMaxEta") 
                      OptSignal.StateForcingOff optimalStateSignals -- optimalitySignalPerState 

etaOptSignalMax = SignalFlow.map (Maybe.fromMaybe err2 . snd) maxSig 
    
    
diffSignal = SignalFlow.zipWith (\opt optMax -> ValueState.map (\x -> optMax Arith.~- x) opt) etaOptSignal etaOptSignalMax              
    
minDifferencePerState = Maybe.fromMaybe err $ SignalFlow.foldl f (Nothing) diffSignal
    
f (Nothing) y = Just y
    
f (Just x) y = Just $ ValueState.minWith (Interp.compareMinWithInvalid) x y
    
err = merror caller modul "findOptimalStatesUsingMaxEta" "empty Signal"
    
err2 = merror caller modul "findOptimalStatesUsingMaxEta" "no State found"
    
conditionedSignal = SignalFlow.map (\ m -> ValueState.zipWith (Arith.~+) m minDifferencePerState) etaOptSignal 



main = do
  print optimalStateSignal
  
  print "-----------EtaPerState------------------" 
  
  print etaOptSignal
  
  print "-----------MaxEta------------------" 
  
  print maxSig
  
  print "-----------Diff------------------" 
  
  print diffSignal
  
  print "-----------MinDiff------------------" 
  
  print minDifferencePerState


  print "-----------CondSig------------------" 
  
  print conditionedSignal
  
  print "-----------Test------------------" 
  
  print $ optimalStateSignal
  print $ optimalStateSignal2 