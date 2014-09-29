{-# LANGUAGE FlexibleContexts #-}

module EFA.Action.Optimisation.Signal.Access where
  
import qualified EFA.Action.Optimisation.Signal as OptSignal
import qualified EFA.Action.Flow.Optimality as FlowOpt

import qualified EFA.Data.Vector as DV

import qualified EFA.Data.Interpolation as Interp

import qualified EFA.Value.State as ValueState
import qualified EFA.Data.OD.Signal.Flow as SignalFlow

import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Equation.Arithmetic as Arith


import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

optimalityPerStateSignalToSignalMap :: 
  (DV.Walker vec,DV.Storage vec (Interp.Val b),DV.Storage vec (ValueState.Map (Interp.Val b)),
   DV.Storage  vec (ValueState.Map (FlowOpt.OptimalityValues (Interp.Val b)))) =>
  (FlowOpt.OptimalityValues (Interp.Val b) -> (Interp.Val b)) ->
  OptSignal.OptimalityPerStateSignal node inst vec a  (Interp.Val b) ->
  Map.Map (Maybe Idx.AbsoluteState) (SignalFlow.Signal inst String vec a (Interp.Val b))
optimalityPerStateSignalToSignalMap faccess signal = Map.fromList $ map f states 
  where states = ValueState.states $ SignalFlow.foldl (ValueState.union) ValueState.empty signal
        f st = (st,SignalFlow.map (\m -> Maybe.fromMaybe (Interp.Invalid ["optimalityPerStateSignalToSignalMap"]) $ 
                                         ValueState.lookUp m st) sig)
        sig = SignalFlow.map (ValueState.map faccess) signal   
        

perStateSignalToSignalMap ::
  (DV.Walker vec,
   DV.Storage vec (ValueState.Map (Interp.Val b)),
   DV.Storage vec (Interp.Val b)) =>
  SignalFlow.Signal inst label vec a (ValueState.Map (Interp.Val b)) ->
  Map.Map (Maybe Idx.AbsoluteState) (SignalFlow.Signal inst label vec a (Interp.Val b))
perStateSignalToSignalMap signal =  Map.fromList $ map f states
  where states =  ValueState.states $ SignalFlow.foldl (ValueState.union) ValueState.empty signal
        f st = (st,SignalFlow.map (\m -> Maybe.fromMaybe (Interp.Invalid ["optimalityPerStateSignalToSignalMap"]) $ 
                                         ValueState.lookUp m st) signal)


        
stateChoiceToSignal :: 
  (Arith.Constant b, 
   Show (vec [SignalFlow.TimeStep a]),
   Show (vec (SignalFlow.TimeStep a)),
   Arith.Constant a,
   DV.Zipper vec,
   DV.Storage vec a,
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.Storage vec [SignalFlow.TimeStep a],
   DV.Storage vec (Interp.Val b),
   DV.Storage vec [a],
   DV.Singleton vec,
   DV.FromList vec, 
   DV.Walker vec,
   DV.Storage vec [Interp.Val b],
   DV.Storage vec ([Maybe Idx.AbsoluteState], Maybe (Interp.Val b)))=>
  OptSignal.OptimalStateChoice node inst vec a  (Interp.Val b) ->
  SignalFlow.Signal inst String vec a (Interp.Val b)
stateChoiceToSignal signal = SignalFlow.concatEvenEvenTimeShare sig
  where sig = SignalFlow.map (map (Maybe.fromMaybe (Arith.negate $ Arith.one) . 
                                   fmap (Arith.fromInteger . fromIntegral . Idx.absoluteState2Int)) . fst) signal       
