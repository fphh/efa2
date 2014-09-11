{-# LANGUAGE FlexibleContexts #-}
  
module EFA.Data.OD.Signal.Chop where

import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Data.Vector as DV
import qualified EFA.Data.Sequence as DataSequ
import qualified EFA.Data.Interpolation as Interp

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Data as Data

import qualified EFA.Data.Axis.Strict as Strict
import qualified EFA.Data.OD.Signal.Flow as SignalFlow

import qualified Data.NonEmpty as NonEmpty
import qualified Data.NonEmpty.Set as NonEmptySet

import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Utility.Trace as UtTrace 
import qualified Debug.Trace as Trace 

import qualified Data.Map as Map

import EFA.Utility(Caller,
                  --merror,
                  (|>),
                   ModuleName(..),FunctionName, genCaller)


modul :: ModuleName
modul = ModuleName "Data.OD.Signal.Chop"

nc :: FunctionName -> Caller
nc = genCaller modul

type Sectioning = [Strict.Range]

chopHRecord ::  
  (Ord a,
   Show label,
   Show (vec (SignalFlow.TimeStep a)),
   Show key,
   Show (vec b), 
   Ord b,
   Arith.Constant b,
   DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec a,
   DV.Slice vec,
   DV.Length vec,
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.Len (vec b)) =>
  Caller ->
  b ->
  SignalFlow.HRecord key inst label vec a b ->
  DataSequ.List (SignalFlow.HRecord key inst label vec a b)
chopHRecord caller eps powerRecord = sequRecord
  where
    sectioning =  --UtTrace.simTrace  "Sections" $ 
                  findHSections caller eps $ 
                  --Trace.trace ("sigLength: " ++ show sigLength) 
                  powerRecord
    sigLength = Map.map (\(SignalFlow.Data vec) -> DV.len vec) $ SignalFlow.getHMap powerRecord
    sequRecord = sliceHRecord powerRecord sectioning



findHSections :: 
  (DV.Storage vec a, DV.Length vec, 
   Ord b,
   Arith.Constant b,
   DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec (SignalFlow.TimeStep a),
   DV.Len (vec b)) => 
  Caller ->
  b ->
  SignalFlow.HRecord key inst label vec a b -> Sectioning
findHSections caller eps (SignalFlow.HRecord time m) = 
  NonEmpty.toList $ NonEmpty.zipWith (Strict.Range) startIndexList stopIndexList
  where startIndexList = --UtTrace.simTrace  "startIndexList" $ 
                         NonEmptySet.toAscList $ foldl1 NonEmptySet.union $ 
                         map (SignalFlow.locateSignChanges (caller |> nc "findVSections") eps) $ Map.elems m
        stopIndexList = NonEmpty.snoc (DV.map (\(Strict.Idx idx)->Strict.Idx $ idx-1) $ 
                                       NonEmpty.tail startIndexList) (Strict.Idx $ (Strict.len time - 1))



sliceHRecord :: (DV.Storage vec b, DV.Slice vec, DV.Storage vec (SignalFlow.TimeStep a)) =>
  SignalFlow.HRecord key inst label vec a b -> 
  Sectioning -> 
  DataSequ.List (SignalFlow.HRecord key inst label vec a b)
sliceHRecord (SignalFlow.HRecord time m) sectioning = DataSequ.fromRangeList $ map f sectioning
  where
    f range= (range,SignalFlow.HRecord (Strict.getSlice range time) 
                     (Map.map (SignalFlow.getDataSlice range) m))


convertHRecord2Old :: 
  (DV.Walker sigVec,DV.Storage sigVec (SignalFlow.TimeStep a),
   DV.Storage sigVec (Interp.Val a),DV.Singleton sigVec,Arith.Sum a,Arith.Product a,Arith.Constant a,
   DV.Storage sigVec a) =>
  SignalFlow.HRecord (TopoIdx.Position node) inst label sigVec a (Interp.Val a) ->   
  Record.FlowRecord node sigVec (Interp.Val a)
convertHRecord2Old (SignalFlow.HRecord (Strict.Axis _ _ time) m) = 
  Record.Record (S.TC $ Data.Data absTime) (Map.map f m)
  where f (SignalFlow.Data vec) = S.TC (Data.Data vec)
        
        absTime = DV.map Interp.Inter $  DV.append (DV.singleton firstVal) absTimeTail
        
        firstVal = (\x -> SignalFlow.getMidTime x Arith.~- 
                          ((SignalFlow.getTimeStep x) Arith.~/ (Arith.one Arith.~+ Arith.one))) $ DV.head time 

        absTimeTail = DV.map (\x -> SignalFlow.getMidTime x Arith.~+
                                ((SignalFlow.getTimeStep x) Arith.~/ 
                                (Arith.one Arith.~+ Arith.one))) time


-- TODO:: Are Signal Indices in Sequence OK ? - are they used in further analysis 
convertToOld :: 
  (DV.Walker sigVec,Arith.Constant a, DV.Singleton sigVec,
   DV.Storage sigVec a,DV.Storage sigVec (SignalFlow.TimeStep a),
   DV.Storage sigVec (Interp.Val a)) =>
  DataSequ.List (SignalFlow.HRecord (TopoIdx.Position node) inst label sigVec a (Interp.Val a)) ->
  Sequ.List (Record.FlowRecord node sigVec (Interp.Val a))
convertToOld sequ = DataSequ.toOldSequence convertHRecord2Old sequ


