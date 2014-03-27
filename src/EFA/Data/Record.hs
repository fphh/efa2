{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.Record where

import EFA.Utility(Caller,merror,(|>),ModuleName(..),FunctionName, genCaller)

import Prelude hiding ((++))

import qualified EFA.Data.Vector as DV
import qualified EFA.Data.OrdData as Ord

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.NonEmpty as NonEmpty

import qualified EFA.Equation.Arithmetic as Arith((~/),(~*),(~+),(~-))


--newtype Record vec vec1 a = Record (NE (NE vec) (DataRow vec1 a))
type NE vec = NonEmpty.T vec
newtype Record vec vec1 a = Record (vec (DataRow vec1 a))
type SignalRecord vec vec1 a = Record (NE (NE vec)) vec1 a
type DeltaRecord vec vec1 a = Record (NE vec) vec1 a
type DataRow vec a = (Ord.Data a, vec a)


map :: (DV.Walker (NE (NE vec)), DV.Walker vec,
        DV.Storage vec (Ord.Data a, vec1 a), 
        DV.Walker vec1, DV.Storage vec1 a)=>
       (a -> a) -> Record vec vec1 a -> Record vec vec1 a 
map f (Record rec) = Record $ DV.map (\(t,vec) -> (t,DV.map f vec)) rec


mapWithTime :: (DV.Walker (NE (NE vec)),DV.Walker vec,
        DV.Storage vec (Ord.Data a, vec1 a), 
        DV.Walker vec1, DV.Storage vec1 a)=>
       (Ord.Data a -> a -> a) -> Record vec vec1 a -> Record vec vec1 a 
mapWithTime f (Record rec) = Record $ DV.map (\(t,vec) -> (t,DV.map (f t) vec)) rec

deltaMapWithTime :: (DV.Walker (NE (NE vec)),DV.Walker vec,
        DV.Storage vec (Ord.Data a, vec1 a), 
        DV.Walker vec1, DV.Storage vec1 a)=>
       (Ord.Data a -> a -> a) -> Record vec vec1 a -> Record vec vec1 a 
deltaMapWithTime f (Record rec) = Record $ DV.map (\(t,vec) -> (t,DV.map (f t) vec)) rec


(++) :: 
  (DV.Storage (Record vec vec1) a,
   DV.Singleton (Record vec vec1)) => 
  Record vec vec1 a -> Record vec vec1 a -> Record vec vec1 a
(++) x y = DV.append x y 


head :: (DV.Storage vec (Ord.Data a, vec1 a),
         DV.Singleton (NE (NE vec)),DV.Singleton vec)=>
        Record vec vec1 a -> DataRow vec1 a
head (Record rec) = DV.head rec

tail :: (DV.Storage vec (Ord.Data a, vec1 a),DV.Singleton vec,
         DV.Singleton (NE (NE vec)))=>
        Record vec vec1 a -> Record vec vec1 a
tail (Record rec) = Record $ DV.tail rec

deltaMap ::  
  (DV.Zipper vec,
   DV.Storage vec (Ord.Data b, vec1 b),
   DV.Storage vec (Ord.Data a, vec1 a),
   DV.Singleton vec)=>
  (DataRow vec1 a -> DataRow vec1 a -> DataRow vec1 b) -> 
  Record vec vec1 a -> Record vec vec1 b
deltaMap f (Record rec) = 
   Record (DV.deltaMap f rec)
   
data ZeroCross = Crossing | NoCrossing
   
{- In Arbeit -- hier weiter machen
-- | 
zeroCrossing :: (Ord.Data a,a) -> (Ord.Data a,a) -> Ord.Data a
zeroCrossing (t,p) (t1,p1) = if t==t1 then t else t ~+ (p2~/m)   
  let m = (p2~-p1)~/(t2~-t1)
      
      
calcZeroCrossingTimes :: PowerPair a -> PowerPair a -> Ord.Data a 
calcZeroCrossingTimes (pA,pB) (pA1,pB1) = (t ~+ t1) ~/Arith.fromRational 2
  where t = calcZeroCrossingTime pA pA1
        t1 = calcZeroCrossingTime pB pB1

calcZeroCrossingTimes :: PowerPair a -> PowerPair a -> Ord.Data a 
calcZeroCrossingTimes (pA,pB) (pA1,pB1) = (t ~+ t1) ~/Arith.fromRational 2
  where t = calcZeroCrossingTime pA pA1
        t1 = calcZeroCrossingTime pB pB1

-}
