{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}

module EFA2.Signal.SignalAnalysis where

import Control.Monad
import Data.List (zip5,zipWith4)

import qualified Data.Map as M
import qualified Data.Bimap as BM
import qualified Data.Foldable as F

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as GV

import Data.Either

import Debug.Trace

-- import EFA2.Graph.GraphData

import EFA2.Signal.SignalData
-- import EFA2.Signal.Sequence

-- import EFA2.Signal.SignalGraph

import EFA2.Utils.Utils


data Sign = PSign | ZSign | NSign deriving (Show, Eq)

-- Generische Funktion für Daten - Container
absd :: Sample a => Val -> Val -> Signal a -> Signal a
absd  w phi time = dmap f time where f x = abs x


-- check for NaN's 
sampleCheck :: (DataAll cont a) => cont a -> Bool     
sampleCheck d = dall (not . isNaN) d





-- check for same vector length
equalLengths :: (UV.Unbox a) => [UV.Vector a] -> Bool
equalLengths vec | length vec == 0 = True
equalLengths xs = and (map (== n) ns)
  where (n:ns) = map UV.length xs

-- determine Signal Sign  
sign :: (Eq a, Ord a, Num a) => a -> Sign
sign x | x > 0 = PSign
       | x == 0 = ZSign -- TODO add intervalls later on Zero - Detection       
       | x < 0 = NSign
  
-- calculate exact time of Zero Crossing Point                 
calcZeroTime :: (TSample,PSample) -> (TSample,PSample) -> TSample 
calcZeroTime (TSample t1,PSample p1) (TSample t2,PSample p2) = if tzero < t2 && tzero > t1 then (toSample tzero) else error ("Zero Point out of Time-Intervall") 
  where m = dp/ dt -- interpolation slope 
        dp = p2-p1 -- delta power 
        dt = t2-t1 -- delta time -- t1 comes in time before t2
        tzero = t1+p1/m -- time of zero crossing 
                  

-- Init == first Sample / End == Last Sample
type StepType = Int

-- detect Sign-Change per Signal / delivers indice of event and type of event
getSignalSteps :: Time -> Power -> [(SignalIdx,TSample,StepType)] 
getSignalSteps time psig@(Signal pvec) = zip3 stepIdxListTyped stepTimeList stepList
  where stepVec = uvdiffMap f pvec -- length reduced by one !!!
        -- with f:
        f s2 s1 | sign s1==ZSign && sign s2 /= ZSign = 1 -- LeavesZeroStep -- signal leaves zero
        f s2 s1 | sign s2==ZSign && sign s1 /= ZSign = 2 -- BecomesZeroStep -- signal becomes zero
        f s2 s1 | sign s2==PSign && sign s1 /= NSign = 3 -- ZeroCrossingStep  -- signal is crossing zero
        f s2 s1 | sign s1==PSign && sign s2 /= NSign = 3 -- ZeroCrossingStep  -- signal is crossing zero
        f s2 s1 | otherwise = 0 -- 0 NoStep  -- nostep
        -- with g:
        -- finds all events indices at point before step in time signal
        stepIdxList = UV.toList $ UV.findIndices ( /= 0) stepVec 
        stepList = (map (stepVec UV.!) stepIdxList)        
                   
        stepIdxListTyped = map (SignalIdx) stepIdxList -- create typed Index List to allow Signal Usage
        stepTimeList = map g (zip stepIdxListTyped stepList)
        g (stepIdx,1) = time ! stepIdx
        g (stepIdx,2) = time ! (stepIdx+1) 
        -- mixed Indexing doesn't work !!! (on a signal with PSample and TSample in the same Name-Space)
        g (stepIdx,3) = calcZeroTime (time ! stepIdx, pvec UV.! (fromIdx stepIdx))  (time ! (stepIdx+1), pvec UV.! (fromIdx (stepIdx+1))) 
                 