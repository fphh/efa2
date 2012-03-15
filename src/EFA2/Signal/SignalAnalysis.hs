{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, ScopedTypeVariables #-}

module EFA2.Signal.SignalAnalysis where

import Control.Monad
import Data.List (zip5,zipWith4)

import qualified Data.Map as M
import qualified Data.Bimap as BM
import qualified Data.Foldable as F

import qualified Data.Vector.Unboxed as UV
import Data.Either

import Debug.Trace

-- import EFA2.Graph.GraphData

import EFA2.Signal.SignalData
import EFA2.Signal.Sequence

-- import EFA2.Signal.SignalGraph

import EFA2.Utils.Utils


data Sign = PSign | ZSign | NSign deriving (Show, Eq)


-- Generische Signal - Funktionen -- brauchen daher keine Typ-Sicherheit

-- check for NaN's of Infs in original Data
sampleCheck :: TimeSignal -> Bool     
sampleCheck s = UV.all (not . isNaN) (getVect s)

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
                  
-- detect Sign-Change per Signal / delivers indice of event and type of event
getSignalSteps :: Time -> PTSig -> [(TimeSampleIdx,TSample,StepType)] 
getSignalSteps time sig = zip3 (map SampleIdx idxList) (map g (zip idxList stepList)) stepList
  where sig2 = GV.fromList (tail (UV.toList sig)) -- vector shifted by one
        sig1 = GV.fromList (init (UV.toList sig)) -- shortened vector
        crossSig = GV.zipWith f sig2 sig1
        idxList = GV.toList (GV.findIndices (/=NoStep) crossSig) -- find all events index of pint before change is delivered        
        stepList = (map (crossSig GV.!) idxList )
        -- with f:
        f s2 s1 | sign s1==ZSign && sign s2 /= ZSign = LeavesZeroStep -- signal leaves zero
        f s2 s1 | sign s2==ZSign && sign s1 /= ZSign = BecomesZeroStep -- signal becomes zero
        f s2 s1 | sign s2==PSign && sign s1 /= NSign = ZeroCrossingStep  -- signal is crossing zero
        f s2 s1 | sign s1==PSign && sign s2 /= NSign = ZeroCrossingStep  -- signal is crossing zero
        f s2 s1 | otherwise = NoStep  -- nostep
        -- with g:
        g (stepIdx,InitStep) = time UV.! stepIdx
        g (stepIdx,EndStep)  = time UV.! (stepIdx+1)
        g (stepIdx,LeavesZeroStep) = time UV.! stepIdx
        g (stepIdx,BecomesZeroStep) = time UV.! (stepIdx+1) 
        g (stepIdx,ZeroCrossingStep) = calcZeroTime (time UV.! stepIdx, sig UV.! stepIdx)  (time UV.! (stepIdx+1), sig UV.! (stepIdx+1)) 
                 