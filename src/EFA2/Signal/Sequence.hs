
module EFA2.Signal.Sequence where

import Data.List
import qualified Data.Map as M
import Control.Monad

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as GV

import EFA2.Graph.GraphData
import EFA2.Term.Env

import System.Random
import EFA2.Utils.Utils
import EFA2.Signal.Arith

-----------------------------------------------------------------------------------
-- Record -- Structure for Handling recorded data

-- indent for power signals from measurement file 
-- ident chosen as string to handle signal names, or just can be signal numer as string 
data SigId = SigId !String deriving (Show, Eq, Ord)
type SignalMap = (M.Map SigId Power) 
  
-- data structure to house the data record or parts of it
data Record = Record Time SignalMap deriving (Show,Eq) 
type PowerSigEnv = PowerMap Power


{-
-- generate Record from data components
genRecord :: Time  -> [(SignalIdent, Power)] -> Record
genRecord time sigIDList = if recordCheck rec == True then rec else error ("Incorrect Data in Record-- either unequal length or NaN's")
  where rec = Record time (M.fromList sigIDList)
                                              

-- check Record Data -- TODO -- include check on time length == sign length                                                               
recordCheck :: Record -> Bool
recordCheck (Record time sigMap) = smplCheck && equlengthCheck && lengthCheck
  where 
    list = [time2Power time] ++ M.elems sigMap -- all signals and time in one list
    smplCheck = all (sampleCheck) list
    equlengthCheck = equalLengths ([time2Power time] ++ list)  -- equal length on all signals
    lengthCheck = all (1 < ) $ map dlength list -- at least two samples per time Signal
 
-----------------------------------------------------------------------------------
-- Section and Sequence -- Structures to handle Sequence Information and Data

-- Section analysis result
type Sequ = GV.Vector Sec 
            
data Sec = Sec { secLen         ::  SectionLength,
                 secTimes       ::  (TSample,TSample), 
                 secStepIndices  ::  (SignalIdx,SignalIdx),
                 secStepTypes    ::  (StepType,StepType)} deriving (Show, Eq)


-- data structure to contain output of section analysis (cvutting history in slices)
type SectionLength = DTSample

-- Sequence Vector to Store Section Data  
data SequData a = SequData (GV.Vector a) deriving Show
-}

{-
-------------------------------------------------------------------------------------
-- functions to split the record in section records
-- generate Sequence Information   
genSequ :: Record -> Sequ
genSequ (Record time sigMap) = GV.fromList (zipWith genSecInfo (init stepList2) (tail stepList2))  
  where stepList1 = sort(concat (map ((getSignalSteps time) . snd)  (M.toList sigMap))) -- get Steps from all Signals
        stepList2 = [(SignalIdx 0,dhead time, 4)] ++ stepList1 ++ [(SignalIdx $ dlength time, dlast time ,5)] -- add steps events for 1st and last sample in first and last section (1 = leaves zero / 2 == becomes zero)


                
-- bild Section data structure from info of two steps 
genSecInfo :: (SignalIdx,TSample,StepType) -> (SignalIdx,TSample,StepType) -> Sec
genSecInfo (idx1,t1,step1) (idx2,t2,step2) =  Sec { secLen         = DTSample (fromSample (t2-t1)),
                                                    secTimes       = (t1,t2),              
                                                    secStepIndices  = (idx1,idx2),
                                                    secStepTypes   = (step1,step2)}
  
                
    
genRecSequ :: Sequ -> Record -> SequData Record 
genRecSequ sequ rec = SequData (GV.map (genSecRec rec) sequ)  
               
genSecRec :: Record -> Sec -> Record
genSecRec (Record time sigMap) sec  = Record (sliceTime time sec) (M.map (sliceSignal sec) sigMap)  


-- extract slice of one signal
sliceSignal :: Sec -> Power -> Power
sliceSignal sec sig  = dfromList (sigHead step1) .++ sigTrunk .++ dfromList (sigTail step2)
        where
          sigTrunk = dslice sig (idx1+1,idx2-idx1+1) -- get middle part which is always same 
          step1 = fst(secStepTypes sec)
          step2 =  snd (secStepTypes sec)
          idx1 = fst(secStepIndices sec)
          idx2 =  snd (secStepIndices sec)
                    
          sigHead 1 =  [sig ! idx1] --  LeavesZeroStep
          sigHead 4 =  [sig ! idx1] -- InitStep
          sigHead 2 =  [] -- BecomesZeroStep
          sigHead 5 = error ("Error in sliceSignal - endStep shouldn't occur here") -- EndStep
          sigHead 3 =  [0] -- ZeroCrossingStep
          
          sigTail 1 =  [0] -- LeavesZeroStep
          sigTail 4 = error ("Error in sliceSignal - initStep shouldn't occur here") -- InitStep
          sigTail 2 =  [sig ! idx2+1] -- BecomesZeroStep
          sigTail 5 =  [sig ! idx2+1] -- EndStep
          sigTail 3 =  [0] -- ZeroCrossingStep
          

-- extract slice of one signal
sliceTime :: Time -> Sec -> Time
sliceTime time sec = dfromList (sigHead step1) .++ sigTrunk .++ dfromList (sigTail step2)
        where
          sigTrunk = dslice time (idx1+1, idx2-idx1+1) -- get middle part which is always same 
          step1 = fst(secStepTypes sec)
          step2 =  snd (secStepTypes sec)
          t1 = fst(secTimes sec)
          t2 = snd(secTimes sec)
          idx1 = fst(secStepIndices sec)
          idx2 = snd (secStepIndices sec)
          
          sigHead 1 =  [time ! idx1] -- LeavesZeroStep
          sigHead 4 =  [time  ! idx1] -- InitStep
          sigHead 2 =  [] -- BecomesZeroStep
          sigHead 5 = error ("Error in sliceSignal - endStep shouldn't occur here") -- EndStep
          sigHead 3 =  [t1] -- ZeroCrossingStep
          
          sigTail 1 =  [0] -- LeavesZeroStep
          sigTail 4 = error ("Error in sliceSignal - initStep shouldn't occur here") -- InitStep
          sigTail 2 =  [time ! (idx2+1)] -- BecomesZeroStep
          sigTail 5 =  [time ! (idx2+1)] -- EndStep
          sigTail 3 =  [t2] -- ZeroCrossingStep

          
          

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
                                                         -- 4 == InitStep -- will be added in analysis on all signals
                                                         -- 5 = EndStep -- will be added in analysis on all signals
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
   

  


-- -- TODO -- include length checking !!
-- checkLength :: Section a -> Either SectionError ()
-- checkLength sec = when (secLen sec < 2) $ Left ShortSection

-}