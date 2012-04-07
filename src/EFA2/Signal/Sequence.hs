
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

import EFA2.Signal.SignalAnalysis




-----------------------------------------------------------------------------------
-- Record -- Structure for Handling recorded data

-- indent for power signals from measurement file 
-- ident chosen as string to handle signal names, or just can be signal numer as string 
data SigId = SigId !String deriving (Show, Eq, Ord)
type SignalMap = (M.Map SigId Power) 
  
-- data structure to house the data record or parts of it
data Record = Record Time SignalMap deriving (Show,Eq) 
type PowerSigEnv = PowerMap Power

-----------------------------------------------------------------------------------
-- Section and Sequence -- Structures to handle Sequence Information and Data

-- Section analysis result
type Sequ = [Sec] 
            
data Sec = Sec { secLen         ::  SectionLength,
                 secTimes       ::  (TSample,TSample), 
                 secStepIndices  ::  (SignalIdx,SignalIdx),
                 secStepTypes    ::  (StepType,StepType)} deriving (Show, Eq)
-- data structure to contain output of section analysis (cvutting history in slices)
type SectionLength = DTSample

-- Sequence Vector to Store Section Data  
data SequData a = SequData [a] deriving Show

data StepType = InitStep | EndStep | LeavesZeroStep | BecomesZeroStep | ZeroCrossingStep | NoStep deriving (Eq, Show,Ord)

-----------------------------------------------------------------------------------
-- 
stepX :: PSample -> PSample -> StepType
stepX s1 s2 | sign s1==ZSign && sign s2 /= ZSign = LeavesZeroStep -- signal leaves zero
stepX s1 s2 | sign s1/=ZSign && sign s2 == ZSign = BecomesZeroStep -- signal becomes zero
stepX s1 s2 | sign s1==PSign && sign s2 == NSign = ZeroCrossingStep  -- signal is crossing zero
stepX s1 s2 | sign s1==NSign && sign s2 == PSign = ZeroCrossingStep  -- signal is crossing zero
                                                   --InitStep -- will be added in analysis on all signals
                                                   -- EndStep -- will be added in analysis on all signals
stepX s2 s1 | otherwise = NoStep  -- nostep
 

-- calculate exact time of Zero Crossing Point                 
calcZeroTime :: StepType -> (TSample,PSample) -> (TSample,PSample) -> TSample 
calcZeroTime stepType (t1,p1) (t2,p2) = f stepType
  where m = dp/ dt -- interpolation slope 
        dp = p2-p1 -- delta power 
        dt = t2-t1 -- delta time -- t1 comes in time before t2
        t = -p1/m+t1 -- time of zero crossing 
        tzero = t -- if t < t2 && t > t1 then t else error m1
        m1 = ("Zero Point out of Time-Intervall - t: " ++ show t ++ " m: " ++ show m ++ "t1: " ++ show t1 ++ "t2: " ++ show t2) 
        f InitStep = t1
        f EndStep = t2
        f LeavesZeroStep = t1
        f BecomesZeroStep = t2
        f ZeroCrossingStep = tzero
        f NoStep = error ("NoStep shouldn't occur here")


-- makeSteps :: Time -> Power -> [(SignalIdx,StepType,TSample)]
-- makeSteps time power | length power == 0 = []
-- makeSteps time power = [(0,InitStep,head time)] ++ reverse (res $ foldl f (last ss, 0, []) ss) ++ [(length ss-1,EndStep,last time)]
--   where ss  = zip time power
--         res (_, _, acc) = acc
--         f (x1@(t1,p1), i, acc) x2@(t2,p2) = if stepTyp == NoStep then (x2, i+1, acc) else (x2, i+1, (i+1,stepTyp,t):acc)
--             where
--                stepTyp = stepX p1 p2
--                t = calcZeroTime stepTyp x1 x2

-- | Function to generate a list containing all signal steps with time and index information 
makeSteps :: Time -> Power -> [(SignalIdx,StepType,TSample)]
makeSteps time power | length power == 0 = []
makeSteps time power = [(0,InitStep,head time)] ++ concat (dmap f ss) ++ [(length ss-1,EndStep,last time)]
  where ss  = idxList (zip time power) 
        f (idx1,(t1,p1)) (idx2,(t2,p2)) = if stepTyp == NoStep then [] else [(idx1,stepTyp,t)] 
            where
               stepTyp = stepX p1 p2
               t = calcZeroTime stepTyp (t1,p1) (t2,p2)

-- | generate a steplist for a Powermap
genSequ :: Time -> PowerMap Power -> Sequ
genSequ time pmap = dmap genSecInfo (S.toAscList s)
  where offs = map (makeSteps time) (M.elems pmap)
        s = S.unions $ map S.fromList offs -- convert steplist to set to use unions functionality 

-- | function to generate section Info        
genSecInfo :: (SignalIdx,StepType,TSample) -> (SignalIdx,StepType,TSample) -> Sec
genSecInfo (idx1,step1,t1) (idx2,step2,t2) =  Sec { secLen         = t2-t1,
                                                    secTimes       = (t1,t2),              
                                                    secStepIndices  = (idx1,idx2),
                                                    secStepTypes   = (step1,step2)}

{-
-------------------------------------------------------------------------------------
-- functions to split the record in section records
-- generate Sequence Information   
genSequ :: Record -> Sequ
genSequ (Record time sigMap) = GV.fromList (zipWith genSecInfo (init stepList2) (tail stepList2))  
  where stepList1 = sort(concat (map ((getSignalSteps time) . snd)  (M.toList sigMap))) -- get Steps from all Signals
        stepList2 = [(SignalIdx 0,dhead time, 4)] ++ stepList1 ++ [(SignalIdx $ dlength time, dlast time ,5)] -- add steps events for 1st and last sample in first and last section (1 = leaves zero / 2 == becomes zero)


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
 

-}

{-
  
                
    
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