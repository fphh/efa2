
module EFA2.Signal.Sequence where

import Data.List
import qualified Data.Map as M
import Control.Monad

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as GV


import System.Random
import EFA2.Utils.Utils
import EFA2.Signal.SignalData
import EFA2.Signal.SignalGeneration

-----------------------------------------------------------------------------------
-- Record -- Structure for Handling recorded data

-- indent for power signals from measurement file 
-- ident chosen as string to handle signal names, or just can be signal numer as string 
data SignalIdent = SigIdent !String deriving (Show, Eq, Ord)

-- data structure to house the data record or parts of it
data Record = Record (UV.Vector TSample) (M.Map SignalIdent (UV.Vector PSample)) deriving (Show,Eq) 

-- generate Record from data components
genRecord :: UV.Vector TSample -> [(SignalIdent, UV.Vector PSample)] -> Record
genRecord time sigIDList = if dataCheck time (map snd sigIDList) ==True then rec else error ("Error in genRecord - unequal length of input data vectors")
  where rec = Record time (M.fromList sigIDList)
                                              
-- check Record Data -- TODO -- include check on time length == sign length                                                               
dataCheck :: UV.Vector TSample ->   [UV.Vector PSample] -> Bool
dataCheck time sigs = all (UV.all sampleCheck) sigs && equalLengths sigs && (length sigs) > 0
    
-- check for NaN's of Infs in original Data
sampleCheck :: PSample -> Bool     
sampleCheck s = not (isNaN s)

-- check for same vector length
equalLengths :: (UV.Unbox a) => [UV.Vector a] -> Bool
equalLengths vec | length vec == 0 = True
equalLengths xs = and (map (== n) ns)
  where (n:ns) = map UV.length xs

  
-----------------------------------------------------------------------------------
-- Section and Sequence -- Structures to handle Sequence Information and Data

-- Section analysis result
type Sequ = GV.Vector Sec 
            
data Sec = Sec { secLen         ::  SectionLength,
                 secTimes       ::  (TSample,TSample), 
                 secStepIndices  ::  (SampleIdx,SampleIdx),
                 secStepTypes    ::  (StepType,StepType)} deriving (Show, Eq)


-- data structure to contain output of section analysis (cvutting history in slices)
type SectionLength = DTSample

-- Sequence Vector to Store Section Data  
data SequData a = SequData (GV.Vector a) deriving Show

-- Init == first Sample / End == Last Sample
data StepType = NoStep | InitStep | LeavesZeroStep | BecomesZeroStep | ZeroCrossingStep | EndStep deriving (Show,Eq,Ord)


-------------------------------------------------------------------------------------
-- functions to split the record in section records

data Sign = PSign | ZSign | NSign deriving (Show, Eq)

-- determine Signal Sign  
sign :: (Eq a, Ord a, Num a) => a -> Sign
sign x | x > 0 = PSign
       | x == 0 = ZSign -- TODO add intervalls later on Zero - Detection       
       | x < 0 = NSign
                 
-- bild Section data structure from info of two steps 
genSecInfo :: (SampleIdx,TSample,StepType) -> (SampleIdx,TSample,StepType) -> Sec
genSecInfo (idx1,t1,step1) (idx2,t2,step2) =  Sec { secLen         = DTSample (fromSample (t2-t1)),
                                                    secTimes       = (t1,t2),              
                                                    secStepIndices  = (idx1,idx2),
                                                    secStepTypes   = (step1,step2)}
  
-- calculate exact time of Zero Crossing Point                 
calcZeroTime :: (TSample,PSample) -> (TSample,PSample) -> TSample 
calcZeroTime (TSample t1,PSample p1) (TSample t2,PSample p2) = if tzero < t2 && tzero > t1 then (toSample tzero) else error ("Zero Point out of Time-Intervall") 
  where m = dp/ dt -- interpolation slope 
        dp = p2-p1 -- delta power 
        dt = t2-t1 -- delta time -- t1 comes in time before t2
        tzero = t1+p1/m -- time of zero crossing 
                 
-- detect Sign-Change per Signal / delivers indice of event and type of event
getSignalSteps :: UV.Vector PSample -> [(SampleIdx,StepType)] 
getSignalSteps sig = zip (map SampleIdx idxList) (map (crossSig GV.!) idxList )
  where sig2 = GV.fromList (tail (UV.toList sig)) -- vector shifted by one
        sig1 = GV.fromList (init (UV.toList sig)) -- shortened vector
        
        crossSig = GV.zipWith f sig2 sig1
        idxList = GV.toList (GV.findIndices (/=NoStep) crossSig) -- find all events index of pint before change is delivered        
        -- with f:
        f s2 s1 | sign s1==ZSign && sign s2 /= ZSign = LeavesZeroStep -- signal leaves zero
        f s2 s1 | sign s2==ZSign && sign s1 /= ZSign = BecomesZeroStep -- signal becomes zero
        f s2 s1 | sign s2==PSign && sign s1 /= NSign = ZeroCrossingStep  -- signal is crossing zero
        f s2 s1 | sign s1==PSign && sign s2 /= NSign = ZeroCrossingStep  -- signal is crossing zero
        f s2 s1 | otherwise = NoStep  -- nostep
 
-- generate Sequence Information   
genSequ :: Record -> Sequ
genSequ (Record time sigMap) = GV.fromList (zipWith genSecInfo (init stepList3) (tail stepList3))  
  where stepList1 = sort (concat (map (getSignalSteps . snd)  (M.toList sigMap))) -- get Steps from all Signals
        stepList2 = [(0,InitStep)] ++ stepList1 ++ [(SampleIdx (UV.length time) ,EndStep)] -- add steps for 1st and last sample in first and last section
        stepList3 = zip3 (fst (unzip stepList2)) stepTimes (snd(unzip stepList2))
        stepTimes = map (map f (M.elems sigMap)) stepList3 -- calculate Step time
          where f sig (stepIdx,InitStep) = time UV.! stepIdx
                f sig (stepIdx,EndStep) = time UV.! (stepIdx+1)
                f sig (stepIdx,LeavesZeroStep) = time UV.! stepIdx
                f sig (stepIdx,BecomesZeroStep) = time UV.! (stepIdx+1) 
                f sig (stepIdx,ZeroCrossingStep) = calcZeroTime (time UV.! stepIdx, sig UV.! stepIdx)  (time UV.! (stepIdx+1), sig UV.! (stepIdx+1)) 


genSecRec :: Record -> Sec -> Record
genSecRec (Record time sigMap) sec  = Record (sliceTime time sec) (M.map (sliceSignal sec) sigMap)  
    
genRecSequ :: Sequ -> Record -> SequData Record 
genRecSequ sequ rec = SequData (GV.map (genSecRec rec) sequ)  
               
-- extract slice of one signal
sliceSignal :: Sec -> UV.Vector PSample -> UV.Vector PSample
sliceSignal sec sig  = (sigHead step1) UV.++ sigTrunk UV.++ (sigTail step2)
        where
          sigTrunk = UV.slice (idx1+inc) (idx2-(idx1+inc)) -- get middle part which is always same 
          step1 = fst(secStepTypes sec)
          step2 =  snd (secStepTypes sec)
          idx1 = fst(secStepIndices sec)
          idx2 =  snd (secStepIndices sec)
          inc = SampleIdx 1
                    
          sigHead LeavesZeroStep =  [sig UV.! idx1]
          sigHead InitStep =  [sig UV.! idx1]
          sigHead BecomesZeroStep =  []
          sigHead EndStep = error ("Error in sliceSignal - endStep shouldn't occur here") 
          sigHead ZeroCrossingStep =  [0]
          
          sigTail LeavesZeroStep =  [0]
          sigTail InitStep = error ("Error in sliceSignal - initStep shouldn't occur here")
          sigTail BecomesZeroStep =  [sig UV.! idx2+1]
          sigTail EndStep =  [sig UV.! idx2+1]
          sigTail ZeroCrossingStep =  [0]
          
-- extract slice of one signal
sliceTime :: UV.Vector TSample -> Sec -> UV.Vector TSample
sliceTime sig sec = (sigHead step1) UV.++ sigTrunk UV.++ (sigTail step2)
        where
          sigTrunk = UV.slice (idx1+1) (idx2-(idx1+1)) -- get middle part which is always same 
          step1 = fst(secStepTypes sec)
          step2 =  snd (secStepTypes sec)
          t1 = fst(secTimes sec)
          t2 = snd(secTimes sec)
          idx1 = fst(secStepIndices sec)
          idx2 =  snd (secStepIndices sec)
          
          sigHead LeavesZeroStep =  [sig UV.! idx1]
          sigHead InitStep =  [sig UV.! idx1]
          sigHead BecomesZeroStep =  []
          sigHead EndStep = error ("Error in sliceSignal - endStep shouldn't occur here") 
          sigHead ZeroCrossingStep =  [t1]
          
          sigTail LeavesZeroStep =  [0]
          sigTail InitStep = error ("Error in sliceSignal - initStep shouldn't occur here")
          sigTail BecomesZeroStep =  [sig UV.! idx2+1]
          sigTail EndStep =  [sig UV.! idx2+1]
          sigTail ZeroCrossingStep =  [t2]

          
          
-- -- TODO -- include length checking !!
-- checkLength :: Section a -> Either SectionError ()
-- checkLength sec = when (secLen sec < 2) $ Left ShortSection


        


