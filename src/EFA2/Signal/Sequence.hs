
module EFA2.Signal.Sequence where

import Control.Monad

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector.Unboxed as UV

import System.Random
import EFA2.Utils.Utils
import EFA2.Signal.SignalData
import EFA2.Signal.SignalGeneration

-------------------------------------------------------------------------------------
-- Section and Sequence Data Stucture


-- Section analysis result
type Sequ = GV.Vector Sec 
            
data Sec = Sec { secLen         ::  SectionLength,
                 secTimes       ::  (TSample,TSample), 
                 secStepIdices  ::  (SampleIdx,SampleIdx),
                 secSepTypes    ::  (StepType,StepType)}


-- data structure to contain output of section analysis (cvutting history in slices)
type SectionLength = DTSample

-- Sequence Vector to Store Section Data  
data SequData a = SequData (GV.Vector a) deriving Show

-- Init == first Sample / End == Last Sample
data StepType = NoStep | InitStep | LeavesZeroStep | BecomesZeroStep | ZeroCrossingStep | EndStep


-------------------------------------------------------------------------------------
-- functions to split the record

data Sign = PSign | ZSign | MSign deriving (Show, Eq)

-- determine Signal Sign  
sign :: (Eq a, Ord a, Num a) => a -> Sign
sign x | x > 0 = PSign
       | x == 0 = ZSign -- TODO add intervalls later on Zero - Detection       
       | x < 0 = NSign
                 
-- bild Section data structure from info of two steps 
genSecInfo :: (SampleIdx,TSample,StepType) -> (SampleIdx,TSample,StepType) -> Sec
genSecInfo (idx1,t1,step1) (idx2,t2,step2) =  Sec { secLen         = t2-t1
                                                    secTimes       = (t1,t2)              
                                                    secStepIdices  = (idx1,idx2)
                                                    secStepTypes   = (step1,step2)}
  
-- calculate exact time of Zero Crossing Point                 
calcZeroTime :: (TSample,PSample) -> (PSample,PSample) -> TSample 
calcZeroTime (t1,p1) (t2,p2) = if tzero < t2 && tzero > t1 then tzero else error ("Zero Point out of Time-Intervall") 
  where m = dp/dt -- interpolation slope 
        dp = p2-p1 -- delta power 
        dt = t2-t1 -- delta time -- t1 comes in time before t2
        tzero = t1+p1/m -- time of zero crossing 
                 
-- detect Sign-Change per Signal / delivers indice of event and type of event
getSignalSteps :: Signal PSample -> [(SampleIdx,Int)] 
getSignalSteps sig = zip (idxList, map (UB.!) crossSig)
  where sig2 = UV.tail sig -- vector shifted by one
        sig1 = UV.init sig -- shortened vector
        crossSig = UV.zipwith f sig2 sig1
        idxList = UV.toList (UV.findIndices (/=NoStep) crossSig) -- find all events index of pint before change is delivered        
        -- with f:
        f s2 s1 | sign s1==ZSign && sign s2 /= ZSign = LeavesZeroStep -- signal leaves zero
        f s2 s1 | sign s2==ZSign && sign s1 /= ZSign = BecomesZeroStep -- signal becomes zero
        f s2 s1 | sign s2==PSign && sign s1 /= NSign = ZeroCrossingStep  -- signal is crossing zero
        f s2 s1 | sign s1==PSign && sign s2 /= NSign = ZeroCrossingStep  -- signal is crossing zero
        f s2 s1 | otherwise = NoStep  -- nostep
 
-- generate Sequence Information   
genSequ :: Record -> Sequ
genSequ rec@(time,sigs) = zipwith genSecInfo (init stepList3) (tail stepList3)  
  where stepList1 = sort (concat (GV.map getSignalSteps sigs)) -- get Steps from all Signals
        stepList2 = [(0,InitStep)] ++ idxListAll ++ [UV.length sig,EndStep] -- add steps for 1st and last sample in first and last section
        stepList3 = zip3 (fst (unzip stepList2), stepTimes, snd(unzip stepList2))
        stepTimes = map (GV.map f) stepListFinal -- calculate Step time
          where f sig (stepIdx,InitStep) = time UV.! stepIdx
                f sig (stepIdx,EndStep) = time UV.! (stepIdx+1)
                f sig (stepIdx,LeavesZero) = time UV.! stepIdx
                f sig (stepIdx,BecomesZero) = time UV.! (stepIdx+1) 
                f sig (stepIdx,ZeroCrossing) = calcZeroTime (time UV.! stepIdx, sig UV.! stepIdx), (time UV.! (stepIdx+1), sig UV.! (stepIdx+1)) 


genSecRec :: Record -> Sec -> Record
genSecRec rec@(time,sigs) sec  = (sliceTime secTime, GV.map sliceSignal sigs)  
    
genRecSequ :: Sequ -> Record -> SequData Record 
genRecSequ sequ rec = GV.map (genSecRec rec) sequ  
               
-- extract slice of one signal
sliceSignal :: UV.Vector PSample -> Sec -> UV.Vector PSample
sliceSignal sig sec = (sigHead step1) UV.++ sigTrunk UV.++ (sigTail step2)
        where
          trunk = UV.split (idx1+1) (idx2-(idx1+1)) -- get middle part which is always same 
          step1 = fst(secStepTypes sec)
          step2 =  snd (secStepTypes sec)
          
          sigHead LeavesZeroStep =  [sig UV.! idx1]
          sigHead InitStep =  [sig UV.! idx1]
          sigHead BecomesZeroStep =  []
          sigHead EndStep = error ("Error in sliceSignal - endStep shouldn't occur here") 
          sigHead ZeroCrossing =  [0]
          
          sigTail LeavesZeroStep =  [0]
          sigTail InitStep = error ("Error in sliceSignal - initStep shouldn't occur here")
          sigTail BecomesZeroStep =  [sig UV.! idx2+1]
          sigTail EndStep =  [sig UV.! idx2+1]
          sigTail ZeroCrossing =  [0]
          
-- extract slice of one signal
sliceTime :: UV.Vector TSample -> Sec -> UV.Vector TSample
sliceTime sig sec = (sigHead step1) UV.++ sigTrunk UV.++ (sigTail step2)
        where
          trunk = UV.split (idx1+1) (idx2-(idx1+1)) -- get middle part which is always same 
          step1 = fst(secStepTypes sec)
          step2 =  snd (secStepTypes sec)
          t1 = fst(secTimes sec)
          t2 = snd(secTimes sec)
          
          sigHead LeavesZeroStep =  [sig UV.! idx1]
          sigHead InitStep =  [sig UV.! idx1]
          sigHead BecomesZeroStep =  []
          sigHead EndStep = error ("Error in sliceSignal - endStep shouldn't occur here") 
          sigHead ZeroCrossing =  [t1]
          
          sigTail LeavesZeroStep =  [0]
          sigTail InitStep = error ("Error in sliceSignal - initStep shouldn't occur here")
          sigTail BecomesZeroStep =  [sig UV.! idx2+1]
          sigTail EndStep =  [sig UV.! idx2+1]
          sigTail ZeroCrossing =  [t2]

          
          
-- TODO -- include length checking !!
checkLength :: Section a -> Either SectionError ()
checkLength sec = when (secLen sec < 2) $ Left ShortSection


        


