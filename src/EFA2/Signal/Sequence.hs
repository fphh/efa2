
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
data Sequ = Sequ GV.Vector { secLen     ::  SectionLength,
                             secStepIdx ::  SampleIdx,
                             secSepType ::  StepType
                             secError   ::  Maybe SectionError}

-- data structure to contain output of section analysis (cvutting history in slices)
type SectionLength = DTSample

-- Sequence Vector to Store Section Data  
data SequData a = SequData (GV.Vector a) deriving Show

-- Init == first Sample / End == Last Sample
data StepType = InitStep | LeavesZeroStep | BecomesZeroStep | ZeroCrossingStep | EndStep


-------------------------------------------------------------------------------------
-- functions to split the record

data Sign = PSign | ZSign | MSign deriving (Show, Eq)

-- determine Signal Sign  
sign :: (Eq a, Ord a, Num a) => a -> Sign
sign x | x > 0 = PSign
       | x == 0 = ZSign -- TODO add intervalls later on Zero - Detection       
       | x < 0 = MSign
                 
-- calculate exact time of Zero Crossing Point                 
calcZeroTime :: (TSample,PSample) -> (PSample,PSample) -> TSample 
calcZeroTime (t1,p1) (t2,p2) = if tzero < t2 && tzero > t1 then tzero else error ("Zero Point out of Time-Intervall") 
  where m = dp/dt -- interpolation slope 
        dp = p2-p1 -- delta power 
        dt = t2-t1 -- delta time -- t1 comes in time before t2
        tzero = t1+p1/m -- time of zero crossing 
                 
-- detect Sign-Change per Signal / delivers indice of event and type of event
getStateChange :: Signal PSample -> [(SampleIdx,Int)] 
getStateChange sig = (idxList, map (UB.!) crossSig)
  where sig2 = UV.tail sig -- vector shifted by one
        sig1 = UV.init sig -- shortened vector
        crossSig = UV.zipwith f sig2 sig1
        f s2 s1 | sign s1==ZSign && sign s2 /= ZSign = LeavesZeroStep -- signal leaves zero
        f s2 s1 | sign s2==ZSign && sign s1 /= ZSign = BecomesZeroStep -- signal becomes zero
        f s2 s1 | otherwise = ZeroCrossingStep  -- signal is crossing zero
        idxList = UV.toList (UV.findIndices (>0) crossSig) -- find all events index of pint before change is delivered
 
-- generate Sequence Information   
genSequ :: Record -> (Sequ, SequData Record)
genSequ rec@(time,sigs) idxList = (sequ,
  where stepListAll = sort (concat (GV.map getStateChange sigs))
        -- for start and end point we choose leaves 0 and becomes zero to include 1st and last sample in first and last section
        stepListFinal = [(0,InitStep)] ++ idxListAll ++ [UV.length sig,EndStep] 
        stepTimes = map (GV.map f) stepListFinal
        f sig (stepIdx,InitStep) = time UV.! stepIdx
        f sig (stepIdx,EndStep) = time UV.! (stepIdx+1)
        f sig (stepIdx,LeavesZero) = time UV.! stepIdx
        f sig (stepIdx,BecomesZero) = time UV.! (stepIdx+1)
        -- calculate exact zero crossings per signal
        f sig (stepIdx,ZeroCrossing) = calcZeroTime (time UV.! stepIdx, sig UV.! stepIdx), (time UV.! (stepIdx+1), sig UV.! (stepIdx+1)) 
                                                                            
genSequRec :: Sequ -> Record -> SequData Record 
genSequRec sequ rec = map f sequ
  where f sect = UV.split  
        where
          (idx1,idx2) =  secIndices sect


-------------------------------------------------------------------------------------
-- Sequence Error Checking
        
data SectionError = ShortSection
                  | ContainsNaN
                  | ContainsInfinity
                  | ContainsDenormalized
                  | ContainsNegativeZero deriving (Show, Eq)


checkLength :: Section a -> Either SectionError ()
checkLength sec = when (secLen sec < 2) $ Left ShortSection

checkNaN :: (UV.Unbox a, RealFloat a) => Section a -> Either SectionError ()
checkNaN sec = when (f (getSecVecs sec)) $ Left ContainsNaN
  where f es = or (map (UV.or . (UV.map isNaN)) es)

checkSections :: (UV.Unbox a, RealFloat a) => Section a -> Either SectionError ()
checkSections sec = do
  checkLength sec
  checkNaN sec
        


