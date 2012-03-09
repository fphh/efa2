
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
data Sequ = Sequ GV.Vector { secLen    ::  SectionLength,
                             secIdxes  ::  (SampleIdx,SampleIdx),
                             secError  ::  Maybe SectionError}

-- data structure to contain output of section analysis (cvutting history in slices)
type SectionLength = DTSample

-- Sequence Vector to Store Section Data  
data SequData a = SequData (GV.Vector a) deriving Show

-------------------------------------------------------------------------------------
-- functions to split the record

data Sign = PSign | ZSign | MSign deriving (Show, Eq)

-- determine Signal Sign  
sign :: (Eq a, Ord a, Num a) => a -> Sign
sign x | x > 0 = PSign
       | x == 0 = ZSign -- TODO add intervalls later on Zero - Detection       
       | x < 0 = MSign
                 
-- calculate Zero Crossing Point                 
calcZeroTime :: (TSample,PSample) -> (PSample,PSample) -> TSample 
calcZeroTime (t1,p1) (t2,p2) = if tzero < t2 && tzero > t1 then tzero else error ("Zero Point out of Time-Intervall") 
  where m = dp/dt -- interpolation slope 
        dp = p2-p1 -- delta power 
        dt = t2-t1 -- delta time
        tzero = t1+p1/m -- time of zero crossing
                 
-- detect Zero-Crossings
getStateChange :: Signal PSample -> [SampleIdx] 
getStateChange sig = idxPairList
  where
    sig2 = UV.tail sig -- shifted vector
    sig1 = UV.init sig -- shortened vector
    crossSig = UV.zipwith f sig2 sig1
    f s2 s1 | (sign s2 /= sign s1) = 1
    f s2 s1 | otherwise = 0
    idxList = UV.toList (UV.findIndices (==1) crossSig)] -- find all Ocurrences
    -- search occured on delta-Index // idex delivered is before the ZCrossing
    idxListEnd = idxList ++ [UV.length sig]  -- find all Indices beeing 1
    idxListStart = [0] ++ (map (+1) idxList)
    idxPairList = zip idxListStart idxListEnd

-- generate Sequence Information   
genSequ :: Record ->  [SampleIdx] -> (Sequ
genSequ rec@(time,sigs) idxList = 
  where
     idxListAll = sort (concat (GV.map getZeroCrossings sigs))
     idxListPair = zip (idxListAll
     
  
-- make zero crossing Interpolation here  
genSequRec :: Sequ -> Record -> SequData Record 
genSequRec sequ rec

-- identify Sequence States  
identifyFlowStates :: Sequ ->   

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
        


