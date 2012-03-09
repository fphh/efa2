
module EFA2.Signal.SplitSignal where

import Control.Monad

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector.Unboxed as UV

import System.Random
import EFA2.Utils.Utils
import EFA2.Signal.SignalData

-------------------------------------------------------------------------------------
-- functions to split the record

data Sign = PSign | ZSign | MSign deriving (Show, Eq)

-- determine Signal Sign        
sign :: (Eq a, Ord a, Num a) => a -> Sign
sign x | x > 0 = PSign
       | x == 0 = ZSign
       | x < 0 = MSign

-- generate Sequence Information   
genSequ :: Record -> Sequ
genSequ rec@(time,sigs) =   
  
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
        
-------------------------------------------------------------------------------------
-- Section and Sequence Data Stucture

-- data structure to contain output of section analysis (cvutting history in slices)
type SectionLength = DTSample

-- Section analysis result
data Sequ = Sequ GV.Vector { secLen    ::  SectionLength,
                             secIdxes  ::  (SampleIdx,SampleIdx),
                             secError  ::  Maybe SectionError}

-- Sequence Vector to Store Section Data  
data SequData a = SequData (GV.Vector a) deriving Show


recordToRecordSequ :: Record -> SequData Record
recordToRecordSequ Record (time,sigs) = SequData res
  where steps = makeStepList sigs
        st = splitList steps time
        ss = (transpose $ map (splitList steps) vecs)
        slens = map (UV.length . head) ss
        secs = L.zipWith4 (\a b c d -> (Section a b Nothing c d)) [0..] slens st (map (zip sids) ss)
        errs = map checkSections secs
        res = map f (zip errs secs)
        f (Left err, sec) = sec { secError = Just err }
        f (_, sec) = sec
