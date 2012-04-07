
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
data Sec = Sec (SignalIdx,StepType,TSample) (SignalIdx,StepType,TSample) deriving (Show)

-- data structure to contain output of section analysis (cutting history in slices)
type SectionLength = DTSample

-- Sequence Vector to Store Section Data  
type SequData a = [a] -- deriving Show

data StepType = InitStep | EndStep | LeavesZeroStep | BecomesZeroStep | ZeroCrossingStep | NoStep deriving (Eq, Show,Ord)

-----------------------------------------------------------------------------------
-- Generate & Check Signal Record

-- | generate Record from data components
genRecord :: Time  -> [(SigId, Power)] -> Record
genRecord time sigIDList = if recordCheck rec == True then rec else error ("Incorrect Data in Record-- either unequal length or NaN's")
  where rec = Record time (M.fromList sigIDList)
                                              

-- | check Record Data -- TODO -- include check on time length == sign length                                                               
recordCheck :: Record -> Bool
recordCheck (Record time sigMap) = smplCheck && equlengthCheck && lengthCheck
  where 
    list = [time] ++ M.elems sigMap -- all signals and time in one list
    smplCheck = all (sampleCheck) list
    equlengthCheck = equalLengths ([time] ++ list)  -- equal length on all signals
    lengthCheck = all (1 < ) $ map length list -- at least two samples per time Signal
 
-----------------------------------------------------------------------------------
-- | Generate Time Sequence
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
genSequ :: Time -> PowerMap Power  -> Sequ
genSequ time pmap = dmap Sec (S.toAscList s)
  where offs = map (makeSteps time) (M.elems pmap)
        s = S.unions $ map S.fromList offs -- convert steplist to set to use unions functionality 

            
-- | function to generate Sequence Time Vectors
genSequTime :: Time -> Sequ -> SequData Time
genSequTime time sequ = map f sequ 
  where
    f (Sec (idx1,step1,t1) (idx2,step2,t2) )  = tHead step1 ++ tTrunk ++ tTail step2
        where
          tv = UV.fromList time
          tTrunk = UV.toList $ UV.slice (idx1+1) (idx2-idx1) tv  -- get middle part which is always same 
          tHead BecomesZeroStep =  [] -- BecomesZeroStep
          tHead EndStep = error ("Error in sliceSignal - endStep shouldn't occur here")
          tHead _ =  [t1]
          
          tTail LeavesZeroStep =  [] -- LeavesZeroStep
          tTail InitStep = error ("Error in sliceSignal - initStep shouldn't occur here") 
          tTail _ =  [t2]
  

genSequPowerMaps :: PowerMap Power -> Sequ -> SequData (PowerMap Power)
genSequPowerMaps pmap sequ = map f sequ
  where
     f (Sec (idx1,step1,t1) (idx2,step2,t2)) = M.map g pmap
       where
          g psig =  pHead step1 ++ pTrunk ++ pTail step2
            where
              pv = UV.fromList psig
              pTrunk = UV.toList $ UV.slice (idx1+1) (idx2-idx1) pv  -- get middle part which is always same 
              pHead BecomesZeroStep =  [] -- BecomesZeroStep
              pHead EndStep = error ("Error in sliceSignal - endStep shouldn't occur here")
              pHead ZeroCrossingStep = [0]
              pHead _ =  [pv UV.! idx1] 
          
              pTail LeavesZeroStep =  [] -- LeavesZeroStep
              pTail InitStep = error ("Error in sliceSignal - initStep shouldn't occur here") 
              pTail ZeroCrossingStep = [0]
              pTail _ =  [pv UV.! idx2]
          




     

          
          

                  

