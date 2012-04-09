
module EFA2.Signal.Sequence where

import qualified Data.List as L
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
type Sec = (SignalIdx,SignalIdx)
--data Sec = Sec (SignalIdx,StepType,TSample) (SignalIdx,StepType,TSample) deriving (Show)

-- data structure to contain output of section analysis (cutting history in slices)
type SectionLength = DTSample

-- Sequence Vector to Store Section Data  
type SequData a = [a] -- deriving Show

data StepType = LeavesZeroStep | BecomesZeroStep | ZeroCrossingStep | NoStep deriving (Eq, Show,Ord)

data EventType = LeftEvent | RightEvent | MixedEvent | NoEvent

type PSampleRow = [PSample]
type XSample = (TSample,PSampleRow)
type XList = [XSample]
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
-- | Generate Power Map

-- genPowerMap :: Time -> M.Map Power -> PowerMap Power    
-- genPowerMap time map = M.map f map
--   where
    
-- | Function to regenerate pMap from prows
unpackXList :: PowerMap Power -> XList -> (Time,PowerMap Power)   
unpackXList pmap xList  = (time, M.fromList $ zipWith h2 (M.toList pmap) sigs)
  where (time,rows) = unzip xList
        sigs = transpose rows
        h2 (key,_) sig = (key,sig) -- format the results

genXList :: Time -> PowerMap Power -> XList
genXList time pmap =  zip time (transpose $ M.elems pmap)

-- | Function to add Zero Crossing Points into the signals and the time 
addZeroCrossingPoints ::  Time -> PowerMap Power -> (Time, PowerMap Power)    
addZeroCrossingPoints time pmap = unpackXList pmap xListNew 
  where xList = genXList time pmap
        xListNew = (concat $ dmap f xList) ++ [last xList]
    
        f :: (TSample,PSampleRow) ->  (TSample,PSampleRow) -> [(TSample,PSampleRow)]
        f (t1, row1) (t2, row2) = zip ([t1]++zeroCrossingTimes) ([row1]++zipWith g (zip row1 row2) zeroCrossings)  
          where 
            -- create list of all zero crossing times
            zeroCrossingTimes = L.sort $ concat $ zeroCrossings :: [TSample]
            zeroCrossings = zipWith h2 row1 row2 :: [[TSample]]
            h2 :: PSample -> PSample -> Time 
            h2 p1 p2 | sign p1 == PSign && sign p2 == NSign = [calcZeroTime (t1,p1) (t2,p2)]
            h2 p1 p2 | sign p1 == NSign && sign p2 == PSign = [calcZeroTime (t1,p1) (t2,p2)]
            h2 _  _ = []

            g :: (PSample,PSample) -> [TSample] -> [PSample]
            g (p1,p2) zeroCrossing = interpPowers (t1,p1) (t2,p2) zeroCrossingTimes zeroCrossing

    
-- | calculate time of Zero Crossing Point                 
calcZeroTime :: (TSample,PSample) -> (TSample,PSample) -> TSample 
calcZeroTime (t1,p1) (t2,p2) = -p1/m+t1 -- time of zero crossing 
  where m = (p2-p1)/(t2-t1) -- interpolation slope 
                  
-- | interpolate Powers at Zero Crossing times 
interpPowers :: (TSample,PSample) -> (TSample,PSample) -> [TSample] -> [TSample] -> [PSample]        
interpPowers (t1,p1) (t2,p2) tzeroList tzero = map f tzeroList
  where f tz | [tz]==tzero = 0 -- avoid numeric error and make zero crossing power zero
        f tz | otherwise = p1+m*(tz-t1) -- interpolate non zero powers
        m = (p2-p1)/(t2-t1) -- interpolation slope 

-- -----------------------------------------------------------------------------------
-- -- | Generate Time Sequence

genSequ ::  Time -> PowerMap Power -> (Sequ,SequData Time, SequData (PowerMap Power))
genSequ time pmap = (sequ++[lastSec],seqDatTime,seqPmaps)
  where xList = genXList time pmap 
        (seqDatTime,seqPmaps) = unzip $ map (unpackXList pmap) (seqXList++[lastXSec])
        ((lastSec,sequ),(lastXSec,seqXList)) = recyc (tail xList) (((0,0),[]),([head xList],[])) 
                                               
        
        recyc :: XList -> ((Sec,Sequ),(XList,[XList])) -> ((Sec,Sequ),(XList,[XList]))  
        recyc [] acc = acc                                                            
        recyc (x2:xlist) (((lastIdx,idx),sequ),(secXList,sequXList)) =  recyc xlist (g $ stepDetect x1 x2, f $ stepDetect x1 x2)
          where
            x1 = last secXList
            f :: EventType -> (XList,[XList])
            f LeftEvent = ([x1,x2],sequXList++[secXList]) -- add actual Interval to next section
            f RightEvent = ([x2],sequXList++[secXList++[x2]]) --add actual Interval to last section
            f MixedEvent = ([x2],sequXList++[secXList]++[[x1,x2]]) -- make additional Mini--Section 
            f NoEvent = (secXList++[x2],sequXList) -- continue incrementing
            g :: EventType -> (Sec,Sequ)            
            g LeftEvent = ((idx,idx+1),sequ++[(lastIdx,idx)])
            g RightEvent = ((idx+1,idx+1),sequ++[(lastIdx,idx+1)])
            g MixedEvent = ((idx+1,idx+1),sequ++[(lastIdx,idx)]++[(idx,idx+1)])
            g NoEvent = ((lastIdx,idx+1),sequ)
            inc (lastIdx,idx) = (lastIdx,idx+1) 
            restart (lastIdx,idx) = (idx,idx+1)
            
stepDetect :: XSample -> XSample -> EventType 
stepDetect  (_,row1) (_,row2) = f stepList
  where stepList = zipWith stepX row1 row2
        f stepList | all (==NoStep) stepList = NoEvent
        f stepList | any (==ZeroCrossingStep) stepList = error "Error in stepDetect - Zero Crossing"
        f stepList | any (==LeavesZeroStep) stepList && (not $ any (==BecomesZeroStep) stepList) = LeftEvent
        f stepList | (not $ any (==LeavesZeroStep) stepList) && any (==BecomesZeroStep) stepList = RightEvent
        f stepList | any (==LeavesZeroStep) stepList && any (==BecomesZeroStep) stepList = MixedEvent

stepX :: PSample -> PSample -> StepType
stepX s1 s2 | sign s1==ZSign && sign s2 /= ZSign = LeavesZeroStep -- signal leaves zero
stepX s1 s2 | sign s1/=ZSign && sign s2 == ZSign = BecomesZeroStep -- signal becomes zero
stepX s1 s2 | sign s1==PSign && sign s2 == NSign = ZeroCrossingStep
stepX s1 s2 | sign s1==NSign && sign s2 == PSign = ZeroCrossingStep
stepX s1 s2 | otherwise = NoStep  -- nostep

        
        
         
    
    





{-
-----------------------------------------------------------------------------------
-- | Generate Time Sequence

stepX :: PSample -> PSample -> StepType
stepX s1 s2 | sign s1==ZSign && sign s2 /= ZSign = LeavesZeroStep -- signal leaves zero
stepX s1 s2 | sign s1/=ZSign && sign s2 == ZSign = BecomesZeroStep -- signal becomes zero
stepX s1 s2 | sign s1==PSign && sign s2 == NSign = ZeroCrossingStep  -- signal is crossing zero
stepX s1 s2 | sign s1==NSign && sign s2 == PSign = ZeroCrossingStep  -- signal is crossing zero
stepX s2 s1 | otherwise = NoStep  -- nostep
 
-- | Function to generate a list containing all signal steps with time and index information 
makeSteps :: Time -> Power -> [(SignalIdx,StepType,TSample)]
makeSteps time power | length power == 0 = []
makeSteps time power = [(0,InitStep,head time)] ++ concat (dmap f ss) ++ [(length ss-1,EndStep,last time)]
  where ss  = idxList (zip time power) 
        f (idx1,(t1,p1)) (idx2,(t2,p2)) = f stepTyp -- if stepTyp == NoStep then [] else 
            where stepTyp = stepX p1 p2
                  tzero = calcZeroTime (t1,p1) (t2,p2)
                  f NoStep = []
                  -- calculate zero crossing
                  f ZeroCrossingStep = [(idx1,stepTyp,tzero)]  
                  -- use first sample
                  f InitStep = [(idx1,stepTyp,t1)]
                  f LeavesZeroStep = [(idx1,stepTyp,t1)]
                  -- use second sample
                  f EndStep = [(idx2,stepTyp,t2)]
                  f BecomesZeroStep = [(idx2,stepTyp,t2)]
                    

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
          

-}


     

          
          

                  

