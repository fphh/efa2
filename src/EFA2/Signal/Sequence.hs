{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module EFA2.Signal.Sequence where

import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector as GV

import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith

import EFA2.Topology.Flow
import EFA2.Topology.TopologyData

import EFA2.Signal.SequenceData

import EFA2.Utils.Utils

import Debug.Trace


data StepType = LeavesZeroStep
              | BecomesZeroStep
              | ZeroCrossingStep 
              | NoStep deriving (Eq, Show,Ord)

data EventType = LeftEvent
               | RightEvent
               | MixedEvent
               | NoEvent

type PSampleRow = [PSample]

-- | XSample contains time and values of all power signals for one time step 
type XSample = (TSample, PSampleRow)

-- | Xlist = list of all xSamples
type XSig = Container XSample

-- | From PowerRecord
fromFlowRecord :: SecIdx -> RecIdx -> FlowRecord -> Envs FSignal
fromFlowRecord (SecIdx secIdx) (RecIdx recIdx) fRec@(FlowRecord dTime flowMap) = 
  Envs { powerMap = M.fromList $ map f (M.toList flowMap),
         dpowerMap = M.empty,
         etaMap = M.empty,
         detaMap = M.empty,
         xMap = M.empty,
         varMap = M.empty } --dtime = dTime,
  where f ((PPosIdx idx1 idx2), (flowSig)) = ((PowerIdx secIdx recIdx idx1 idx2), flowSig)    

-- | Generate Sequence Flow 
genSequFlow :: SequPwrRecord -> SequFlowRecord
genSequFlow sqPRec = (map recFullIntegrate) `fmap` sqPRec

makeSequence :: PowerRecord -> Topology -> ([Envs FSignal], Topology)
makeSequence pRec topo = (sqEnvs, sqTopo)
  where pRec0 = addZeroCrossings pRec
        (sequ,sqPRec) = genSequ pRec0          
        sqFRec = genSequFlow sqPRec
        SequData sqEnvs = fmap f sqFRec
        f = map g . zip h
        g (s, rec) = fromFlowRecord s (RecIdx 0) rec
        h = map SecIdx $ listIdx sequ

        sqFStRec = genSequFState sqFRec
        sqFlowTops = genSequFlowTops topo sqFStRec
        sqSecTops = genSectionTopology sqFlowTops
        sqTopo = mkSequenceTopology sqSecTops


-- | Pre-Integrate all Signals in Record  
recFullIntegrate :: PowerRecord -> FlowRecord   
recFullIntegrate pRec@(PowerRecord time pMap) = FlowRecord time fMap 
  where dtime = dmap' (-) time
        fMap = M.map (sigFullInt dtime) pMap  
        
-- | Partial Signal Integration
sigFullInt ::  DTime -> Power -> Flow
sigFullInt dTime power = csingleton (cfoldr (+) 0  $ czipWith (*) dTime $ dmap (\ p1 p2 -> (p1+p2)/2) power)



-- | Pre-Integrate all Signals in Record  
recPartIntegrate :: PowerRecord -> FlowRecord   
recPartIntegrate pRec@(PowerRecord time pMap) = FlowRecord time fMap 
  where dtime = dmap' (-) time
        fMap = M.map (sigPartInt dtime) pMap  
        
-- | Partial Signal Integration        
sigPartInt ::  DTime -> Power -> Flow
sigPartInt dTime power = czipWith (*) dTime $ dmap (\ p1 p2 -> (p1+p2)/2) power 
  

-----------------------------------------------------------------------------------
-- | Function to Generate Time Sequence
genSequ ::  PowerRecord -> (Sequ,SequPwrRecord)
genSequ pRec = removeNilSections (sequ++[lastSec],SequData pRecs)
  where xSig = genXSig pRec
        pRecs = map (repackXSig pRec) (seqXSig ++ [lastXSec])
        ((lastSec,sequ),(lastXSec,seqXSig)) = recyc (ctail xSig) (((0,0),[]),([chead xSig],[])) 
        
        --recyc :: XSig -> ((Sec,Sequ), (XSig, [XSig])) -> ((Sec,Sequ), (XSig, [XSig]))  
        recyc [] acc = acc                                                            
        recyc (x2:xlist) (((lastIdx,idx),sequ),(secXSig, sequXSig)) = recyc xlist (g $ stepDetect x1 x2, f $ stepDetect x1 x2)
          where
            x1 = last secXSig
            --f :: EventType -> (XSig, [XSig])
            f LeftEvent = ([x1,x2], sequXSig ++ [secXSig])           -- add actual Interval to next section
            f RightEvent = ([x2], sequXSig ++ [secXSig ++ [x2]])     --add actual Interval to last section
            f MixedEvent = ([x2], sequXSig ++ [secXSig]++ [[x1,x2]]) -- make additional Mini--Section 
            f NoEvent = (secXSig ++ [x2], sequXSig)                  -- continue incrementing
            --g :: EventType -> (Sec, Sequ)            
            g LeftEvent = ((idx, idx+1), sequ ++ [(lastIdx, idx)])
            g RightEvent = ((idx+1, idx+1), sequ ++ [(lastIdx, idx+1)])
            g MixedEvent = ((idx+1, idx+1), sequ ++ [(lastIdx, idx)] ++ [(idx, idx+1)])
            g NoEvent = ((lastIdx, idx+1), sequ)
            inc (lastIdx, idx) = (lastIdx, idx+1) 
            restart (lastIdx, idx) = (idx, idx+1)
--            h lastIdx idx | lastIdx == idx =  recyc xlist (g $ NoEvent, f $ NoEvent)
--            h lastIdx idx | otherwise = recyc xlist (g $ stepDetect x1 x2, f $ stepDetect x1 x2)
            
-- | Function to remove Nil-Sections which have same start and stop Index            
removeNilSections :: (Sequ,SequPwrRecord) ->   (Sequ, SequPwrRecord)           
removeNilSections (sequ, SequData pRecs) = (fsequ, SequData fRecs)
  where (fsequ, fRecs) = unzip $ filter f $ zip sequ pRecs
        f ((lastIdx, idx), _) | lastIdx == idx = False 
        f _ = True
            
-- | Function to detect and classify a step over several signals
stepDetect :: XSample -> XSample -> EventType 
stepDetect  (t1,row1) (t2,row2) = f stepList
  where stepList = zipWith stepX row1 row2
        f stepList | all (==NoStep) stepList = NoEvent
        f stepList | any (==ZeroCrossingStep) stepList = error $ "Error in stepDetect - Zero Crossing - t1: " ++ show t1 ++ " t2 :" ++ (show t2)
        f stepList | any (==LeavesZeroStep) stepList && (not $ any (==BecomesZeroStep) stepList) = LeftEvent
        f stepList | (not $ any (==LeavesZeroStep) stepList) && any (==BecomesZeroStep) stepList = RightEvent
        f stepList | any (==LeavesZeroStep) stepList && any (==BecomesZeroStep) stepList = MixedEvent
        
-- | Function to detect and classify a step over one signal
stepX :: PSample -> PSample -> StepType
stepX s1 s2 | sign s1==ZSign && sign s2 /= ZSign = LeavesZeroStep -- signal leaves zero
stepX s1 s2 | sign s1/=ZSign && sign s2 == ZSign = BecomesZeroStep -- signal becomes zero
stepX s1 s2 | sign s1==PSign && sign s2 == NSign = ZeroCrossingStep
stepX s1 s2 | sign s1==NSign && sign s2 == PSign = ZeroCrossingStep
stepX s1 s2 | otherwise = NoStep  -- nostep

-----------------------------------------------------------------------------------
-- | Function to add Zero Crossing Points into the signals and the time 
addZeroCrossings ::  PowerRecord -> PowerRecord    
addZeroCrossings pRec = repackXSig pRec xSigNew 
  where xSig = genXSig pRec
        xSigNew = (concat $ dmap f xSig) ++ [last xSig]
    
        --f :: (TSample,PSampleRow) ->  (TSample,PSampleRow) -> [(TSample,PSampleRow)]
        f (t1, row1) (t2, row2) = zip (csingleton t1 `cappend` zeroCrossingTimes)
                                      (csingleton row1 `cappend` (ctranspose $ czipWith g (czip row1 row2) zeroCrossings))
          where 
            -- create list of all zero crossing times
            zeroCrossingTimes = L.sort $ concat $ zeroCrossings -- :: [TSample]
            zeroCrossings = zipWith h2 row1 row2 -- :: [[TSample]]
            --h2 :: PSample -> PSample -> Time 
            h2 p1 p2 | sign p1 == PSign && sign p2 == NSign = [calcZeroTime (t1,p1) (t2,p2)]
            h2 p1 p2 | sign p1 == NSign && sign p2 == PSign = [calcZeroTime (t1,p1) (t2,p2)]
            h2 _  _ = []

            --g :: (PSample,PSample) -> [TSample] -> [PSample]
            g (p1, p2) zeroCrossing = mytrace 0 "interp" "" $ interpPowers (t1,p1) (t2,p2) zeroCrossingTimes zeroCrossing

-----------------------------------------------------------------------------------
-- | Interpolation Functions

-- | calculate time of Zero Crossing Point                 
calcZeroTime :: (TSample,PSample) -> (TSample,PSample) -> TSample 
calcZeroTime (t1,p1) (t2,p2) = f t1 t2 
  where m = (p2-p1)/(t2-t1) -- interpolation slope 
        f t1 t2 | t2 > t1 = -p1/m+t1 -- time of zero crossing 
        f t1 t2 | t2 == t1 = t1
        d t1 t2 | t2 < t1 = error ("Error in calcZeroTime- Discontinous time vector - t1: " ++ show t1 ++ " t2: " ++ show t2)
                  
-- | interpolate Powers at Zero Crossing times 
interpPowers :: (TSample,PSample) -> (TSample,PSample) -> [TSample] -> [TSample] -> [PSample]        
interpPowers (t1,p1) (t2,p2) tzeroList tzero = map f tzeroList
  where f tz | [tz]==tzero = 0 -- avoid numeric error and make zero crossing power zero
        f tz | otherwise = g t1 t2 
          where 
            g t1 t2 | t2 > t1 = p1+m*(tz-t1) -- interpolate non zero powers
            g t1 t2 | t2 == t1 = (p1+p2)/2 --
            g t1 t2 | t2 < t1 =  error ("Error in interpPowers - Discontinous time vector - t1: " ++ show t1 ++ " t2: " ++ show t2)
        m = (p2-p1)/(t2-t1) -- interpolation slope 

-----------------------------------------------------------------------------------
-- | Helper Functions

-- | Generate X-List from Power Record
genXSig :: PowerRecord -> XSig
genXSig (PowerRecord time pmap) = czip time (ctranspose $ M.elems pmap)


-- | Function to regenerate pMap from pRows
repackXSig :: PowerRecord -> XSig -> PowerRecord
repackXSig (PowerRecord _ pmap) xSig = PowerRecord time (M.fromList $ zipWith h2 (M.toList pmap) sigs)
  where (time,rows) = unzip xSig
        sigs = ctranspose rows
        h2 (key,_) sig = (key,sig) -- format the results

-- -- | check Record Data -- TODO -- include check on time length == sign length                                                               
-- recordCheck :: Record -> Bool
-- recordCheck (Record time sigMap) = smplCheck && equlengthCheck && lengthCheck
--   where 
--     list = [time] ++ M.elems sigMap -- all signals and time in one list
--     smplCheck = all (sampleCheck) list
--     equlengthCheck = equalLengths ([time] ++ list)  -- equal length on all signals
--     lengthCheck = all (1 < ) $ map length list -- at least two samples per time Signal
         
    
    

                  

