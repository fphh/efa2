{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, ScopedTypeVariables,GADTs, FlexibleContexts #-}

module EFA2.Signal.Sequence where

import qualified Data.List as L
import qualified Data.Map as M
import Control.Monad

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector as GV

import EFA2.Interpreter.Env
-- import EFA2.Interpreter.Arith

import EFA2.Topology.Flow
import EFA2.Topology.TopologyData

import EFA2.Signal.SequenceData
import EFA2.Signal.Base
import EFA2.Signal.Signal
import EFA2.Signal.Typ
import EFA2.Signal.Data
import EFA2.Signal.Vector

import EFA2.Utils.Utils


import Debug.Trace
import Data.Monoid

data StepType = LeavesZeroStep
              | BecomesZeroStep
              | ZeroCrossingStep 
              | NoStep deriving (Eq, Show,Ord)

data EventType = LeftEvent
               | RightEvent
               | MixedEvent
               | NoEvent

--type PSample = Val
--type PSampleRow = TC Sample (Typ A P Tt) (UVec Val)
--type TSample = TC Sample (Typ D T Tt) (DVal Val)

-- | RSample contains time and values of all power signals for one time step 
--type RSample = (TSample, PSampleRow)

-- | Xlist = list of all xSamples
--type RSig =  [RSample] 

{-
-- | From PowerRecord
--fromFlowRecord :: SecIdx -> RecIdx -> FlowRecord -> Envs FSig -- [Val]

--fromFlowRecord :: SecIdx -> RecIdx -> FlRecord a b -> Envs a --UTFSig
fromFlowRecord (SecIdx secIdx) (RecIdx recIdx) fRec@(FlRecord dTime flowMap) =
  emptyEnv { energyMap = M.map untype $ M.mapKeys f flowMap, dtimeMap = M.fromList [(DTimeIdx secIdx recIdx, untype dTime)] }
  where f (PPosIdx idx1 idx2) = EnergyIdx secIdx recIdx idx1 idx2

  --where f ((PPosIdx idx1 idx2), (flowSig)) = ((PowerIdx secIdx recIdx idx1 idx2), [fromScalar $ sigSum flowSig])    



-- | Pre-Integrate all Signals in Record  
recFullIntegrate :: SecPowerRecord -> FlowRecord
recFullIntegrate pRec@(SecPowerRecord time pMap) = FlRecord (sfromList [fromScalar $ sigSum $ deltaSig time]) fMap
  where fMap = M.map (sigFullInt time) pMap  

-- | Pre-Integrate all Signals in Record  
recPartIntegrate :: SecPowerRecord -> FlowRecord   
recPartIntegrate pRec@(SecPowerRecord time pMap) = FlRecord (deltaSig time) fMap 
  where fMap = M.map (sigPartInt time) pMap  

-- | Generate Sequence Flow 
genSequFlow :: SequPwrRecord -> SequFlowRecord FlowRecord
genSequFlow sqPRec = (map recFullIntegrate) `fmap` sqPRec

-- TODO: Umschalten zwischen recFullIntegrate und recPartIntegrate.
--genSequFlow :: SequPwrRecord -> SequFlowRecord FlowRecord
--genSequFlow sqPRec = (map recFullIntegrate) `fmap` sqPRec

--makeSequence :: PowerRecord -> Topology -> ([Envs (Scal (Typ UT UT UT) Val)], Topology)
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

      
-----------------------------------------------------------------------------------
-- | Function to Generate Time Sequence
genSequ ::  PowerRecord -> (Sequ, SequPwrRecord)
genSequ pRec = removeNilSections (sequ++[lastSec],SequData pRecs)
  where xSig = genRSig pRec
        pRecs = map (repackRSig2 pRec) (seqRSig ++ [lastXSec])
        ((lastSec,sequ),(lastXSec,seqRSig)) = recyc (tail xSig) (((0,0),[]),([head xSig],[])) 
        
        --recyc :: RSig -> ((Sec,Sequ), (RSig, [RSig])) -> ((Sec,Sequ), (RSig, [RSig]))  
        recyc [] acc = acc                                                            
        recyc (x2:xlist) (((lastIdx,idx),sequ),(secRSig, sequRSig)) = recyc xlist (g $ stepDetect x1 x2, f $ stepDetect x1 x2)
          where
            x1 = last secRSig
            --f :: EventType -> (RSig, [RSig])
            f LeftEvent = ([x1,x2], sequRSig ++ [secRSig])           -- add actual Interval to next section
            f RightEvent = ([x2], sequRSig ++ [secRSig ++ [x2]])     --add actual Interval to last section
            f MixedEvent = ([x2], sequRSig ++ [secRSig]++ [[x1,x2]]) -- make additional Mini--Section 
            f NoEvent = (secRSig ++ [x2], sequRSig)                  -- continue incrementing
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
-- stepDetect :: RSample -> RSample -> EventType 
stepDetect  (t1,row1) (t2,row2) = f stepList
  where stepList = szipWith stepX (sunbox row1) (sunbox row2)
        f stepList | sall (==NoStep) stepList = NoEvent
        f (TC(Data(D1(stepList)))) | sany (==ZeroCrossingStep) stepList = error $ "Error in stepDetect - Zero Crossing - t1: " ++ show t1 ++ " t2 :" ++ (show t2)
        f (TC(Data(D1(stepList)))) | sany (==LeavesZeroStep) stepList && (not $ sany (==BecomesZeroStep) stepList) = LeftEvent
        f (TC(Data(D1(stepList)))) | (not $ sany (==LeavesZeroStep) stepList) && sany (==BecomesZeroStep) stepList = RightEvent
        f (TC(Data(D1(stepList)))) | sany (==LeavesZeroStep) stepList && sany (==BecomesZeroStep) stepList = MixedEvent
        
-- | Function to detect and classify a step over one signal
stepX :: PSample -> PSample -> StepType
stepX s1 s2 | sign s1==ZSign && sign s2 /= ZSign = LeavesZeroStep -- signal leaves zero
stepX s1 s2 | sign s1/=ZSign && sign s2 == ZSign = BecomesZeroStep -- signal becomes zero
stepX s1 s2 | sign s1==PSign && sign s2 == NSign = ZeroCrossingStep
stepX s1 s2 | sign s1==NSign && sign s2 == PSign = ZeroCrossingStep
stepX s1 s2 | otherwise = NoStep  -- nostep

-} 

{-

addZeroCrossings :: PowerRecord -> PowerRecord
addZeroCrossings r = rsig2Record rSigNew r
  where rSig = record2RSig r 
        rSigNew = (recurse rSig (rhead rSig) mempty) .++ (rsingleton $ rlast rSig)
        (timeNew,mSigNew) = rSigNew


recurse :: RSig -> RSample1 -> RSig -> RSig   
recurse mempty _  rList = rList 
recurse rSig rold rList = recurse (rtail rSig) rnew ((getZeroCrossings rold rnew) .++ rList)  
  where rnew = rhead rSig  
                                      
                                      
getZeroCrossings :: RSample1 -> RSample1 -> RSig         
getZeroCrossings (t1, ps1) (t2, ps2) = ((ssingleton t1) .++ zeroCrossingTimes,
                                      ((ssingleton ps1) .++ (szipWith g (szip ps1 ps2) zeroCrossings)))
          where 
            -- | create ascending list containing all zero crossing times
            zeroCrossingTimes = ssort $ sfilter (/=mempty) (zeroCrossings) :: TSigL
            zeroCrossings = szipWith h2 ps1 ps2 :: TSample1L
            
            -- | Zero crossing time per signal, if zero crossing happens otherwise empty
            h2 :: PSample -> PSample -> TSample  
            h2 p1 p2 | sign p1 == PSign && sign p2 == NSign = calcZeroTime (t1,p1) (t2,p2)
            h2 p1 p2 | sign p1 == NSign && sign p2 == PSign = calcZeroTime (t1,p1) (t2,p2)
            h2 _  _ = mempty

            g :: (PSample,PSample) -> TSample1L -> PSample1L
            g (p1, p2) zeroCrossing = interpPowers (t1,p1) (t2,p2) zeroCrossingTimes zeroCrossing
-}

-----------------------------------------------------------------------------------
-- | Interpolation Functions

-- | calculate time of Zero Crossing Point                  
calcZeroTime :: (TSample,PSample) -> (TSample,PSample) -> TSample 
calcZeroTime (t1,p1) (t2,p2) = f t1 t2 
  where m = (p2.-p1)./(t2.-t1) -- interpolation slope 
        f :: TSample -> TSample -> TSample
        f t1 t2 | t2 > t1 = dt.+t1 where dt = changeType $ ((sneg p1)./m) :: DTSample -- time of zero crossing 
        f t1 t2 | t2 == t1 = t1
        f t1 t2 | t2 < t1 = error ("Error in calcZeroTime- Discontinous time vector t1: " ++ show t1 ++ " t2: " ++ show t2)
                  

-- | interpolate Powers at Zero Crossing times 
interpPowers :: (TSample,PSample) -> (TSample,PSample) -> TSample1L -> TSample -> PSample1L        
interpPowers (t1,p1) (t2,p2) tzeroList tzero = stmap f tzeroList
  where f :: TSample -> PSample
        f tz | tz==tzero = (toSample 0) -- avoid numeric error and make zero crossing power zero
        f tz | otherwise = g t1 t2 
          where 
            g :: TSample -> TSample -> PSample
            g t1 t2 | t2 > t1 = p1.+m.*(tz.-t1) -- interpolate non zero powers
            g t1 t2 | t2 == t1 = sampleAverage p1 p2
            g t1 t2 | t2 < t1 =  error ("Error in interpPowers - Discontinous time vector - t1: " ++ show t1 ++ " t2: " ++ show t2)
        m = (p2.-p1)./(t2.-t1) -- interpolation slope 


{-
-----------------------------------------------------------------------------------
-- | Helper Functions

-- | Generate X-List from Power Record
updateMap :: Ord key => M.Map key a -> [a] -> M.Map key a
updateMap pmap xs = if check then M.fromList $ zip keys xs else error "Error in updateMap - map and List length don't match"   
  where keys = map snd $ M.toList pmap
        check = length keys == length xs 


record2RSig :: PowerRecord -> RSig    
record2RSig (PowerRecord t pMap) = (t, stranspose $ fromSigList $ M.elems pMap) 

rsig2Record :: RSig -> PowerRecord -> PowerRecord                  
rsig2Record (t, ps) (PowerRecord _ pMap) = PowerRecord t  (M.fromList (zip  keys  (toSigList  $ stranspose ps))) 
   where keys = map fst (M.toList pMap)

-}