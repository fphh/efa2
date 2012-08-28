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


-- makeSequence :: PowerRecord -> Topology -> ([Envs (Scal (Typ UT UT UT) Val)], Topology)
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
  where rSig = record2RSig pRec
        pRecs = map (rsig2SecRecord pRec) (seqRSig ++ [lastRSec])
        ((lastSec,sequ),(lastRSec,seqRSig)) = recyc (rtail rSig) (rhead rSig) (((0,0),[]),(rsingleton $ rhead rSig,[])) 
        
        recyc :: RSig -> RSamp1 -> ((Sec,Sequ), (RSig, [RSig])) -> ((Sec,Sequ), (RSig, [RSig]))  
        recyc rSig x1 (((lastIdx,idx),sequ),(secRSig, sequRSig)) | rSig /= mempty = recyc (rtail rSig) (rhead rSig) (g $ stepDetect x1 x2, f $ stepDetect x1 x2)
          where
            x2 = rhead rSig
            xs1 = rsingleton x1
            xs2 = rsingleton x2

            
            f :: EventType -> (RSig, [RSig])
            f LeftEvent = (xs1.++xs2, sequRSig ++ [secRSig])           -- add actual Interval to next section
            f RightEvent = (xs2, sequRSig ++ [secRSig .++ xs2])     --add actual Interval to last section
            f MixedEvent = (xs2, sequRSig ++ [secRSig] ++ [xs1 .++ xs2]) -- make additional Mini--Section 
            f NoEvent = (secRSig .++ xs2, sequRSig)                  -- continue incrementing
            
            g :: EventType -> (Sec, Sequ)            
            g LeftEvent = ((idx, idx+1), sequ ++ [(lastIdx, idx)])
            g RightEvent = ((idx+1, idx+1), sequ ++ [(lastIdx, idx+1)])
            g MixedEvent = ((idx+1, idx+1), sequ ++ [(lastIdx, idx)] ++ [(idx, idx+1)])
            g NoEvent = ((lastIdx, idx+1), sequ)

            inc (lastIdx, idx) = (lastIdx, idx+1) 
            restart (lastIdx, idx) = (idx, idx+1)
        recyc _ _ acc | otherwise = acc                                                            


-- | Function to remove Nil-Sections which have same start and stop Index            
removeNilSections :: (Sequ,SequPwrRecord) ->   (Sequ, SequPwrRecord)           
removeNilSections (sequ, SequData pRecs) = (fsequ, SequData fRecs)
  where (fsequ, fRecs) = unzip $ filter f $ zip sequ pRecs
        f ((lastIdx, idx), _) | lastIdx == idx = False 
        f _ = True


-- | Function to detect and classify a step over several signals
stepDetect :: RSamp1 -> RSamp1 -> EventType 
stepDetect  (t1,ps1) (t2,ps2) = f stepList
  where stepList = stzipWith stepX ps1 ps2 :: Samp1L (Typ A STy Tt) StepType

        f ::  Samp1L (Typ A STy Tt) StepType -> EventType
        f stepList | sall (==NoStep) stepList = NoEvent
        f stepList | sany (==ZeroCrossingStep) stepList = error $ "Error in stepDetect - Zero Crossing - t1: " ++ show t1 ++ " t2 :" ++ (show t2)
        f stepList | sany (==LeavesZeroStep) stepList && (not $ sany (==BecomesZeroStep) stepList) = LeftEvent
        f stepList | (not $ sany (==LeavesZeroStep) stepList) && sany (==BecomesZeroStep) stepList = RightEvent
        f stepList | sany (==LeavesZeroStep) stepList && sany (==BecomesZeroStep) stepList = MixedEvent


-- | Function to detect and classify a step over one signal
stepX :: PSamp -> PSamp -> Samp (Typ A STy Tt) StepType
stepX p1 p2 | ssign p1== toSample ZSign && ssign p2 /= toSample ZSign = toSample LeavesZeroStep -- signal leaves zero
stepX p1 p2 | ssign p1/=toSample ZSign && ssign p2 == toSample ZSign = toSample BecomesZeroStep -- signal becomes zero
stepX p1 p2 | ssign p1==toSample PSign && ssign p2 == toSample NSign = toSample ZeroCrossingStep
stepX p1 p2 | ssign p1==toSample NSign && ssign p2 == toSample PSign = toSample ZeroCrossingStep
stepX p1 p2 | otherwise = toSample NoStep  -- nostep


addZeroCrossings :: PowerRecord -> PowerRecord
addZeroCrossings r = rsig2Record rSigNew r
  where rSig = record2RSig r 
        rSigNew = (f (rtail rSig) (rhead rSig) mempty) .++ (rsingleton $ rlast rSig)
        (timeNew,mSigNew) = rSigNew

        f :: RSig -> RSamp1 -> RSig -> RSig
        f rSig rold rSigNew | rSig /= mempty = f (rtail rSig) rnew (rSigNew .++ (getZeroCrossings rold rnew))  
          where rnew = rhead rSig  
        f rSig  _ rSigNew |  otherwise = rSigNew                                         

-----------------------------------------------------------------------------------
-- | Function for calculating zero Crossings 
                                      
getZeroCrossings :: RSamp1 -> RSamp1 -> RSig         
getZeroCrossings rs1@(t1,ps1) rs2@(t2,ps2) = ((ssingleton t1) .++ zeroCrossingTimes,(ssingleton ps1) .++ zeroPowers)
          where 
             (zeroCrossings, zeroCrossingTimes) = calcZeroTimes rs1 rs2
             zeroPowers = calcZeroPowers rs1 rs2 zeroCrossingTimes zeroCrossings


calcZeroPowers :: RSamp1 -> RSamp1 -> TSigL -> TZeroSamp1L -> PSamp2LL  
calcZeroPowers (t1,(TC (Data (D1 ps1)))) (t2,(TC (Data (D1 ps2)))) zeroCrossingTimes (TC (Data (D1 tz))) = stranspose $ fromSigList sigList 
               where g p1 p2 tz = f (toSample p1) (toSample p2) (toSample tz) 
                     sigList = L.zipWith3 g ps1 ps2 tz :: [PSigL]
                    
                     f :: PSamp -> PSamp -> TZeroSamp -> PSigL
                     f p1 p2 zeroCrossing = interpPowers (t1,p1) (t2,p2) zeroCrossingTimes zeroCrossing

calcZeroTimes :: RSamp1 -> RSamp1 -> (TZeroSamp1L,TSigL)
calcZeroTimes (t1,ps1) (t2,ps2)  = (zeroCrossings, zeroCrossingTimes)                                            
              where
                -- | create ascending list containing all zero crossing times
                 zeroCrossingTimes = ssort $ filterTZero zeroCrossings :: TSigL
                 zeroCrossings = stzipWith h2 ps1 ps2 :: TZeroSamp1L 
            
                 -- | Zero crossing time per signal, if zero crossing happens otherwise empty
                 h2 :: PSamp -> PSamp -> TZeroSamp
                 h2 p1 p2 | ssign p1 == toSample PSign && ssign p2 == toSample NSign = calcZeroTime (t1,p1) (t2,p2)
                 h2 p1 p2 | ssign p1 == toSample NSign && ssign p2 == toSample PSign = calcZeroTime (t1,p1) (t2,p2)
                 h2 _  _ = toSample NoCrossing
                                              
-----------------------------------------------------------------------------------
-- | Interpolation Functions for one Signal

-- | calculate time of Zero Crossing Point                  
calcZeroTime :: (TSamp,PSamp) -> (TSamp,PSamp) -> TZeroSamp
calcZeroTime (t1,p1) (t2,p2) = s
  where m = (p2.-p1)./(t2.-t1) -- interpolation slope
        s =
           case compare t2 t1 of
              GT -> makeTZero $ dt.+t1 where dt = changeType $ ((sneg p1)./m) :: DTSamp -- time of zero crossing
              EQ -> makeTZero t1
              LT -> error ("Error in calcZeroTime- Discontinous time vector t1: " ++ show t1 ++ " t2: " ++ show t2)


-- | interpolate Powers at Zero Crossing times
interpPowers :: (TSamp,PSamp) -> (TSamp,PSamp) -> TSigL -> TZeroSamp -> PSigL
interpPowers (t1,p1) (t2,p2) tzeroList tzero = stmap f tzeroList
  where f :: TSamp -> PSamp
        f tz | (makeTZero tz)==tzero = (toSample 0) -- avoid numeric error and make zero crossing power zero
             | otherwise =
                 case compare t2 t1 of
                    GT -> p1.+m.*(tz.-t1) -- interpolate non zero powers
                    EQ -> sampleAverage p1 p2
                    LT -> error ("Error in interpPowers - Discontinous time vector - t1: " ++ show t1 ++ " t2: " ++ show t2)
        m = (p2.-p1)./(t2.-t1) -- interpolation slope

-----------------------------------------------------------------------------------
-- | Helper Functions

makeTZero :: TSamp -> TZeroSamp
makeTZero (TC (Data (D0 x))) = TC $ Data $ D0 $ ZeroCrossing x 

filterTZero :: TZeroSamp1L -> TSigL
filterTZero x = stranspose $ smap (\ (ZeroCrossing x) -> x) $ sfilter (/=NoCrossing) x  



-----------------------------------------------------------------------------------
-- | Helper Functions

-- | Generate rSig from Power Record
updateMap :: Ord k => M.Map k a -> [a] -> M.Map k a
updateMap pmap xs = if check then M.fromList $ zip keys xs else error "Error in updateMap - map and List length don't match"   
  where keys = map fst $ M.toList pmap
        check = length keys == length xs 

record2RSig :: PowerRecord -> RSig    
record2RSig (PowerRecord t pMap) = (t, stranspose $ fromSigList $ M.elems pMap) 

rsig2Record :: RSig -> PowerRecord -> PowerRecord                  
rsig2Record (t, ps) (PowerRecord _ pMap) = PowerRecord t  (M.fromList (zip  keys  (toSigList  $ stranspose ps))) 
   where keys = map fst (M.toList pMap)

rsig2SecRecord :: PowerRecord -> RSig -> SecPowerRecord                  
rsig2SecRecord  (PowerRecord _ pMap) (t, ps) = SecPowerRecord (sconvert t)  (M.fromList (zip  keys  (map sconvert $ toSigList  $ stranspose ps))) 
   where keys = map fst (M.toList pMap)
