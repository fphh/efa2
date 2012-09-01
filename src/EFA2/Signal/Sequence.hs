{-# LANGUAGE GeneralizedNewtypeDeriving, TypeOperators, ScopedTypeVariables,GADTs, FlexibleContexts #-}

module EFA2.Signal.Sequence where

import qualified Data.List as L
import qualified Data.Map as M

import EFA2.Interpreter.Env

import EFA2.Topology.Flow
import EFA2.Topology.TopologyData (Topology)

import qualified EFA2.Signal.Signal as S

import EFA2.Signal.SequenceData
import EFA2.Signal.Base
import EFA2.Signal.Signal
          (TC(TC), RSig, TSigL, TZeroSamp1L, TZeroSamp, TSamp, PSamp, PSigL,
           RSamp1, DTSamp, PSamp2LL, Samp, Samp1L, FSignal,
           (.+), (.-), (.*), (./), (.++),
           rhead, rlast, rtail, rsingleton,
           sampleAverage, deltaSig, sigPartInt, sigFullInt,
           changeType, untype, fromScalar, toSample, sigSum, toSigList, fromSigList)
import EFA2.Signal.Typ
import EFA2.Signal.Data (Data(Data), Nil, (:>))

import qualified Data.Vector.Unboxed as UV

import EFA2.Utils.Utils

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
fromFlowRecord ::
   SecIdx ->
   RecIdx ->
   FlRecord
      (TC s1 (Typ delta2 t2 p2) (c1 d1))
      (TC s1 (Typ delta1 t1 p1) (c1 d1)) ->
   Envs (TC s1 (Typ UT UT UT) (c1 d1))
fromFlowRecord (SecIdx secIdx) (RecIdx recIdx) (FlRecord dTime flowMap) =
  emptyEnv { energyMap = M.map untype $ M.mapKeys f flowMap, dtimeMap = M.fromList [(DTimeIdx secIdx recIdx, untype dTime)] }
  where f (PPosIdx idx1 idx2) = EnergyIdx secIdx recIdx idx1 idx2

  --where f ((PPosIdx idx1 idx2), (flowSig)) = ((PowerIdx secIdx recIdx idx1 idx2), [fromScalar $ sigSum flowSig])



-- | Pre-Integrate all Signals in Record
recFullIntegrate :: SecPowerRecord -> FlowRecord
recFullIntegrate (SecPowerRecord time pMap) = FlRecord (S.fromList [fromScalar $ sigSum $ deltaSig time]) fMap
  where fMap = M.map (sigFullInt time) pMap

-- | Pre-Integrate all Signals in Record
recPartIntegrate :: SecPowerRecord -> FlowRecord
recPartIntegrate (SecPowerRecord time pMap) = FlRecord (deltaSig time) fMap
  where fMap = M.map (sigPartInt time) pMap

-- | Generate Sequence Flow
genSequFlow :: SequPwrRecord -> SequFlowRecord FlowRecord
genSequFlow sqPRec = (map recFullIntegrate) `fmap` sqPRec

-- TODO: Umschalten zwischen recFullIntegrate und recPartIntegrate.
--genSequFlow :: SequPwrRecord -> SequFlowRecord FlowRecord
--genSequFlow sqPRec = (map recFullIntegrate) `fmap` sqPRec

-- makeSequence :: PowerRecord -> Topology -> ([Envs (Scal (Typ UT UT UT) Val)], Topology)
makeSequence ::
   PowerRecord ->
   Topology ->
   ([Envs
       (TC
          FSignal
          (Typ UT UT UT)
          (Data (UV.Vector :> Nil) Val))],
    Topology)
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
        recyc _ _ acc | otherwise = acc

-- | Function to remove Nil-Sections which have same start and stop Index
removeNilSections :: (Sequ,SequPwrRecord) ->   (Sequ, SequPwrRecord)
removeNilSections (sequ, SequData pRecs) = (fsequ, SequData fRecs)
  where (fsequ, fRecs) = unzip $ filter f $ zip sequ pRecs
        f ((lastIdx, idx), _) | lastIdx == idx = False
        f _ = True


-- | Function to detect and classify a step over several signals
stepDetect :: RSamp1 -> RSamp1 -> EventType
stepDetect  (t1,ps1) (t2,ps2) = f
  where stepList :: Samp1L (Typ A STy Tt) StepType
        stepList = S.tzipWith stepX ps1 ps2

        f :: EventType
        f
           | S.all (==NoStep) stepList = NoEvent
           | S.any (==ZeroCrossingStep) stepList = error $ "Error in stepDetect - Zero Crossing - t1: " ++ show t1 ++ " t2 :" ++ (show t2)
           | S.any (==LeavesZeroStep) stepList && (not $ S.any (==BecomesZeroStep) stepList) = LeftEvent
           | (not $ S.any (==LeavesZeroStep) stepList) && S.any (==BecomesZeroStep) stepList = RightEvent
           | S.any (==LeavesZeroStep) stepList && S.any (==BecomesZeroStep) stepList = MixedEvent


-- | Function to detect and classify a step over one signal
stepX :: PSamp -> PSamp -> Samp (Typ A STy Tt) StepType
stepX p1 p2
   | S.sign p1== toSample ZSign && S.sign p2 /= toSample ZSign = toSample LeavesZeroStep -- signal leaves zero
   | S.sign p1/=toSample ZSign && S.sign p2 == toSample ZSign = toSample BecomesZeroStep -- signal becomes zero
   | S.sign p1==toSample PSign && S.sign p2 == toSample NSign = toSample ZeroCrossingStep
   | S.sign p1==toSample NSign && S.sign p2 == toSample PSign = toSample ZeroCrossingStep
   | otherwise = toSample NoStep  -- nostep


addZeroCrossings :: PowerRecord -> PowerRecord
addZeroCrossings r = rsig2Record rSigNew r
  where rSig = record2RSig r
        rSigNew = (f (rtail rSig) (rhead rSig) mempty) .++ (rsingleton $ rlast rSig)

        f :: RSig -> RSamp1 -> RSig -> RSig
        f rSig rold rSigNew | rSig /= mempty = f (rtail rSig) rnew (rSigNew .++ (getZeroCrossings rold rnew))
          where rnew = rhead rSig
        f _ _ rSigNew |  otherwise = rSigNew

-----------------------------------------------------------------------------------
-- | Function for calculating zero Crossings

getZeroCrossings :: RSamp1 -> RSamp1 -> RSig
getZeroCrossings rs1@(t1,ps1) rs2 = ((S.singleton t1) .++ zeroCrossingTimes,(S.singleton ps1) .++ zeroPowers)
          where
             (zeroCrossings, zeroCrossingTimes) = calcZeroTimes rs1 rs2
             zeroPowers = calcZeroPowers rs1 rs2 zeroCrossingTimes zeroCrossings


calcZeroPowers :: RSamp1 -> RSamp1 -> TSigL -> TZeroSamp1L -> PSamp2LL
calcZeroPowers (t1,(TC (Data ps1))) (t2,(TC (Data ps2))) zeroCrossingTimes (TC (Data tz)) = S.transpose2 $ fromSigList sigList
               where g p1 p2 tz = f (toSample p1) (toSample p2) (toSample tz)
                     sigList = L.zipWith3 g ps1 ps2 tz :: [PSigL]

                     f :: PSamp -> PSamp -> TZeroSamp -> PSigL
                     f p1 p2 zeroCrossing = interpPowers (t1,p1) (t2,p2) zeroCrossingTimes zeroCrossing

calcZeroTimes :: RSamp1 -> RSamp1 -> (TZeroSamp1L,TSigL)
calcZeroTimes (t1,ps1) (t2,ps2)  = (zeroCrossings, zeroCrossingTimes)
              where
                -- | create ascending list containing all zero crossing times
                 zeroCrossingTimes = S.sort $ filterTZero zeroCrossings :: TSigL
                 zeroCrossings = S.tzipWith h2 ps1 ps2 :: TZeroSamp1L

                 -- | Zero crossing time per signal, if zero crossing happens otherwise empty
                 h2 :: PSamp -> PSamp -> TZeroSamp
                 h2 p1 p2 | S.sign p1 == toSample PSign && S.sign p2 == toSample NSign = calcZeroTime (t1,p1) (t2,p2)
                 h2 p1 p2 | S.sign p1 == toSample NSign && S.sign p2 == toSample PSign = calcZeroTime (t1,p1) (t2,p2)
                 h2 _  _ = toSample NoCrossing

-----------------------------------------------------------------------------------
-- | Interpolation Functions for one Signal

-- | calculate time of Zero Crossing Point
calcZeroTime :: (TSamp,PSamp) -> (TSamp,PSamp) -> TZeroSamp
calcZeroTime (t1,p1) (t2,p2) = s
  where m = (p2.-p1)./(t2.-t1) -- interpolation slope
        s =
           case compare t2 t1 of
              GT -> makeTZero $ dt.+t1 where dt = changeType $ ((S.neg p1)./m) :: DTSamp -- time of zero crossing
              EQ -> makeTZero t1
              LT -> error ("Error in calcZeroTime- Discontinous time vector t1: " ++ show t1 ++ " t2: " ++ show t2)


-- | interpolate Powers at Zero Crossing times
interpPowers :: (TSamp,PSamp) -> (TSamp,PSamp) -> TSigL -> TZeroSamp -> PSigL
interpPowers (t1,p1) (t2,p2) tzeroList tzero = S.tmap f tzeroList
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
makeTZero (TC (Data x)) = TC $ Data $ ZeroCrossing x

filterTZero :: TZeroSamp1L -> TSigL
filterTZero = S.transpose1 . S.map (\ (ZeroCrossing x) -> x) . S.filter (/=NoCrossing)



-----------------------------------------------------------------------------------
-- | Helper Functions

-- | Generate rSig from Power Record
updateMap :: Ord k => M.Map k a -> [a] -> M.Map k a
updateMap pmap xs = if check then M.fromList $ zip keys xs else error "Error in updateMap - map and List length don't match"
  where keys = map fst $ M.toList pmap
        check = length keys == length xs

record2RSig :: PowerRecord -> RSig
record2RSig (PowerRecord t pMap) = (t, S.transpose2 $ fromSigList $ M.elems pMap)

rsig2Record :: RSig -> PowerRecord -> PowerRecord
rsig2Record (t, ps) (PowerRecord _ pMap) = PowerRecord t  (M.fromList (zip  keys  (toSigList  $ S.transpose2 ps)))
   where keys = map fst (M.toList pMap)

rsig2SecRecord :: PowerRecord -> RSig -> SecPowerRecord
rsig2SecRecord  (PowerRecord _ pMap) (t, ps) = SecPowerRecord (S.convert t)  (M.fromList (zip  keys  (map S.convert $ toSigList  $ S.transpose2 ps)))
   where keys = map fst (M.toList pMap)
