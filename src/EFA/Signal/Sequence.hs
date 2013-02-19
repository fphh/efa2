{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Sequence where

-- import qualified EFA.Equation.Env as Env
-- import EFA.Equation.Env (Env(..))

import qualified EFA.Graph.Flow as Flow
-- import qualified EFA.Graph.Topology.Index as Idx
import EFA.Graph.Topology (Topology, FlowTopology, SequFlowGraph)

import qualified EFA.Signal.Base as SB
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as V
import qualified EFA.Signal.Record as Record

import EFA.Signal.SequenceData
          (SequData(..), Sequ, Sec,
           filterSequWithSequData, filterSequWithSequData2)


import EFA.Signal.Record (Record(..), PowerRecord, FlowRecord)

import EFA.Signal.Base
          (Val, Sign(..), ZeroCrossing(..))
import EFA.Signal.Signal
          (TC(TC),  TSigL, TZeroSamp1L, TZeroSamp, TSamp, PSamp, PSigL,
           DTSamp, PSamp2LL, Samp, Samp1L,
           (.+), (.-), (.*), (./), (.++),
            sampleAverage, deltaSig, sigPartInt, sigFullInt,
           changeType, fromScalar, toSample, sigSum, toSigList, fromSigList)

import EFA.Signal.Typ (Typ, STy, Tt, T, P, A)
import EFA.Signal.Data (Data(Data), Nil, (:>))

import qualified Data.Foldable as Fold
import qualified Data.NonEmpty.Mixed as NonEmptyM
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as HTL
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)

import Data.Bool.HT (if')
import Data.Eq.HT (equating)
import Data.Tuple.HT (mapPair)
import Control.Functor.HT (void)
import Control.Monad (liftM2)
import Data.Monoid (Monoid, mempty, mconcat)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Maybe.HT (toMaybe)


data StepType = LeavesZeroStep
              | BecomesZeroStep
              | ZeroCrossingStep
              | NoStep deriving (Eq, Show,Ord)

data EventType = LeftEvent
               | RightEvent
               | MixedEvent
               | NoEvent

{-

-- | From PowerRecord
--fromFlowRecord :: Idx.Section -> Idx.Record -> FlowRecord -> Env rec FSig -- [Val]

--fromFlowRecord :: Idx.Section -> Idx.Record -> FlowRecord a b -> Env rec a --UTFSig
fromFlowRecord ::
   Idx.Section ->
   Idx.Record ->
   FlowRecord node v a ->
 --     (TC s1 (Typ delta2 t2 p2) (Data c1 d1))
 --     (TC s1 (Typ delta1 t1 p1) (Data c1 d1)) ->
   Env Env.NoRecord (TC FSignal (Typ UT UT UT) (Data v a))
fromFlowRecord secIdx recIdx (FlowRecord dTime flowMap) =
  (Env.empty Env.NoRecord) { energyMap = M.map untype $ M.mapKeys f flowMap, dtimeMap = M.fromList [(Idx.DTime recIdx secIdx, untype dTime)] }
  where f (PPosIdx idx1 idx2) =
           Idx.Energy recIdx (Idx.SecNode secIdx idx1) (Idx.SecNode secIdx idx2)

  --where f ((PPosIdx idx1 idx2), (flowSig)) = ((Idx.Power secIdx recIdx idx1 idx2), [fromScalar $ sigSum flowSig])
-}


-- | Pre-Integrate all Signals in Record
recFullIntegrate :: (Num a,
                      V.Zipper v,
                      V.Walker v,
                      V.Singleton v,
                      SB.BSum a,
                      SB.BProd a a,
                      V.Storage v a,
                      V.FromList v) =>
                    PowerRecord node v a -> FlowRecord node v a
recFullIntegrate (Record time pMap) = Record (S.fromList [fromScalar $ sigSum $ deltaSig time]) fMap
  where fMap = M.map (sigFullInt time) pMap

-- | Pre-Integrate all Signals in Record
recPartIntegrate :: (Num a,
                      V.Zipper v,
                      V.Walker v,
                      V.Singleton v,
                      SB.BSum a,
                      SB.BProd a a,
                      V.Storage v a,
                      V.FromList v) =>
                    PowerRecord node v a -> FlowRecord node v a
recPartIntegrate (Record time pMap) = Record (deltaSig time) fMap
  where fMap = M.map (sigPartInt time) pMap

-- | Generate Sequence Flow
genSequFlow :: (Num a,
                V.Zipper v,
                V.Walker v,
                V.Storage v a,
                V.Singleton v,
                V.FromList v,
                SB.BSum a,
                SB.BProd a a)=>
               (SequData (PowerRecord node v a)) -> SequData (FlowRecord node v a)
genSequFlow sqPRec = fmap recFullIntegrate sqPRec

-- | Filter Sequence Flow
-- | Used to filter Modelica signals 
-- | State changes in solver create several DataPoints with exact the same time
-- | The resulting sections which have zero time duration are removed 


removeZeroTimeSections :: (Fractional a, Ord a, Eq a, V.Storage v a, V.Singleton v) => (Sequ,SequData (PowerRecord nty v a)) -> (Sequ,SequData (PowerRecord nty v a))
removeZeroTimeSections (xs, ys)  = filterSequWithSequData f (xs, ys) 
   where f (_,Record time _) = x /= y
           where 
              err = error "Error in SequenceData.hs / removeZeroTimeSections -- empty head or tail"
              TC (Data x) = (fst $ maybe err id $ S.viewL time) 
              TC (Data y) = (snd $ maybe err id $ S.viewR time) 

-- | Drop Sections with time duration below threshold
removeLowTimeSections :: (Fractional a, Ord a, Eq a, V.Storage v a, V.Singleton v) => (Sequ,SequData (PowerRecord nty v a)) -> a -> (Sequ,SequData (PowerRecord nty v a))
removeLowTimeSections (xs, ys)  threshold = filterSequWithSequData f (xs, ys) 
   where  
          f (_,Record time _) = abs (x -y) > threshold 
            where 
              err = error "Error in SequenceData.hs / removeZeroTimeSections -- empty head or tail"
              TC (Data x) = (fst $ maybe err id $ S.viewL time) 
              TC (Data y) = (snd $ maybe err id $ S.viewR time) 

-- | Drop Sections with negligible energy flow
removeLowEnergySections :: (Num a, SB.BSum a, Ord a, V.Walker v, V.Storage v a) =>
   (Sequ, SequData (PowerRecord node v a), SequData (FlowRecord node v a))
   -> a
   -> (Sequ, SequData (PowerRecord node v a), SequData (FlowRecord node v a))
removeLowEnergySections  (xs, ys, zs) threshold = filterSequWithSequData2 f (xs, ys, zs)
   where  f (_, _ , Record _ fMap) =  not $ all g (M.toList fMap)
          g (_,s) = (abs (fromScalar (sigSum s))) < threshold  


-- TODO: Umschalten zwischen recFullIntegrate und recPartIntegrate.
--genSequFlow :: (SequData PowerRecord) -> SequData FlowRecord
--genSequFlow sqPRec = fmap recFullIntegrate sqPRec
{-
-- makeSequence :: PowerRecord -> Topology -> ([Env rec (Scal (Typ UT UT UT) Val)], Topology)
makeRecSequence ::
   SequData (FlowRecord node v a) ->
   SequData (Env Env.NoRecord
       (TC
          FSignal
          (Typ UT UT UT)
          (Data (UV.Vector :> Nil) Val)))
makeRecSequence =
   zipWithSecIdxs (flip fromFlowRecord (Idx.Record Idx.Absolute))
-}

makeSeqFlowGraph ::
  (Fractional a,
   Ord a,
   V.Walker v,
   V.Storage v a,
   SB.BSum a,
   Ord node,
   Show node) =>
   Topology node ->
   SequData (FlowRecord node v a) ->
   SequFlowGraph node
makeSeqFlowGraph topo =
   Flow.mkSequenceTopology .
   Flow.genSectionTopology .
   Flow.genSequFlowTops topo .
   Flow.genSequFState


makeSeqFlowTopology ::
  (Ord node) => SequData (FlowTopology node) -> SequFlowGraph node
makeSeqFlowTopology =
   Flow.mkSequenceTopology .
   Flow.genSectionTopology

makeSequence ::
   (Show node, Ord node) => PowerRecord node [] Val ->
   SequData (FlowRecord node [] Val)
makeSequence =
    genSequFlow . snd . removeZeroTimeSections . genSequ . addZeroCrossings

{-
-- | PG - Its better to have processing under controll in Top-Level for inspeting and debugging signal treatment
makeSequenceRaw ::
   (Show node, Ord node) => PowerRecord node [] Val ->
   (Sequ, SequData (PowerRecord node [] Val))
makeSequenceRaw = genSequ . addZeroCrossings
-}
-----------------------------------------------------------------------------------
{-
ToDo:
Must be fixed for empty signals and
must correctly handle the last section.
-}
-- | Function to Generate Time Sequence
genSequ ::  Ord node => PowerRecord node [] Val -> (Sequ, SequData (PowerRecord node [] Val))
genSequ pRec = removeNilSections (SequData $ sequ++[lastSec], SequData pRecs)
  where rSig = record2RSig pRec
        pRecs = map (rsig2SecRecord pRec) (seqRSig ++ [lastRSec])
        ((lastSec,sequ),(lastRSec,seqRSig)) = recyc rTail rHead (((0,0),[]),(Record.singleton $ rHead,[]))
          where
            (rHead, rTail) = maybe err id $ Record.viewL rSig
            err = error ("Error in EFA.Signal.Sequence/genSequence, case 1 - empty rSig")

        recyc ::
           Record.Sig -> Record.Samp1 ->
           ((Sec, [Sec]), (Record.Sig, [Record.Sig])) ->
           ((Sec, [Sec]), (Record.Sig, [Record.Sig]))

        -- Incoming rSig is at least two samples long -- detect changes
        recyc rsig x1 (((lastIdx,idx),sq),(secRSig, sqRSig)) |
          (Record.len rsig) >=2 = recyc rTail x2 (g $ stepDetect x1 x2, f $ stepDetect x1 x2)
          where
            (x2, rTail) = maybe err id $ Record.viewL rsig
            err = error ("Error in EFA.Signal.Sequence/genSequence, case 2 - empty rSig")
            xs1 = Record.singleton x1
            xs2 = Record.singleton x2

            f :: EventType -> (Record.Sig, [Record.Sig])
            f LeftEvent = (xs1.++xs2, sqRSig ++ [secRSig])           -- add actual Interval to next section
            f RightEvent = (xs2, sqRSig ++ [secRSig .++ xs2])     --add actual Interval to last section
            f MixedEvent = (xs2, sqRSig ++ [secRSig] ++ [xs1 .++ xs2]) -- make additional Mini--Section
            f NoEvent = (secRSig .++ xs2, sqRSig)                  -- continue incrementing

            g :: EventType -> (Sec, [Sec])
            g LeftEvent = ((idx, idx+1), sq ++ [(lastIdx, idx)])
            g RightEvent = ((idx+1, idx+1), sq ++ [(lastIdx, idx+1)])
            g MixedEvent = ((idx+1, idx+1), sq ++ [(lastIdx, idx)] ++ [(idx, idx+1)])
            g NoEvent = ((lastIdx, idx+1), sq)

        -- Incoming rList is only one Point long -- append last sample to last section
        recyc rsig _ (((lastIdx,idx),sq),(secRSig, sqRSig)) | (Record.len rsig) >=1 = (((lastIdx,idx+1),sq),(secRSig .++ rsig, sqRSig))

        -- Incoming rList is empty -- return result
        recyc _ _ acc = acc


-- | Function to remove Nil-Sections which have same start and stop Index
removeNilSections :: (Sequ,(SequData (PowerRecord node v a))) ->   (Sequ, (SequData (PowerRecord node v a)))
removeNilSections (SequData sequ, SequData pRecs) = (SequData fsequ, SequData fRecs)
  where (fsequ, fRecs) = unzip $ filter (uncurry (/=) . fst) $ zip sequ pRecs


-- | Function to detect and classify a step over several signals
stepDetect :: Record.Samp1 -> Record.Samp1 -> EventType
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
           | otherwise = error ("Sequence.hs, stepDetect unforeseen case")                                                                            


-- | Function to detect and classify a step over one signal
stepX :: PSamp -> PSamp -> Samp (Typ A STy Tt) StepType
stepX p1 p2
   | S.sign p1== toSample ZSign && S.sign p2 /= toSample ZSign = toSample LeavesZeroStep -- signal leaves zero
   | S.sign p1/=toSample ZSign && S.sign p2 == toSample ZSign = toSample BecomesZeroStep -- signal becomes zero
   | S.sign p1==toSample PSign && S.sign p2 == toSample NSign = toSample ZeroCrossingStep
   | S.sign p1==toSample NSign && S.sign p2 == toSample PSign = toSample ZeroCrossingStep
   | otherwise = toSample NoStep  -- nostep


addZeroCrossings ::(Ord node) => PowerRecord node [] Val -> PowerRecord node [] Val
addZeroCrossings r = rsig2Record rSigNew0 r
  where rSigNew0 =
           case record2RSig r of
              rSig ->
                 case liftM2 (,) (Record.viewL rSig) (Record.viewR rSig) of
                    Nothing -> error "addZeroCrossings: empty signal"
                    Just ((rHead, rTail), (_, rLast)) ->
                       f rTail rHead mempty .++ Record.singleton rLast

        f :: Record.Sig -> Record.Samp1 -> Record.Sig -> Record.Sig
        f rSig rold rSigNew =
           case Record.viewL rSig of
              Nothing -> rSigNew
              Just (rnew, rTail) ->
                 f rTail rnew (rSigNew .++ getZeroCrossings rold rnew)

-----------------------------------------------------------------------------------
-- | Function for calculating zero Crossings

getZeroCrossings :: Record.Samp1 -> Record.Samp1 -> Record.Sig
getZeroCrossings rs1@(t1,ps1) rs2 = ((S.singleton t1) .++ zeroCrossingTimes,(S.singleton ps1) .++ zeroPowers)
          where
             (zeroCrossings, zeroCrossingTimes) = calcZeroTimes rs1 rs2
             zeroPowers = calcZeroPowers rs1 rs2 zeroCrossingTimes zeroCrossings


calcZeroPowers :: Record.Samp1 -> Record.Samp1 -> TSigL -> TZeroSamp1L -> PSamp2LL
calcZeroPowers (t1,(TC (Data ps1))) (t2,(TC (Data ps2))) zeroCrossingTimes (TC (Data tz)) = S.transpose2 $ fromSigList sigList
               where g p1 p2 tz2 = f (toSample p1) (toSample p2) (toSample tz2)
                     sigList = L.zipWith3 g ps1 ps2 tz :: [PSigL]

                     f :: PSamp -> PSamp -> TZeroSamp -> PSigL
                     f p1 p2 zeroCrossing = interpPowers (t1,p1) (t2,p2) zeroCrossingTimes zeroCrossing

calcZeroTimes :: Record.Samp1 -> Record.Samp1 -> (TZeroSamp1L,TSigL)
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
-- * Interpolation Functions for one Signal

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
-- * TZero helper Functions

makeTZero :: TSamp -> TZeroSamp
makeTZero (TC (Data x)) = TC $ Data $ ZeroCrossing x

filterTZero :: TZeroSamp1L -> TSigL
filterTZero =
   S.transpose1 .
   S.mapMaybe (\ c -> case c of ZeroCrossing x -> Just x; NoCrossing -> Nothing)



-----------------------------------------------------------------------------------
-- * Conversions between Record.Sig and Record

-- | Generate rSig from Power Record
updateMap :: Ord k => M.Map k a -> [b] -> M.Map k b
updateMap pmap xs =
   case M.keys pmap of
      keys ->
         if void keys == void xs
           then M.fromList $ zip keys xs
           else error "Error in updateMap - map and List length don't match"

type RSigX a =
        (TC S.Signal (Typ A T Tt) (Data ([] :> Nil) a),
         TC S.Sample (Typ A P Tt) (Data ([] :> [] :> Nil) a))

record2RSig :: PowerRecord node [] a -> RSigX a
record2RSig (Record t pMap) = (t, S.transpose2 $ fromSigList $ M.elems pMap)

rsig2Record :: Ord node => RSigX a -> PowerRecord node [] a -> PowerRecord node [] a
rsig2Record (t, ps) (Record _ pMap) =
   Record t $ updateMap pMap $ toSigList $ S.transpose2 ps

rsig2SecRecord ::
   (V.Convert [] v, V.Storage v a, Ord node) =>
   PowerRecord node [] a ->
   RSigX a ->
   PowerRecord node v a
rsig2SecRecord (Record _ pMap) (t, ps) =
   Record (S.convert t) $
   updateMap pMap $ map S.convert $ toSigList $ S.transpose2 ps


-----------------------------------------------------------------------------------
-- * Alternative approach

{- |
A crossing like [1,0,-1] will be counted twice
and will be filtered out by chopAtZeroCrossingsRSig using allEqual.
This allows for consistency with [1,0,0,-1]
where we actually want to count two crossings.
-}
checkZeroCrossing :: (RealFrac a) => a -> a -> Maybe a
checkZeroCrossing x0 x1 =
   toMaybe (compare x0 0 /= compare x1 0) (-x0/(x1-x0))

multiZeroCrossings :: (RealFrac a) => [a] -> [a] -> M.Map a IntSet
multiZeroCrossings xs ys =
   M.fromListWith IntSet.union $ catMaybes $
   zipWith (fmap . flip (,) . IntSet.singleton) [0..] $
   zipWith checkZeroCrossing xs ys
{-
   zipWith3 (\i x y -> fmap (flip (,) i) $ checkZeroCrossing x y) [0..] xs ys
-}

{- |
This version touches more elements than necessary
but I hope that it is easier to fuse.
-}
clearAt :: Num a => IntSet -> [a] -> [a]
clearAt ns = zipWith (\i -> if' (IntSet.member i ns) 0) [0..]

interpolate :: (RealFrac a) => a -> a -> a -> a
interpolate i x y = (1-i)*x + i*y

{-
clearAt is used to insert exact zeros
where we detected zero crossings.
If you compute with exact number types, clearAt can be omitted.
-}
sample :: (RealFrac a) => (a, IntSet) -> [a] -> [a] -> [a]
sample (i,ns) xs ys = clearAt ns $ zipWith (interpolate i) xs ys

expandIntervals :: (a -> b) -> (a -> a -> [b]) -> [a] -> [b]
expandIntervals g f xs0 =
   case xs0 of
      [] -> []
      xt@(x:xs) ->
         g x : concat (zipWith (\x0 x1 -> f x0 x1 ++ [g x1]) xt xs)

{-
This removes duplicate nodes
when zero crossings coincide with original nodes.
For floating point arithmetic
we may get rounding errors at the original nodes.
But for real world data we still need a way to handle
jitter around zero crossings anyway.
-}
removeDuplicates :: (Eq b) => (a -> b) -> [a] -> [a]
removeDuplicates f = map NonEmpty.head . NonEmptyM.groupBy (equating f)


chopAtZeroCrossings :: (RealFrac a) => [(a, [a])] -> [[(a, [a])]]
chopAtZeroCrossings =
   map (map snd) .
   HTL.segmentBefore fst .
   expandIntervals
      ((,) False)
      (\(xt,xs) (yt,ys) ->
         concatMap
            (\s ->
               let ss = (interpolate (fst s) xt yt, sample s xs ys)
               in  [(False, ss), (True, ss)]) $
         M.toAscList $
         multiZeroCrossings xs ys)

zeroCrossingsPerInterval :: (RealFrac a) => [[a]] -> [[[a]]]
zeroCrossingsPerInterval =
   HTL.mapAdjacent
      (\xs ys ->
         xs :
         (map (\s -> sample s xs ys) $
          M.toAscList $ multiZeroCrossings xs ys) ++
         ys :
         [])

chopAtZeroCrossingsRSig :: (RealFrac a) => RSigX a -> [RSigX a]
chopAtZeroCrossingsRSig (TC (Data times), TC (Data vectorSignal)) =
   map (mapPair (TC . Data, TC . Data)) $
   map unzip $
   filter (HTL.lengthAtLeast 2) $
   map (removeDuplicates fst) $
   chopAtZeroCrossings $
   zip times vectorSignal

chopAtZeroCrossingsPowerRecord ::
   (V.Convert [] v, V.Storage v a, RealFrac a, Ord node) =>
   PowerRecord node [] a -> SequData (PowerRecord node v a)
chopAtZeroCrossingsPowerRecord rSig =
   SequData $ map (rsig2SecRecord rSig) $
   chopAtZeroCrossingsRSig $
   record2RSig rSig

concatPowerRecords ::
   (V.Singleton v, V.Storage v a, Ord node) =>
   SequData (PowerRecord node v a) -> PowerRecord node v a
concatPowerRecords (SequData recs) =
   case recs of
      [] -> Record mempty M.empty
      Record time0 pMap0 : recs0 ->
         let recs1 = map tailPowerRecord recs0
         in  Record
                (mconcat $ time0 : map (\(Record times _) -> times) recs1)
                (M.mapWithKey
                    (\idx pSig ->
                       mconcat $ pSig :
                       mapMaybe (\(Record _ pMap) -> M.lookup idx pMap) recs1)
                    pMap0)

tailPowerRecord ::
   (V.Singleton v, V.Storage v a) =>
   PowerRecord node v a -> PowerRecord node v a
tailPowerRecord (Record times pMap) =
   Record
      (maybe mempty snd $ S.viewL times)
      (fmap (maybe mempty snd . S.viewL) pMap)


approxSequPwrRecord ::
   (V.Walker v, V.Storage v a, Real a, Ord node) =>
   a -> SequData (PowerRecord node v a) -> SequData (PowerRecord node v a) -> Bool
approxSequPwrRecord eps (SequData xs) (SequData ys) =
   V.equalBy (approxPowerRecord eps) xs ys

approxPowerRecord ::
   (V.Walker v, V.Storage v a, Real a,Ord node) =>
   a -> PowerRecord node v a -> PowerRecord node v a -> Bool
approxPowerRecord eps
      (Record xt xm) (Record yt ym) =
   S.equalBy (approxAbs eps) xt yt
   &&
   M.keys xm == M.keys ym
   &&
   Fold.and (M.intersectionWith (S.equalBy (approxAbs eps)) xm ym)

approxAbs :: (Real a) => a -> a -> a -> Bool
approxAbs eps x y =
   abs (x-y) <= eps



-----------------------------------------------------------------------------------
-- * New Functions from PG to allow Signal Cutting on Time Windows


-- | Get Start and Stop Times for all Power Records in a Sequence
extractCuttingTimes:: (Ord a,
                       V.Storage v a,
                       V.Singleton v) =>
                      SequData (PowerRecord node v a) ->
                      SequData (S.Scal (Typ A T Tt) a, S.Scal (Typ A T Tt) a)
extractCuttingTimes = fmap Record.getTimeWindow



-- | Create SequencePowerRecord by extracting Slices from Indices given by Sequence
sectionRecordsFromSequence ::  (V.Slice v, V.Storage v a) => Record s t1 t2 id v a -> Sequ -> SequData (Record s t1 t2 id v a)
sectionRecordsFromSequence rec = fmap (Record.slice rec)


-- | Generate Time Signal with Sequence Number to allow Plotting
genSequenceSignal :: (V.FromList v, V.Storage v a, Num a) => Sequ -> S.UTSignal v a 
genSequenceSignal (SequData xs) = S.fromList $ concat $ fmap f xs
  where
    f (idx1, idx2) = [1] ++ replicate (idx2-idx1-1) 0 ++ [-1]  
      
