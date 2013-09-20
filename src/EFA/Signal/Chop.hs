{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module EFA.Signal.Chop where


import qualified EFA.Graph.Flow as Flow
import EFA.Graph.Topology (Topology, FlowTopology)

import qualified EFA.Signal.Sequence as Sequ

import qualified EFA.Signal.Base as SB
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Vector as V
import qualified EFA.Signal.Record as Record

import EFA.Signal.Sequence (Range)



import EFA.Signal.Record (Record(Record), PowerRecord, FlowRecord)

import EFA.Signal.Base
          (Val, Sign(PSign, NSign, ZSign),
           ZeroCrossing(NoCrossing, ZeroCrossing))
import EFA.Signal.Signal
          (TC(TC),  TSigL, TZeroSamp1L, TZeroSamp, TSamp, PSamp, PSigL,
           DTSamp, PSamp2LL, Samp, Samp1L,
           (.+), (.-), (.*), (./), (.++),
           sampleAverage, changeType, toSample, toSigList, fromSigList)

import EFA.Signal.Typ (Typ, STy, Tt, T, P, A)
import EFA.Signal.Data (Data(Data), Nil, (:>))

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.NonEmpty.Set as NonEmptySet
import qualified Data.NonEmpty.Mixed as NonEmptyM
import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import qualified Data.List as L
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.NonEmpty ((!:))
import Data.Bool.HT (if')
import Data.Eq.HT (equating)
import Data.Tuple.HT (mapPair, swap)
import Control.Functor.HT (void)
import Control.Monad (liftM2)
import Data.Monoid (Monoid, mempty, mappend, mconcat)
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


{-# DEPRECATED genSequFlow "better use (fmap Record.partIntegrate)" #-}
-- | Generate Sequence Flow
genSequFlow :: (Num a,
                V.Zipper v,
                V.Walker v,
                V.Storage v a,
                V.Singleton v,
                V.FromList v,
                SB.BSum a,
                SB.BProd a a)=>
               (Sequ.List (PowerRecord node v a)) -> Sequ.List (FlowRecord node v a)
genSequFlow sqPRec = fmap Record.partIntegrate sqPRec


-- | Filter Sequence Flow
-- | Used to filter Modelica signals
-- | State changes in solver create several DataPoints with exact the same time
-- | The resulting sections which have zero time duration are removed


{-
separateUncleanSections :: (Num d,
                          V.Storage v d,
                          V.Singleton v,
                          SB.BSum d,
                          V.Walker v,
                          Ord d) =>
                         (Sequ, Sequ.List (PowerRecord id v d) , Sequ.List (FlowRecord id v d)) ->
                          ((Sequ, Sequ.List (PowerRecord id v d), Sequ.List (FlowRecord id v d)),
                          (Sequ, Sequ.List(PowerRecord id v d),  Sequ.List (FlowRecord id v d)),
                          (Sequ, Sequ.List(PowerRecord id v d),  Sequ.List (FlowRecord id v d)))

separateUncleanSections  (xs, ys, zs) =
  (Sequ.filter3 f (xs, ys, zs),
   Sequ.filter3 g (xs, ys, zs),
   Sequ.filter3 h (xs, ys, zs))
   where  f (_,q) = q == Flow.Clean
          g (_,q) = q == Flow.Dirty
          h (_,q) = q == Flow.Wrong
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
   Sequ.List (FlowRecord node v a) ->
   Flow.RangeGraph node
makeSeqFlowGraph topo =
   Flow.sequenceGraph .
   Flow.genSequFlowTops topo .
   Flow.genSequFState


makeSeqFlowTopology ::
   (Ord node, Show node) =>
   Sequ.List (FlowTopology node) ->
   Flow.RangeGraph node
makeSeqFlowTopology =
   Flow.sequenceGraph

makeSequence ::
   (Show node, Ord node) =>
   PowerRecord node [] Val ->
   Sequ.List (FlowRecord node [] Val)
makeSequence =
    genSequFlow . genSequ . addZeroCrossings

-----------------------------------------------------------------------------------
{-
ToDo:
Must be fixed for empty signals and
must correctly handle the last section.
-}
-- | Function to Generate Time Sequence
genSequ ::
   Ord node =>
   PowerRecord node [] Val ->
   Sequ.List (PowerRecord node [] Val)
genSequ pRec =
   removeNilSections $ Sequ.fromRangeList $ zip (sequ++[lastSec]) pRecs
  where rSig = record2RSig pRec

        inc (S.SignalIdx idx) = S.SignalIdx (idx+1)

        pRecs = map (rsig2SecRecord pRec) (seqRSig ++ [lastRSec])
        ((lastSec,sequ),(lastRSec,seqRSig)) =
           recyc rTail rHead
              ((Sequ.rangeSingleton (S.SignalIdx 0), []),
               (Record.singleton $ rHead, []))
          where
            (rHead, rTail) = maybe err id $ Record.viewL rSig
            err = error ("Error in EFA.Signal.Chop/genSequence, case 1 - empty rSig")

        recyc ::
           Record.Sig -> Record.Samp1 ->
           ((Range, [Range]), (Record.Sig, [Record.Sig])) ->
           ((Range, [Range]), (Record.Sig, [Record.Sig]))

        -- Incoming rSig is at least two samples long -- detect changes
        recyc rsig x1 (((Sequ.Range lastIdx idx),sq),(secRSig, sqRSig)) |
          (Record.len rsig) >=2 = recyc rTail x2 (g $ stepDetect x1 x2, f $ stepDetect x1 x2)
          where
            (x2, rTail) = maybe err id $ Record.viewL rsig
            err = error ("Error in EFA.Signal.Chop/genSequence, case 2 - empty rSig")
            xs1 = Record.singleton x1
            xs2 = Record.singleton x2

            f :: EventType -> (Record.Sig, [Record.Sig])
            f LeftEvent = (xs1.++xs2, sqRSig ++ [secRSig])           -- add actual Interval to next section
            f RightEvent = (xs2, sqRSig ++ [secRSig .++ xs2])     --add actual Interval to last section
            f MixedEvent = (xs2, sqRSig ++ [secRSig] ++ [xs1 .++ xs2]) -- make additional Mini--Section
            f NoEvent = (secRSig .++ xs2, sqRSig)                  -- continue incrementing

            g :: EventType -> (Range, [Range])

            g LeftEvent = (Sequ.Range idx (inc idx), sq ++ [Sequ.Range lastIdx idx])
            g RightEvent = (Sequ.Range (inc idx) (inc idx), sq ++ [Sequ.Range lastIdx (inc idx)])
            g MixedEvent = (Sequ.Range (inc idx) (inc idx), sq ++ [Sequ.Range lastIdx idx] ++ [Sequ.Range idx (inc idx)])
            g NoEvent = (Sequ.Range lastIdx (inc idx), sq)

        -- Incoming rList is only one Point long -- append last sample to last section
        recyc rsig _ ((Sequ.Range lastIdx idx, sq), (secRSig, sqRSig)) | (Record.len rsig) >=1 =
               ((Sequ.Range lastIdx (inc idx), sq), (secRSig .++ rsig, sqRSig))

        -- Incoming rList is empty -- return result
        recyc _ _ acc = acc


-- | Function to remove Nil-Sections which have same start and stop Index
removeNilSections ::
   Sequ.List (PowerRecord node v a) ->
   Sequ.List (PowerRecord node v a)
removeNilSections =
   Sequ.filterRange (not . Sequ.rangeIsSingleton)


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


--addZeroCrossings ::(Ord node) => PowerRecord node [] Val -> PowerRecord node [] Val

--addZeroCrossings ::
--  Record t0 t1 (Typ A T Tt) (Typ A P Tt) id0 [] Double Double ->
addZeroCrossings ::
  Ord node =>
  PowerRecord node [] Double ->
  PowerRecord node [] Double
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
updateMap :: Ord k => Map k a -> [b] -> Map k b
updateMap pmap xs =
   case Map.keys pmap of
      keys ->
         if void keys == void xs
           then Map.fromList $ zip keys xs
           else error "Error in updateMap - map and List length don't match"

type RSigX a =
        (TC S.Signal (Typ A T Tt) (Data ([] :> Nil) a),
         TC S.Sample (Typ A P Tt) (Data ([] :> [] :> Nil) a))



record2RSig ::
  (V.Transpose v1 v2, V.Storage v1 d, V.Storage v2 (v1 d),
      V.FromList v2, S.TransposeType s1 s2) =>
     Record t s1 t1 typ k v1 t2 d
     -> (TC t t1 (Data (v1 :> Nil) t2),
         TC s2 typ (Data (v2 :> (v1 :> Nil)) d))
record2RSig (Record t pMap) = (t, S.transpose2 $ fromSigList $ Map.elems pMap)

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

multiZeroCrossings :: (RealFrac a) => [a] -> [a] -> Map a IntSet
multiZeroCrossings xs ys =
   Map.fromListWith IntSet.union $ catMaybes $
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

interpolate :: (Num a) => a -> a -> a -> a
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
   ListHT.segmentBefore fst .
   expandIntervals
      ((,) False)
      (\(xt,xs) (yt,ys) ->
         concatMap
            (\s ->
               let ss = (interpolate (fst s) xt yt, sample s xs ys)
               in  [(False, ss), (True, ss)]) $
         Map.toAscList $
         multiZeroCrossings xs ys)

zeroCrossingsPerInterval :: (RealFrac a) => [[a]] -> [[[a]]]
zeroCrossingsPerInterval =
   ListHT.mapAdjacent
      (\xs ys ->
         xs :
         (map (\s -> sample s xs ys) $
          Map.toAscList $ multiZeroCrossings xs ys) ++
         ys :
         [])

chopAtZeroCrossingsRSig :: (RealFrac a) => RSigX a -> [RSigX a]
chopAtZeroCrossingsRSig (TC (Data times), TC (Data vectorSignal)) =
   map (mapPair (TC . Data, TC . Data)) $
   map unzip $
   filter (ListHT.lengthAtLeast 2) $
--   map (removeDuplicates fst) $
   chopAtZeroCrossings $
   zip times vectorSignal

chopAtZeroCrossingsPowerRecord ::
   (V.Convert [] v, V.Storage v a, RealFrac a, Ord node) =>
   PowerRecord node [] a -> Sequ.List (PowerRecord node v a)
chopAtZeroCrossingsPowerRecord rSig =
   Sequ.fromLengthList $
   map (\r -> (L.length $ S.unconsData $ fst r, rsig2SecRecord rSig r)) $
   chopAtZeroCrossingsRSig $
   record2RSig rSig

concatPowerRecords ::
   (V.Singleton v, V.Storage v a, Ord node) =>
   Sequ.List (PowerRecord node v a) -> PowerRecord node v a
concatPowerRecords recs =
   case Fold.toList recs of
      [] -> Record mempty Map.empty
      Record time0 pMap0 : recs0 ->
         let recs1 = map tailPowerRecord recs0
         in  Record
                (mconcat $ time0 : map (\(Record times _) -> times) recs1)
                (Map.mapWithKey
                    (\idx pSig ->
                       mconcat $ pSig :
                       mapMaybe (\(Record _ pMap) -> Map.lookup idx pMap) recs1)
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
   a -> Sequ.List (PowerRecord node v a) -> Sequ.List (PowerRecord node v a) -> Bool
approxSequPwrRecord eps xs ys =
   V.equalBy (approxPowerRecord eps) (Fold.toList xs) (Fold.toList ys)

approxPowerRecord ::
   (V.Walker v, V.Storage v a, Real a, Ord node) =>
   a -> PowerRecord node v a -> PowerRecord node v a -> Bool
approxPowerRecord eps
      (Record xt xm) (Record yt ym) =
   S.equalBy (approxAbs eps) xt yt
   &&
   Map.keys xm == Map.keys ym
   &&
   Fold.and (Map.intersectionWith (S.equalBy (approxAbs eps)) xm ym)

approxAbs :: (Real a) => a -> a -> a -> Bool
approxAbs eps x y =
   abs (x-y) <= eps




-- * Third approach using a Monoid structure

{-
We divide the problem into three functionalities:

* For every signal determine the positions of phase changes.
* Combine the position lists (this is a Monoid)
* Chop every signal according to the combined positions list.

This has several advantages:

* you can chop lists, vectors and other stream types without resorting to lists
* you can combine chunks with different element types


We should use a NonEmpty type for the chunks and for the time set.
Note the similarity to our EventList datatypes.
-}
newtype IntPattern a = IntPattern [(Int, NonEmptySet.T a)]
   deriving (Show)

{-
tests: test Monoid laws
-}
instance (Ord a) => Monoid (IntPattern a) where
   mempty = IntPattern []
   mappend (IntPattern pa) (IntPattern pb) =
      let go [] p = p
          go p [] = p
          go (ac@(ak,at):as) (bc@(bk,bt):bs) =
             case compare ak bk of
                EQ -> (ak, NonEmptySet.union at bt) : go as bs
                LT -> ac : go as ((bk-ak,bt) : bs)
                GT -> bc : go ((ak-bk,at) : as) bs
      in  IntPattern $ go pa pb

intPattern ::
   (V.Storage v a, V.FromList v, RealFrac a) =>
   v a -> IntPattern a
intPattern =
   IntPattern . snd .
   L.mapAccumL (\k0 (k1,t) -> (k1, (k1-k0, NonEmptySet.singleton t))) (-1) .
   catMaybes . zipWith (\k -> fmap ((,) k)) [0..] .
   ListHT.mapAdjacent checkZeroCrossing . V.toList


{- |
In contrast to 'IntPattern' I use @NonEmpty.T v ()@
for measuring lengths of chunks.
This has two advantages:

* Using the non-empty type the compiler
  can check non-emptiness for cutting operations.

* If @v@ is a lazy type we can measure length lazily.
-}
newtype Pattern v a = Pattern [(NonEmpty.T v (), NonEmptySet.T a)]
   deriving (Show)

{-
tests: test Monoid laws
-}
instance
   (V.DiffLength v, V.Storage v (), V.Core v ~ v, Ord a) =>
      Monoid (Pattern v a) where
   mempty = Pattern []
   mappend (Pattern pa) (Pattern pb) =
      let go [] p = p
          go p [] = p
          go (ac@(ak,at):as) (bc@(bk,bt):bs) =
             case V.diffLength ak bk of
                V.NoRemainder -> (ak, NonEmptySet.union at bt) : go as bs
                V.RemainderLeft  ck -> ac : go as ((ck, bt) : bs)
                V.RemainderRight ck -> bc : go ((ck, at) : as) bs
      in  Pattern $ go pa pb

pattern ::
   (V.Storage v a, V.Storage v (), V.FromList v, RealFrac a) =>
   v a -> Pattern v a
pattern =
   Pattern .
   map (mapPair (NonEmpty.Cons () . V.fromList . void, NonEmptySet.singleton)) .
   fst . ListHT.segmentAfterMaybe id .
   ListHT.mapAdjacent checkZeroCrossing . V.toList


chopVector ::
   (V.SplitMatch v, V.Singleton v, V.Core v ~ v,
    V.Storage v a, V.Storage v ()) =>
   [NonEmpty.T v ()] -> v a -> NonEmpty.T [] (NonEmpty.T v a)
chopVector ks v0 =
   (\(y,ys) -> NonEmpty.snoc ys $ checkedFetch y) $
   L.mapAccumL (\v k -> swap $ V.splitAtMatch k $ checkedFetch v) v0 ks

{- |
partial function

I need it, since I do not know, how to prove,
that the chunks sum up to the whole signal.
-}
checkedFetch ::
   (V.Singleton v, V.Storage v a) =>
   v a -> NonEmpty.T v a
checkedFetch xs =
   case V.viewL xs of
      Nothing -> error "empty vector - This may mean that signals of different lengths were chopped."
      Just (y,ys) -> NonEmpty.Cons y ys


type Chunk v = NonEmpty.T v

chopVectorInterpolate ::
   (Trav.Traversable v, NonEmptyC.Append v,
    NonEmptyC.Empty v, NonEmptyC.Cons v,
    V.Singleton v, V.SplitMatch v,
    V.Storage v a, V.Storage v (), V.Core v ~ v,
    Num a) =>
   Pattern v a -> v a -> [Chunk v a]
chopVectorInterpolate (Pattern p) =
   let go [] (NonEmpty.Cons v []) = [v]
       go (ts:tss) (NonEmpty.Cons v0 (v1:vs)) =
          case (NonEmpty.last v0, NonEmpty.head v1) of
             (al, ar) ->
                (\(vs2,v2) -> vs2 ++ go tss (v2 !: vs)) $
                NonEmpty.viewR $
                NonEmpty.mapAdjacent NonEmptyC.append $
                NonEmpty.cons v0 $ flip NonEmptyC.snoc v1 $
                fmap (\t -> NonEmpty.singleton $ interpolate t al ar) $
                NonEmptySet.toAscList ts
       go _ _ = error "lists must be equally long"
   in  case unzip p of
          (ks,ts) -> go ts . chopVector ks

{-
Usually the chunks should have at least two elements:
A beginning and an ending point.
The only exception I can see
can happen for an input signal of length one.
We could enforce an input signal of at least length two,
but I do not know, how to prove then that the Chunks have at least two elements.
-}
type Chunk2 v = NonEmpty.T (NonEmpty.T v)

chopVectorInterpolate2 ::
   (Trav.Traversable v, NonEmptyC.Append v,
    NonEmptyC.Empty v, NonEmptyC.Cons v,
    V.Singleton v, V.SplitMatch v,
    V.Storage v a, V.Storage v (), V.Core v ~ v,
    Num a) =>
   Pattern v a -> v a -> [Chunk2 v a]
chopVectorInterpolate2 (Pattern p) =
   let go [] (NonEmpty.Cons v []) = [NonEmpty.mapTail checkedFetch v]  -- we called 'flatten' in the last round, maybe too early
       go (ts:tss) (NonEmpty.Cons v0 (v1:vs)) =
          case (NonEmpty.last v0, NonEmpty.head v1) of
             (al, ar) ->
                (\(vs2,v2) -> vs2 ++ go tss (NonEmpty.flatten v2 !: vs)) $
                NonEmpty.viewR $
                NonEmpty.mapAdjacent NonEmpty.append $
                NonEmpty.cons v0 $ flip NonEmptyC.snoc v1 $
                fmap (\t -> NonEmpty.singleton $ interpolate t al ar) $
                NonEmptySet.toAscList ts
       go _ _ = error "lists must be equally long"
   in  case unzip p of
          (ks,ts) -> go ts . chopVector ks


chopVectorContainer ::
   (Trav.Traversable v, NonEmptyC.Append v,
    NonEmptyC.Empty v, NonEmptyC.Cons v,
    V.FromList v, V.DiffLength v,
    V.SplitMatch v, V.Singleton v,
    V.Storage v a, V.Storage v (), V.Core v ~ v,
    Trav.Traversable f,
    RealFrac a) =>
   f (v a) -> [f (Chunk v a)]
chopVectorContainer m =
   let p = Fold.foldMap pattern m
   in  Trav.traverse (chopVectorInterpolate p) m

{-
Possible tests:
   p <> p == (p::Pattern)

   bei Ergebnis von chopVectorContainer:
      an der Schnittstelle muss wenigstens eine Zahl null sein

   teste laziness

mappend (pattern [3,3,-1,-1,-1,-1,-1,-1::Double]) (pattern [1,1,-1,-1,-1,-1,1,1::Double])
-}

chopSignal ::
   (Trav.Traversable v, NonEmptyC.Append v,
    NonEmptyC.Empty v, NonEmptyC.Cons v,
    V.SplitMatch v, V.Singleton v,
    V.Storage v a, V.Storage v (), V.Core v ~ v,
    Num a) =>
   Pattern v a ->
   TC S.Signal (Typ A t Tt) (Data (v :> Nil) a) ->
   [TC S.Signal (Typ A t Tt) (Data (Chunk v :> Nil) a)]
chopSignal p = S.toSigList . S.liftData (chopVectorInterpolate p)

chopRecord ::
   (Trav.Traversable v, NonEmptyC.Append v,
    NonEmptyC.Empty v, NonEmptyC.Cons v,
    V.FromList v, V.DiffLength v,
    V.Length v, V.SplitMatch v, V.Singleton v,
    V.Storage v a, V.Storage v (), V.Core v ~ v,
    RealFrac a) =>
   PowerRecord node v a ->
   Sequ.List (PowerRecord node (Chunk v) a)
chopRecord (Record t m) =
   let p@(Pattern chunks) = Fold.foldMap (pattern . S.unconsData) m
   in  Sequ.fromLengthList $
       zip (map (V.length . fst) chunks) $
       zipWith Record
          (chopSignal p t)
          (Trav.traverse (chopSignal p) m)



-----------------------------------------------------------------------------------
-- * New Functions from PG to allow Signal Cutting on Time Windows


{-# DEPRECATED extractCuttingTimes "better use fmap Record.getTimeWindow" #-}
-- | Get Start and Stop Times for all Power Records in a Sequence
extractCuttingTimes:: (Ord a,
                       V.Storage v a,
                       V.Singleton v) =>
                      Sequ.List (PowerRecord node v a) ->
                      Sequ.List (S.Scal (Typ A T Tt) a, S.Scal (Typ A T Tt) a)
extractCuttingTimes = fmap Record.getTimeWindow
