{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Signal.Record where

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Data as D
import qualified EFA.Signal.Vector as V
import EFA.Signal.Signal
          (TC(TC),
           Range(Range),
           Scalar,
           Signal,
           FSignal,
           TSigL,
           UTSignal,
           TSignal,
           TSamp,
           PSamp,
           PSamp1L,
           PSamp2LL,
           FDistrib)
--           UTDistr,
--           FDistr)
import EFA.Signal.Typ (Typ,
                       A,
                       D,
                       P,
                       T,
                       Tt,
                       UT,
                       F,
                       --D
                      )
import EFA.Signal.Data (Data(Data),
                        (:>),
                        Nil)

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-), Product, (~*), (~/), Constant, abs)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatStructureEdge)

import EFA.Report.Report (ToTable(toTable), Table(..), tvcat)
import EFA.Report.Typ (TDisp, getDisplayTypName)
import EFA.Report.Base (DispStorage1)

import Text.Printf (PrintfArg)
import qualified Test.QuickCheck as QC
import System.Random (Random)

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Set as Set ; import Data.Set (Set)
import qualified Data.List as List
import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT
import qualified Data.List.Key as Key
import qualified Data.List.Match as Match
import qualified Data.NonEmpty as NonEmpty; import Data.NonEmpty ((!:))

import Control.Monad (liftM2)
import Data.Ratio (Ratio, (%))
import Data.Foldable (foldMap)
import Data.List (transpose)
import Data.Tuple.HT (mapFst)
import Data.Ord.HT (inRange)

import qualified Prelude as P; import Prelude hiding (abs, map)

newtype SigId = SigId String deriving (Eq, Ord, Show, Read)

{-
-- Don't use this, if you want read to work!!!

instance Show SigId where
  show (SigId x) = show x
-}

type instance D.Value (Record s1 s2 t1 t2 id v d1 d2) = d2


data Record s1 s2 t1 t2 id v d1 d2 =
     Record { recordTime :: TC s1 t1 (Data (v :> Nil) d1),
              recordSignalMap :: Map id (TC s2 t2 (Data (v :> Nil) d2)) } 
              deriving (Show, Read, Eq)


type SignalRecord v d = Record Signal Signal (Typ A T Tt) (Typ UT UT UT) SigId v d d

type PowerRecord n v d = Record Signal Signal (Typ A T Tt) (Typ A P Tt) (Idx.PPos n) v d d

type FlowRecord n v d = Record Signal FSignal (Typ A T Tt) (Typ A F Tt) (Idx.PPos n) v d d

-- type CumFlowRecord n v d = Record Scalar Scalar (Typ A T Tt) (Typ A F Tt) (Idx.PPos n) v d d

type DTimeFlowRecord n v d = Record FSignal FSignal (Typ D T Tt) (Typ A F Tt) (Idx.PPos n) v d d

type DTimePowerRecord n v d = Record FSignal FSignal (Typ D T Tt) (Typ A P Tt) (Idx.PPos n) v d d


type DistRecord n v d = Record FDistrib FDistrib (Typ UT UT UT) (Typ A F Tt) (Idx.PPos n) v ([S.Class d], [S.SignalIdx]) d

-- data DistRecord n v d = DistRecord (UTDistr v ([S.Class d], [S.SignalIdx])) (Map (Idx.PPos n) (FDistr v d))


class (Ord id) => Index id where
   formatIndex :: id -> String

instance Index SigId where
   formatIndex (SigId str) = str

instance Node.C node => Index (Idx.PPos node) where
   formatIndex (Idx.PPos se) =
      Format.unUnicode $
      formatStructureEdge (Format.literal "ppos") se

formatIndexSet :: (Index id) => Set id -> String
formatIndexSet =
   Fold.foldMap (\i -> formatIndex i ++ "\n")



newtype Name = Name String

newtype DeltaName = DeltaName String

deltaName :: Name -> Name -> DeltaName
deltaName (Name x) (Name y) =  (DeltaName $ y ++ "_vs_" ++ x)

map ::
   (TC s1 t1 (Data (v :> Nil) d2) -> TC s2 t2 (Data (v :> Nil) d2)) ->
   Record s s1 t t1 id v d1 d2 -> Record s s2 t t2 id v d1 d2
map f (Record t ma) = Record t (Map.map f ma)

mapKeys ::
   (Index id2) =>
   (id1 -> id2) ->
   Record s1 s2 t1 t2 id1 v d1 d2 -> Record s1 s2 t1 t2 id2 v d1 d2
mapKeys f (Record t ma) = Record t (Map.mapKeys f ma)

mapWithKey ::
   (id ->
    TC s0 t0 (Data (v :> Nil) d2) ->
    TC s1 t1 (Data (v :> Nil) d2)) ->
   Record s s0 t t0 id v d1 d2 ->
   Record s s1 t t1 id v d1 d2
mapWithKey f (Record t ma) = Record t (Map.mapWithKey f ma)



-- | Time Access Function -- not for Distributions
getTime :: Record s1 s2 t1 t2 id v d d -> TC s1 t1 (Data (v :> Nil) d)
getTime (Record time _) = time

-- | Signal Access Function -- not for Distributions
getSig ::
   (Show (v d2), Index id) =>
   Record s1 s2 t1 t2 id v d1 d2 -> id -> TC s2 t2 (Data (v :> Nil) d2)
getSig (Record _ sigMap) key =
   Map.findWithDefault
      (error $
         "Record.getSig: unknown key " ++ formatIndex key ++ "\n" ++
         "available keys " ++ formatIndexSet (Map.keysSet sigMap))
      key sigMap

-- | Get Start and End time
{- Wollen wir wirklich (Typ A T Tt) vorschreiben?
getTimeWindow :: (Ord a,
                  V.Storage v a,
                  V.Singleton v) =>
                 Record s1 s2 (Typ A T Tt) t2 id v d1 d2 ->
                 (Scal (Typ A T Tt) a, Scal (Typ A T Tt) a)
-}
-- | not for Distributions
getTimeWindow ::
  (Ord d, V.Storage v d, V.Singleton v) =>
  Record s1 s2 t1 t2 id v d d ->
  (TC Scalar t1 (Data Nil d), TC Scalar t1 (Data Nil d))
getTimeWindow = S.unzip . S.minmax . getTime



diffTime ::
{-
   (V.Zipper v, V.Walker v, V.Singleton v, V.Storage v a, Sum a,
    DSucc abs delta) =>
   Record Signal s2 (Typ abs t1 p1) t2 id v a ->
   Record FSignal s2 (Typ delta t1 p1) t2 id v a
-}

   (V.Zipper v, V.Walker v, V.Singleton v, V.Storage v d, Sum d) =>
   FlowRecord node v d ->
   DTimeFlowRecord node v d
diffTime (Record time signals) = Record (S.delta time) signals


dTimePowerRecord ::
   (V.Zipper v, V.Walker v, V.Singleton v, V.Storage v d,
    Constant d) =>
   PowerRecord n v d ->
   DTimePowerRecord n v d
dTimePowerRecord (Record time signals) =
  map (S.deltaMap (\x y -> (x~+y) ~/ Arith.fromInteger 2)) $
    Record (S.delta time) signals




-- | Use carefully -- removes signal jitter around zero
removeZeroNoise ::
   (V.Walker v, V.Storage v d, Ord d, Constant d) =>
   d -> PowerRecord node v d -> PowerRecord node v d
removeZeroNoise threshold (Record time pMap) =
   Record time $ Map.map (S.map (hardShrinkage threshold)) pMap

hardShrinkage :: (Ord d, Constant d) => d -> d -> d
hardShrinkage threshold x =
   if abs x < threshold then Arith.zero else x


-- | Generate a new Record with selected signals
extract ::
   (Index id) =>
   [id] -> Record s1 s2 t1 t2 id v d1 d2 -> Record s1 s2 t1 t2 id v d1 d2
extract xs rec = extractLogSignals rec $ List.map (flip (,) id) xs
{-
extract ::
   (Show (v a), Index id) =>
extract xs rec@(Record time _) =
   Record time $ mapFromSet (getSig rec) $ Set.fromList xs
-}

-- | Split SignalRecord in even chunks
split ::
   (Index id) =>
   Int -> Record s1 s2 t1 t2 id v d1 d2 -> [Record s1 s2 t1 t2 id v d1 d2]
split n (Record time pMap) =
   List.map (Record time . Map.fromList) $
   ListHT.sliceVertical n $ Map.toList pMap


sortSigList ::
   (Ord d, Constant d, V.Walker v, V.Storage v d) =>
   [(SigId, TC Signal (Typ UT UT UT) (Data (v :> Nil) d))] ->
   [(SigId, TC Signal (Typ UT UT UT) (Data (v :> Nil) d))]
sortSigList = Key.sort (S.sum . snd)


-----------------------------------------------------------------------------------
-- Functions to support Signal Selection

-- | List of Operations for pre-processing signals

-- | create a Record of selected, and sign corrected signals
extractLogSignals ::
   (Index id) =>
   Record s1 s2 t1 t2 id v d1 d2 ->
   [(id, TC s2 t2 (Data (v :> Nil) d2) -> TC s2 t2 (Data (v :> Nil) d2))] ->
   Record s1 s2 t1 t2 id v d1 d2
extractLogSignals (Record time sMap) idList =
   let idMap = Map.fromList idList
       notFound = Set.difference (Map.keysSet idMap) (Map.keysSet sMap)
   in  if Set.null notFound
         then Record time $ Map.intersectionWith ($) idMap sMap
         else error $
                 "extractLogSignals: signals not found in record: " ++
                 formatIndexSet notFound ++ "\n" ++
                 "Available Keys in Map : \n" ++
                 (formatIndexSet $ Map.keysSet sMap)


genPowerRecord ::
  ( Show (v d), V.Zipper v, V.Walker v,
    V.Storage v d, Product d, Ord node) =>
  TSignal v d ->
  [(Idx.PPos node, UTSignal v d, UTSignal v d)] ->
  PowerRecord node v d
genPowerRecord time =
   Record time .
      foldMap
         (\(pposIdx, sigA, sigB) ->
            Map.fromList
               [(pposIdx, S.setType sigA),
                (Idx.flip pposIdx, S.setType sigB)])


addSignals ::
   (Index id, V.Len (v d1), V.Len (v d2)) =>
   [(id, TC s2 t2 (Data (v :> Nil) d2))]  ->
   Record s1 s2 t1 t2 id v d1 d2 -> Record s1 s2 t1 t2 id v d1 d2
addSignals list (Record time m) =  (Record time (foldl f m list))
  where f ma (ident,sig) =
          if S.len time == S.len sig
             then Map.insert ident sig ma
             else error $ "Error in addSignals - signal length differs: "
                          ++ formatIndex ident


-- | adding signals of two records with same time vector by using Data.Map.union
union ::
   (Eq (v d1), Index id) =>
   Record s1 s2 t1 t2 id v d1 d2 ->
   Record s1 s2 t1 t2 id v d1 d2 ->
   Record s1 s2 t1 t2 id v d1 d2
union (Record timeA mA) (Record timeB mB) =
   if timeA == timeB
      then Record timeA
             (Map.unionWith
                (error "EFA.Signal.Record.union: duplicate signal ids") mA mB)
      else error "EFA.Signal.Record.union: time vectors differ"

{-
-- Wegen newTimeBase ist der Typ nicht so algemein wie bei "union" oben. Schade.
unionWithNewTime ::
  ( Eq (v d), Show d, Show (v d),
    Index id,
    Product d,
    Ord d,
    V.Filter v,
    V.Storage v d,
    V.Walker v,
    V.Singleton v,
    V.Lookup v,
    V.Find v,
    V.Sort v) =>
  [Record S.Signal S.Signal (Typ A T Tt) t2 id v d d] ->
  Record S.Signal S.Signal (Typ A T Tt) t2 id v d d
unionWithNewTime rs = Record newTime $
  Map.unionsWith (error "unionWithNewTime: duplicate signal ids") $
    List.map ((\(Record _ m) -> m) . flip (newTimeBase "unionWithNewTime") newTime) rs
  where (starts, ends) = unzip $ List.map getTimeWindow rs
        newTime = S.sort $ List.foldr1 S.append $ List.map (filt . getTime) rs
        filt = S.filter $ inRange $ (,)
                 (S.fromScalar (maximum starts))
                 (S.fromScalar (minimum ends))
-}

nonEmptyUnzip :: NonEmpty.T [] (a, b) -> (NonEmpty.T [] a, NonEmpty.T [] b)
nonEmptyUnzip (NonEmpty.Cons (x, y) rest) =
  let (xs, ys) = unzip rest in (x !: xs, y !: ys)


unionWithNewTime ::
  ( Eq (v d), Show d, Show (v d),
    Ord id, Show id, Product d, Ord d,
    V.Filter v, V.FromList v, V.Storage v d, V.Walker v,
    V.Singleton v, V.Lookup v, V.Find v, V.Sort v) =>
  NonEmpty.T [] (Record S.Signal S.Signal (Typ A T Tt) t2 id v d d) ->
  Record S.Signal S.Signal (Typ A T Tt) t2 id v d d
unionWithNewTime rs = Record newTime $
  Map.unionsWith (error "unionWithNewTime: duplicate signal ids") $
  NonEmpty.toList $ fmap g rs
  where g x = recordSignalMap $ newTimeBase "unionWithNewTime" x newTime
        (starts, ends) = nonEmptyUnzip $ fmap getTimeWindow rs
        newTime = NonEmpty.foldBalanced (S.mergeBy (<=)) $ fmap (filt . getTime) rs
        filt =
           S.filter $
              inRange
                 (S.fromScalar (NonEmpty.maximum starts),
                  S.fromScalar (NonEmpty.minimum ends))

-- | Modify the SigId
modifySigId ::
  (String -> String) ->
  Record s1 s2 t1 t2 SigId v d1 d2 ->
  Record s1 s2 t1 t2 SigId v d1 d2
modifySigId f = mapKeys (\(SigId str) -> SigId (f str))


-- | Modify specified signals with function
modifySignals ::
   (Index id) =>
   ToModify id ->
   (TC s2 t2 (Data (v :> Nil) d2) ->
    TC s2 t2 (Data (v :> Nil) d2)) ->
   Record s1 s2 t1 t2 id v d1 d2->
   Record s1 s2 t1 t2 id v d1 d2
modifySignals idList f (Record time ma) =
  Record time $
  List.foldl' (flip $ Map.adjust f) ma $
  case idList of
       ModifyAll -> Map.keys ma
       ToModify x -> x

-- | Get maximum signal range for all signals specified
maxRange ::
  ( Ord d2, V.Storage v d2, Show (v d2), V.Singleton v, Index id) =>
  RangeFrom id ->
  Record s1 s2 t1 t2 id v d1 d2 ->
  (TC Scalar t2 (Data Nil d2), TC Scalar t2 (Data Nil d2))
maxRange list rec@(Record _ m) =
  (S.toScalar $ minimum lmin, S.toScalar $ maximum lmax)
  where (lmin, lmax) =
           unzip $
           List.map (S.fromScalar . S.minmax . getSig rec) $
           case list of
              RangeFromAll -> Map.keys m
              RangeFrom w -> w


-- | Get maximum signal range for all signals specified
data RangeFrom id = RangeFrom [id] | RangeFromAll
data ToModify id = ToModify [id] | ModifyAll

normSignals2Range :: (Index id,
                      Ord d2,
                      Show (v d2),
                      V.Storage v d2,
                      V.Singleton v,
                      V.Walker v,
                      Product d2)  =>
                     (RangeFrom id, ToModify id) ->
                     Record s1 s2 t1 t2 id v d1 d2 ->
                     Record s1 s2 t1 t2 id v d1 d2
normSignals2Range (listM,listN) record = modifySignals listN f record
  where (TC (Data minx),TC (Data maxx)) = maxRange listM record
        f = S.map (\y -> y ~* (maxx ~- minx) ~+ minx) . S.norm

normSignals2Max75 :: (Index id,
                      Ord d2,
                      Show (v d2),
                      V.Storage v d2,
                      V.Singleton v,
                      V.Walker v,
                      Constant d2)  =>
                     (RangeFrom id, ToModify id) ->
                     Record s1 s2 t1 t2 id v d1 d2 ->
                     Record s1 s2 t1 t2 id v d1 d2
normSignals2Max75 (listM,listN) record = modifySignals listN f record
  where ( _ ,TC (Data maxx)) = maxRange listM record
        f = S.map (\y -> y ~* Arith.fromRational 0.75 ~* maxx) . S.norm

-- | Norm all signals to one
norm :: (Product d2,
         Ord d2,
         V.Walker v,
         V.Storage v d2,
         V.Singleton v) =>
        Record s1 s2 t1 t2 id v d1 d2 -> Record s1 s2 t1 t2 id v d1 d2
norm rec = map S.norm rec


-- | Add interpolated data points in an existing record
newTimeBase ::
  (Product d, Ord d, V.Find v, Show d, Show (v d),
   V.Lookup v, V.Walker v, V.Singleton v, V.Storage v d) =>
  String ->
  Record Signal Signal (Typ A T Tt) t2 id v d d ->
  TSignal v d ->
  Record Signal Signal (Typ A T Tt) t2 id v d d
newTimeBase caller (Record time m) newTime = Record newTime (Map.map f m)
  where f sig = S.interp1LinSig caller time sig newTime



-- | Create a new Record by slicing time and all signals on given Indices
slice ::
   (V.Slice v, V.Storage v d) =>
   Record s1 s2 t1 t2 id v d d ->
   Range ->
   Record s1 s2 t1 t2 id v d d
slice (Record t m) (Range sidx1@(S.SignalIdx idx1) (S.SignalIdx idx2)) =
    Record (f t) (Map.map f m)
  where f ::
           (V.Slice v, V.Storage v d) =>
           TC s t (Data (v :> Nil) d) -> TC s t (Data (v :> Nil) d)
        f = S.slice sidx1 (idx2-idx1+1)


{- | Filter Sequence Flow

Used to filter Modelica signals.
State changes in solver create several DataPoints with exact the same time.
The resulting sections which have zero time duration are removed.
-}
longerThanZero ::
   (Sum d, Ord d, V.Storage v d, V.Singleton v) =>
   PowerRecord node v d -> Bool
longerThanZero = uncurry (/=) . getTimeWindow

-- | Check for minimum duration
longerThan ::
   (Sum d, Ord d, V.Storage v d, V.Singleton v) =>
   d -> Record s1 s2 (Typ A T Tt) t2 id v d d -> Bool
longerThan threshold r =
   case getTimeWindow r of
      (TC (Data x), TC (Data y)) -> abs (x ~- y) > threshold

-- | Check for minimum duration
longerEqual ::
   (Constant d, Ord d, V.Storage v d, V.Singleton v) =>
   d -> Record s1 s2 (Typ A T Tt) t2 id v d d -> Bool
longerEqual threshold r =
   case getTimeWindow r of
      (TC (Data x), TC (Data y)) -> abs (x ~- y) >= threshold

-- | Check for negligible energy flow
energyBelow ::
   (Constant d, Ord d, V.Walker v, V.Storage v d) =>
   d -> FlowRecord node v d -> Bool
energyBelow threshold (Record _ fMap) =
   Fold.all (\s -> abs (S.fromScalar (S.sum s)) < threshold) fMap


major ::
   (Constant d, Ord d,
    V.Storage v d, V.Singleton v, V.Walker v) =>
   TC Scalar (Typ A F Tt) (Data Nil d) ->
   TC Scalar (Typ A T Tt) (Data Nil d) ->
   FlowRecord id v d -> Bool
major (S.TC (D.Data energyThreshold)) (S.TC (D.Data timeThreshold)) rec =
   not (energyBelow energyThreshold rec)
   &&
   longerEqual timeThreshold rec


-----------------------------------------------------------------------------------
-- Various Class and Instance Definition for the different Sequence Datatypes

instance
   (Sample d1,
    Sample d2,
    V.FromList v,
    V.Storage v d1,
    V.Storage v d2,
    QC.Arbitrary id,
    Index id) =>
      QC.Arbitrary (Record s1 s2 t1 t2 id v d1 d2) where
   arbitrary = do
      xs <- QC.listOf arbitrarySample
      n <- QC.choose (1,5)
      pos <- QC.vectorOf n QC.arbitrary
      let vectorSamples =
             ListHT.switchR [] (\equalSized _ -> equalSized) $
             ListHT.sliceVertical n xs
      return $
         Record (S.fromList $ Match.take vectorSamples $ iterate (1+) 0) $
         Map.fromList $ zip pos $ List.map S.fromList $ transpose vectorSamples

{-
we need this class,
because QC.choose requires a Random instance
but there is no Random Ratio instance
-}
class Num d => Sample d where arbitrarySample :: QC.Gen d
instance Sample Double where arbitrarySample = QC.choose (-1,1)
instance (Random d, Integral d) => Sample (Ratio d) where
   arbitrarySample = do
      x <- QC.choose (-100,100)
      y <- QC.choose (-100,100)
      return $
         case compare (P.abs x) (P.abs y) of
            LT -> x%y
            GT -> y%x
            EQ -> 1 -- prevent 0/0


instance
   (V.Walker v,
    V.Singleton v,
    V.FromList v,
    V.Storage v d1,
    V.Storage v d2,
    DispStorage1 v,
    Ord d1,
    Constant d1,
    PrintfArg d1,
    Index id,
    Ord d2,
    Constant d2,
    PrintfArg d2,
    S.DispApp s1,
    S.DispApp s2,
    TDisp t1,
    TDisp t2) =>
   ToTable (Record s1 s2 t1 t2 id v d1 d2) where
   toTable os (ti, Record time sigs) =
      [Table {
         tableTitle =
            (getDisplayTypName $ S.getDisplayType $ snd $ head sigList) ++
            "Record - " ++ ti,
         tableData = tableData t,
         tableFormat = tableFormat t,
         tableSubTitle = ""}]

      where sigList = Map.toList sigs
            t = tvcat $ S.toTable os ("Time",time) !:
                        concatMap (toTable os . mapFst formatIndex) sigList


------------------------------------
-- RSignal als Transponierte Form


type Sig = (TSigL, PSamp2LL)
type Samp1 = (TSamp, PSamp1L)
type Samp = (TSamp, PSamp)


--viewL :: Sig -> Maybe (Samp1, Sig)
viewL
  :: (V.Storage v1 (D.Apply v2 d), V.Storage v3 (D.Apply v4 d1),
      V.Singleton v1, V.Singleton v3, S.TailType s, S.TailType s1) =>
     (TC s typ (Data (v1 :> v2) d), TC s1 typ1 (Data (v3 :> v4) d1))
     -> Maybe
          ((TC (S.Head s) typ (Data v2 d), TC (S.Head s1) typ1 (Data v4 d1)),
           (TC s typ (Data (v1 :> v2) d), TC s1 typ1 (Data (v3 :> v4) d1)))

viewL (t, ps) =
   liftM2 zipPairs (S.viewL t) (S.viewL ps)

--viewR :: Sig -> Maybe (Sig, Samp1)
viewR
  :: (V.Storage v1 (D.Apply v2 d), V.Storage v3 (D.Apply v4 d1),
      V.Singleton v1, V.Singleton v3, S.TailType s, S.TailType s1) =>
     (TC s typ (Data (v1 :> v2) d), TC s1 typ1 (Data (v3 :> v4) d1))
     -> Maybe
          ((TC s typ (Data (v1 :> v2) d), TC s1 typ1 (Data (v3 :> v4) d1)),
           (TC (S.Head s) typ (Data v2 d), TC (S.Head s1) typ1 (Data v4 d1)))
viewR (t,ps) =
   liftM2 zipPairs (S.viewR t) (S.viewR ps)

zipPairs :: (a,b) -> (c,d) -> ((a,c), (b,d))
zipPairs (a,b) (c,d) = ((a,c), (b,d))

len :: Sig -> Int
len  (t,ps) = min (S.len t) (S.len ps)

singleton
  :: (V.Storage v (D.Apply c d), V.Storage v1 (D.Apply c1 d1),
      S.Singleton s v c, S.Singleton s1 v1 c1) =>
     (TC (S.SingletonSource s) t (Data c d),
      TC (S.SingletonSource s1) t1 (Data c1 d1))
     -> (TC s t (Data (v :> c) d), TC s1 t1 (Data (v1 :> c1) d1))
singleton (t,ps) = (S.singleton t, S.singleton ps)



-- * Conversion between Signal and Power Record

-- | Convert a power record to a signal record
powerToSignal :: (Show id) => PowerRecord id v d -> SignalRecord v d
powerToSignal (Record time m) =
   Record time $
   Map.mapKeys (\x -> SigId $ show x) $
   Map.map S.untype m

-- | Plot Records with readible keys
powerToSignalWithFunct ::
   (Ord node, Show node, Show (v d)) =>
   (Idx.PPos node -> SigId) -> PowerRecord node v d -> SignalRecord v d
powerToSignalWithFunct funct rec = map S.untype $ mapKeys funct rec

-- | Combine a power and a signal record together in a signal record (plotting)
combinePowerAndSignal :: (Eq (v d), Show id) => PowerRecord id v d -> SignalRecord v d -> SignalRecord v d
combinePowerAndSignal pr sr = union (powerToSignal pr) sr

-- | Combine a power and a signal record together in a signal record (plotting)
combinePowerAndSignalWithFunction :: (Eq (v d),
                                      Ord node,
                                      Show (v d),
                                      Show node) =>
                                     (Idx.PPos node -> SigId) -> PowerRecord node v d -> SignalRecord v d -> SignalRecord v d
combinePowerAndSignalWithFunction funct pr sr = union (powerToSignalWithFunct funct pr) sr

-- | Add Record name to SigId -- can be used for plotting multiple records in one window
addRecName2SigId :: String -> SignalRecord v d -> SignalRecord v d
addRecName2SigId name (Record time sigs) = Record time (Map.mapKeys (\ (SigId x) -> SigId (name ++ "_" ++ x) ) sigs)

-- | Integrate power signal step wise to get a flow record
partIntegrate :: (V.Zipper v,
                  V.Walker v,
                  V.Storage v d,
                  V.Singleton v,
                  Constant d) => PowerRecord node v d -> FlowRecord node v d
partIntegrate rec@(Record time _) = map (S.partIntegrate time) rec

-- | Classify a flow record to get a distribution record
distribution :: (V.FromList v,
                 V.Filter v,
                 V.Unique v (S.Class d),
                 V.Storage v S.SignalIdx,
                 V.Storage v Int,
                 V.Storage v (S.Class d),
                 RealFrac d,
                 V.Walker v,
                 V.Storage v d,
                 V.Storage v ([S.Class d], [S.SignalIdx]),
                 V.Lookup v,
                 Constant d,
                 V.Find v,
                 Node.C n,
                 Show (v d)) => FlowRecord n v d -> [Idx.PPos n] -> d -> d -> DistRecord n v d
distribution rec@(Record _ pMap) xs interval offset = Record classification energyDistribution
  where classification =
           S.combineDistributions $
           List.map
              ((S.genDistribution1D $ S.classifyEven interval offset) .
               S.changeSignalType . S.untype . getSig rec)
              xs
        energyDistribution =
           Map.map (S.calcDistributionValues classification) pMap

-- | Careful quick hack

sumFlowRecord :: (V.FromList v,
                  V.Zipper v,
                  V.Walker v,
                  V.Storage v d,
                  V.Singleton v,
                  Constant d) => FlowRecord node v d -> FlowRecord node v d
sumFlowRecord (Record time pmap) =
   Record
      (S.fromList $ [head $ S.toList time, last $ S.toList time])
      (Map.map (S.fromList . (\x -> [x]) . S.fromScalar . S.sum) pmap)


{-
sumFlowRecord :: (V.FromList v,
                  V.Zipper v,
                  V.Walker v,
                  V.Storage v d,
                  V.Singleton v,
                  Product d) => DTimeFlowRecord node v d -> CumFlowRecord node v d
sumFlowRecord (Record dtime map) = Record (S.sum dtime) (Map.map (S.sum) map)
-}
