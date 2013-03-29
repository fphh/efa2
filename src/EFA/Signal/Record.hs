{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts#-}

module EFA.Signal.Record where

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Data as D
import qualified EFA.Signal.Vector as V
import EFA.Signal.Signal
          (TC, Signal, FSignal, TSigL, UTSignal, TSignal,
           TSamp, PSamp, PSamp1L, PSamp2LL,Scal)

import EFA.Signal.Typ (Typ, A, P, T, Tt, UT,F,D)
import EFA.Signal.Data (Data, (:>), Nil)
import EFA.Signal.Base (Sign, BSum, BProd)

import EFA.Report.Report (ToTable(toTable), Table(..), tvcat)
import EFA.Report.Typ (TDisp, getDisplayTypName)
import EFA.Report.Base (DispStorage1)

import Text.Printf (PrintfArg)
import qualified Test.QuickCheck as QC
import System.Random (Random)

import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.List.HT as HTL
import qualified Data.List.Key as Key
import qualified Data.List.Match as Match

import Data.NonEmpty ((!:))
import Data.Ratio (Ratio, (%))
import Data.Foldable (foldMap)
import Data.List (transpose)
import Data.Tuple.HT (mapFst)
import Control.Monad (liftM2)
import EFA.Utility (checkedLookup)


newtype SigId = SigId String deriving (Eq, Ord)

instance Show SigId where
  show (SigId x) = show x


-- | Indices for Power Position
-- data PPosIdx = PPosIdx !Idx.Node !Idx.Node deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------------
-- | Indices for Power Position
data PPosIdx node = PPosIdx !node !node deriving (Show, Eq, Ord)

flipPos ::  PPosIdx node -> PPosIdx node
flipPos (PPosIdx idx1 idx2) = PPosIdx idx2 idx1


type instance D.Value (Record s t1 t2 id v a) = a


data Record s t1 t2 id v a =
     Record (TC s t1 (Data (v :> Nil) a))
            (M.Map id (TC s t2 (Data (v :> Nil) a))) deriving (Show, Eq)
                                                              
                                                              

type SignalRecord = Record Signal (Typ A T Tt) (Typ UT UT UT) SigId

type PowerRecord n = Record Signal (Typ A T Tt) (Typ A P Tt) (PPosIdx n)

type FlowRecord n = Record FSignal (Typ D T Tt) (Typ A F Tt) (PPosIdx n)

-- | Flow record to contain flow signals assigned to the tree
newtype FlowState node = FlowState (M.Map (PPosIdx node) Sign) deriving (Show)

-----------------------------------------------------------------------------------
-- | Indice Record Number

data Idx = Idx Int | NoIdx

instance Show Idx where
  show (Idx x) = "Rec" ++ show x
  show NoIdx = ""



-- | Access Functions
getTime :: Record s t1 t2 id v a ->  TC s t1 (Data (v :> Nil) a)
getTime (Record time _) = time


getSig :: (Show (v a),Ord id, Show id) => Record s t1 t2 id v a -> id -> TC s t2 (Data (v :> Nil) a)
getSig (Record _ sigMap) key = checkedLookup sigMap key

-- | Get Start and End time
getTimeWindow :: (Ord a,
                  V.Storage v a,
                  V.Singleton v) =>
                 Record s (Typ A T Tt) t2 id v a ->
                 (Scal (Typ A T Tt) a, Scal (Typ A T Tt) a)
getTimeWindow rec = (S.minimum t, S.maximum t)
  where t = getTime rec

-- | Use carefully -- removes signal jitter around zero
removeZeroNoise :: (V.Walker v, V.Storage v a, Ord a, Num a) => PowerRecord node v a -> a -> PowerRecord node v a
removeZeroNoise (Record time pMap) threshold =
   Record time $ M.map (S.map (hardShrinkage threshold)) pMap

hardShrinkage :: (Ord a, Num a) => a -> a -> a
hardShrinkage threshold x =
   if abs x < threshold then 0 else x


-- | Generate a new Record with selected signals
extract ::
   (Ord id, Show id) =>
   [id] -> Record s t1 t2 id v a -> Record s t1 t2 id v a
extract xs rec = extractLogSignals rec $ map (flip (,) id) xs
{-
extract ::
   (Show (v a), Ord id, Show id) =>
extract xs rec@(Record time _) =
   Record time $ mapFromSet (getSig rec) $ Set.fromList xs
-}

-- | Split SignalRecord in even chunks
split ::
   (Ord id) =>
   Int -> Record s t1 t2 id v a -> [Record s t1 t2 id v a]
split n (Record time pMap) =
   map (Record time . M.fromList) $ HTL.sliceVertical n $ M.toList pMap


sortSigList ::
   (Num a, Ord a,
    V.Walker v, V.Storage v a, BSum a) =>
   [(SigId, TC Signal (Typ UT UT UT) (Data (v :> Nil) a))] ->
   [(SigId, TC Signal (Typ UT UT UT) (Data (v :> Nil) a))]
sortSigList = Key.sort (S.sigSum . snd)


-----------------------------------------------------------------------------------
-- Functions to support Signal Selection

-- | List of Operations for pre-processing signals

-- | create a Record of selected, and sign corrected signals
extractLogSignals ::
   (Ord id, Show id) =>
   Record s t1 t2 id v a ->
   [(id, TC s t2 (Data (v :> Nil) a) -> TC s t2 (Data (v :> Nil) a))] ->
   Record s t1 t2 id v a
extractLogSignals (Record time sMap) idList =
   let idMap = M.fromList idList
       notFound = Set.difference (M.keysSet idMap) (M.keysSet sMap)
   in  if Set.null notFound
         then Record time $ M.intersectionWith ($) idMap sMap
         else error $ "signals not found in record: " ++ show notFound


genPowerRecord :: (Show (v a),
                   V.Zipper v,
                   V.Walker v,
                   V.Storage v a,
                   BProd a a,
                   BSum a,
                   Ord node) =>
                  TSignal v a -> [(PPosIdx node, UTSignal v a, UTSignal v a)] -> PowerRecord node v a
genPowerRecord time =
   Record time .
      foldMap
         (\(pposIdx, sigA, sigB) ->
            M.fromList
               [(pposIdx, S.setType sigA),
                (flipPos pposIdx, S.setType sigB)])


addSignals :: (Ord id, V.Len (v a),Show id) => 
              [(id, TC s t2 (Data (v :> Nil) a))]  -> 
              Record s t1 t2 id v a -> Record s t1 t2 id v a
addSignals list (Record time m) =  (Record time (foldl f m list))   
  where f ma (ident,sig) = if S.len time == S.len sig 
                       then M.insert ident sig ma
                       else error ("Error in addSignals - signal length differs: " ++ show ident) 

-- | Add interpolated data points in an existing record 
newTimeBase :: (Fractional a,
                Ord a,
                V.Find v,
                V.Lookup v,
                V.Walker v,
                V.Singleton v,
                V.Storage v a) => 
               Record Signal (Typ A T Tt) t2 id v a -> TSignal v a -> Record Signal  (Typ A T Tt)  t2 id v a
newTimeBase (Record time m) newTime = Record newTime (M.map f m)   
  where f sig = S.interp1LinSig time sig newTime  



-- | Create a new Record by slicing time and all signals on given Indices
slice ::
   (V.Slice v, V.Storage v a) =>
   Record s t1 t2 id v a -> (Int, Int) {- Range -} -> Record s t1 t2 id v a
slice (Record t m) (idx1,idx2) = Record (f t) (M.map f m)
  where f ::
           (V.Slice v, V.Storage v a) =>
           TC s t (Data (v :> Nil) a) -> TC s t (Data (v :> Nil) a)
        f = S.slice idx1 (idx2-idx1+1)

-----------------------------------------------------------------------------------
-- Various Class and Instance Definition for the different Sequence Datatypes

instance (QC.Arbitrary node) => QC.Arbitrary (PPosIdx node) where
   arbitrary = liftM2 PPosIdx QC.arbitrary QC.arbitrary
   shrink (PPosIdx from to) = map (uncurry PPosIdx) $ QC.shrink (from, to)

instance
   (Sample a, V.FromList v, V.Storage v a, QC.Arbitrary id, Ord id) =>
      QC.Arbitrary (Record s t1 t2 id v a) where
   arbitrary = do
      xs <- QC.listOf arbitrarySample
      n <- QC.choose (1,5)
      ppos <- QC.vectorOf n QC.arbitrary
      let vectorSamples =
             HTL.switchR [] (\equalSized _ -> equalSized) $
             HTL.sliceVertical n xs
      return $
         Record (S.fromList $ Match.take vectorSamples $ iterate (1+) 0) $
         M.fromList $ zip ppos $ map S.fromList $ transpose vectorSamples

{-
we need this class,
because QC.choose requires a Random instance
but there is no Random Ratio instance
-}
class Num a => Sample a where arbitrarySample :: QC.Gen a
instance Sample Double where arbitrarySample = QC.choose (-1,1)
instance (Random a, Integral a) => Sample (Ratio a) where
   arbitrarySample = do
      x <- QC.choose (-100,100)
      y <- QC.choose (-100,100)
      return $
         case compare (abs x) (abs y) of
            LT -> x%y
            GT -> y%x
            EQ -> 1 -- prevent 0/0


instance
   (V.Walker v, V.Singleton v, V.FromList v, V.Storage v a, DispStorage1 v,
    Ord a, Fractional a, PrintfArg a, Show id,
    S.DispApp s, TDisp t1, TDisp t2) =>
   ToTable (Record s t1 t2 id v a) where
   toTable os (ti, Record time sigs) =
      [Table {
         tableTitle =
            (getDisplayTypName $ S.getDisplayType $ snd $ head sigList) ++
            "Record - " ++ ti,
         tableData = tableData t,
         tableFormat = tableFormat t,
         tableSubTitle = ""}]

      where sigList = M.toList sigs
            t = tvcat $ S.toTable os ("Time",time) !:
                        concatMap (toTable os . mapFst show) sigList


------------------------------------
-- RSignal als Transponierte Form


type Sig = (TSigL, PSamp2LL)
type Samp1 = (TSamp, PSamp1L)
type Samp = (TSamp, PSamp)


viewL :: Sig -> Maybe (Samp1, Sig)
viewL (t,ps) =
   liftM2 zipPairs (S.viewL t) (S.viewL ps)

viewR :: Sig -> Maybe (Sig, Samp1)
viewR (t,ps) =
   liftM2 zipPairs (S.viewR t) (S.viewR ps)

zipPairs :: (a,b) -> (c,d) -> ((a,c), (b,d))
zipPairs (a,b) (c,d) = ((a,c), (b,d))

len :: Sig -> Int
len  (t,ps) = min (S.len t) (S.len ps)

singleton :: Samp1 -> Sig
singleton (t,ps) = (S.singleton t, S.singleton ps)
