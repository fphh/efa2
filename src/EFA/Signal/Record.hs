{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Signal.Record where
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Data as D
import qualified EFA.Signal.Vector as V
import EFA.Report.Base (DispStorage1)
import EFA.Signal.Signal
          (TC, Signal, neg, FSignal,TSamp,PSamp,PSamp1L,PSamp2LL,TSigL)
          
import EFA.Signal.Typ (Typ, A, P, T, Tt, UT,F,D)
import EFA.Signal.Data (Data, (:>), Nil)
import EFA.Signal.Base (Sign, Val, BSum, DArith0,BProd)

import EFA.Report.Report (ToTable(toTable), Table(..), tvcat)
import Text.Printf (PrintfArg)
import qualified Test.QuickCheck as QC
import System.Random (Random)

import qualified Data.Map as M
import qualified Data.List.HT as HTL
import qualified Data.List as L
import qualified Data.List.Match as Match

import Data.NonEmpty ((!:))
import Data.Ratio (Ratio, (%))
import Data.List (transpose)
import Data.Tuple.HT (mapFst)
import Control.Monad (liftM2)
import EFA.Utility (checkedLookup)




-----------------------------------------------------------------------------------
-- | Signal Record 

-- | Signal record to contain original time signals
-- data Record = Record TSig (M.Map SigId UTSigL) deriving (Show)

data SignalRecord v a =
   SignalRecord
      (TC Signal (Typ A T Tt) (Data (v :> Nil) a))
      (M.Map SigId (TC Signal (Typ UT UT UT) (Data (v :> Nil) a)))
   deriving (Show, Eq)

type instance D.Value (SignalRecord v a) = a

data SigId = SigId String deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------------
-- | Indices for Power Position
data PPosIdx nty = PPosIdx !nty !nty deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------------
-- | Power Record 

-- | Power record to contain power signals assigned to the tree
data PowerRecord nty v a =
   PowerRecord
      (TC Signal (Typ A T Tt) (Data (v :> Nil) a))
      (M.Map (PPosIdx nty) (TC Signal (Typ A P Tt) (Data (v :> Nil) a)))
   deriving (Show, Eq)

type instance D.Value (PowerRecord nty v a) = a

type ListPowerRecord nty = PowerRecord nty [] Val

-----------------------------------------------------------------------------------
-- | Flow Record 

-- | Flow record to contain flow signals assigned to the tree
-- | Do we need a flow value record ?? 
data FlowRecord nty v a =
   FlowRecord
      (TC FSignal (Typ D T Tt) (Data (v :> Nil) a))
      (M.Map (PPosIdx nty) (TC FSignal (Typ A F Tt) (Data (v :> Nil) a)))
   deriving (Show, Eq)

type instance D.Value (FlowRecord nty v a) = a

-- | Flow record to contain flow signals assigned to the tree
newtype FlowState nty = FlowState (M.Map (PPosIdx nty) Sign) deriving (Show)
-- type SequFlowState = SequData FlowState
-- type SequFlowTops = SequData FlowTopology


-----------------------------------------------------------------------------------
-- Utility Functions on Records 

getTime :: SignalRecord v a -> TC Signal (Typ A T Tt) (Data (v :> Nil) a) 
getTime (SignalRecord time _) = time

getPTime :: PowerRecord nty v a -> TC Signal (Typ A T Tt) (Data (v :> Nil) a) 
getPTime (PowerRecord time _) = time

getSig ::
  Show (v a) =>
  SignalRecord v a -> SigId -> TC Signal (Typ UT UT UT) (Data (v :> Nil) a)   
getSig (SignalRecord _ sigMap) sigId = checkedLookup sigMap sigId

getPSig ::
  (Show (v a), Ord nty, Show nty) =>
  PowerRecord nty v a -> PPosIdx nty
  -> TC Signal (Typ A P Tt) (Data (v :> Nil) a)   
getPSig (PowerRecord _ pMap) idx = checkedLookup pMap idx

-- | Use carefully -- removes signal jitter around zero 
removeZeroNoise ::
  (V.Walker v, V.Storage v a, Ord a, Num a) =>
  PowerRecord nty v a -> a -> PowerRecord nty v a        
removeZeroNoise (PowerRecord time pMap) threshold =
  PowerRecord time (M.map f pMap)
  where f sig = S.map g sig
        g x | abs x < threshold = 0 
            | otherwise = x

-- | Generate a new Record with selected signals
selectRecord ::
  Show (v a) =>
  SignalRecord v a -> [SigId] -> SignalRecord v a
selectRecord rec@(SignalRecord time _ ) xs =
  SignalRecord time  (M.fromList $ zip xs (map f xs))
  where f x = getSig rec x
        
        
-- | Split SignalRecord in even Junks                          
splitSignalRecord ::
  (Num a, Ord a, V.Walker v, V.Storage v a, BSum a) =>
  SignalRecord v a -> Int -> [SignalRecord v a]                          
splitSignalRecord (SignalRecord time pMap) n  = recList
  where (recList, _) = f ([],sortSigList $ M.toList pMap)
        f (rs, []) = (rs,[])        
        f (rs, xs) = f (rs ++ [SignalRecord time (M.fromList $ take n xs)], drop n xs)
        

sortSigList ::
  (Num a, Ord a, V.Walker v, V.Storage v a, BSum a) =>
  [ (SigId,TC Signal (Typ UT UT UT) (Data (v :> Nil) a))]
  ->  [(SigId, TC Signal (Typ UT UT UT) (Data (v :> Nil) a))]
sortSigList  sigList = L.sortBy g  sigList
  where g (_,x) (_,y) = compare (S.sigSum x) (S.sigSum y) 

-- | Split PowerRecord in even Junks                          
splitPowerRecord ::
  (Num a, Ord a, V.Walker v, V.Storage v a, BSum a, Ord nty) =>
  PowerRecord nty v a -> Int -> [PowerRecord nty v a]                          
splitPowerRecord (PowerRecord time pMap) n  = recList
  where (recList, _) = f ([],M.toList pMap)
        f (rs, []) = (rs,[])        
        f (rs, xs) =
          f (rs ++ [PowerRecord time (M.fromList $ take n xs)], drop n xs)
 

-----------------------------------------------------------------------------------
-- Functions to support Signal Selection
 
-- | List of Operations for pre-processing signals
-- | TODO - a gadt construction would be awesome to provide parameters        
data SignalOps = Negate | RemoveZeroNoise | Filter | Offset          
        
-- | create a Record of selected, and sign corrected signals
extractLogSignals ::
  (V.Walker v, V.Storage v a, DArith0 a, Show (v a)) => 
  SignalRecord v a -> [(SigId, [SignalOps])] -> SignalRecord  v a        
extractLogSignals rec@(SignalRecord time _) idList =
  SignalRecord time (M.fromList $ map f idList)
  where f (SigId sigId,opList) = (SigId sigId, foldl g  signal opList)
          where -- newId = SigId $ Str.replace "." "_" sigId              
                signal = getSig rec (SigId sigId)              
                g  sig Negate = neg sig
                g  sig _ = sig

data PowerCalc a = Take a | Extra a | Mult a | Add a | Subtract a

generatePowerRecord ::
  ( Show (v a), V.Zipper v, V.Walker v, V.Storage v a, 
    BProd a a, BSum a, Ord nty) => 
  SignalRecord v a -> SignalRecord v a
  -> [(PPosIdx nty, [PowerCalc String],[PowerCalc String])] -> PowerRecord nty v a 
generatePowerRecord rec@(SignalRecord time _) eRec idList =
  PowerRecord time (M.fromList $ concat $ map f idList) 
  where f (pposIdx, calcListA, calcListB) =
          [ (pposIdx, S.setType $ foldl g  (S.untype time) calcListA),
            (swap pposIdx, S.setType $ foldl g  (S.untype time) calcListB)]
          where g  _   (Take idx) = getSig rec (SigId idx)
                g  _   (Extra idx) = getSig eRec (SigId idx)
                g  sig (Mult idx) = sig S..* (getSig rec (SigId idx))
                g  sig (Add idx) = sig S..+ (getSig rec (SigId idx))
                g  sig (Subtract idx) = sig S..- (getSig rec (SigId idx))
                swap (PPosIdx n1 n2) = PPosIdx n2 n1
          
-----------------------------------------------------------------------------------
-- Various Class and Instance Definition for the different Sequence Datatypes 

instance QC.Arbitrary nty => QC.Arbitrary (PPosIdx nty) where
   arbitrary = liftM2 PPosIdx QC.arbitrary QC.arbitrary
   shrink (PPosIdx from to) = map (uncurry PPosIdx) $ QC.shrink (from, to)

instance
   (Show (v a), Sample a, V.FromList v, V.Storage v a, QC.Arbitrary nty, Ord nty) =>
      QC.Arbitrary (PowerRecord nty v a) where
   arbitrary = do
      xs <- QC.listOf arbitrarySample
      n <- QC.choose (1,5)
      ppos <- QC.vectorOf n QC.arbitrary
      let vectorSamples =
             HTL.switchR [] (\equalSized _ -> equalSized) $
             HTL.sliceVertical n xs
      return $
         PowerRecord (S.fromList $ Match.take vectorSamples $ iterate (1+) 0) $
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
    Ord a, Fractional a, PrintfArg a) =>
   ToTable (SignalRecord v a) where
   toTable os (ti, SignalRecord time sigs) =
      [Table {
         tableTitle = "SignalRecord - " ++ ti ,
         tableData = tableData t,
         tableFormat = tableFormat t,
         tableSubTitle = ""}]

      where t = tvcat $ S.toTable os ("Time",time) !:
                        concatMap (toTable os . mapFst show) (M.toList sigs)

instance
   (V.Walker v, V.Singleton v, V.FromList v, V.Storage v a, DispStorage1 v,
    Ord a, Fractional a, PrintfArg a, Show nty) =>
   ToTable (PowerRecord nty v a) where
   toTable os (ti, PowerRecord time sigs) =
      [Table {
         tableTitle = "PowerRecord - " ++ ti ,
         tableData = tableData t,
         tableFormat = tableFormat t,
         tableSubTitle = ""}]

      where t = tvcat $ S.toTable os ("Time",time) !:
                        concatMap (toTable os . mapFst show) (M.toList sigs)


------------------------------------
-- RSignal als Transponierte Form


type RSig = (TSigL, PSamp2LL)
type RSamp1 = (TSamp, PSamp1L)
type RSamp = (TSamp, PSamp)

{-
{-# DEPRECATED rhead, rtail "use rviewL instead" #-}
{-# DEPRECATED rlast, rinit "use rviewR instead" #-}

rhead :: RSig -> RSamp1
rhead (t,ps) = (S.head t, S.head ps)

rtail :: RSig -> RSig
rtail (t,ps) = (S.tail t, S.tail ps)

rlast :: RSig -> RSamp1
rlast (t,ps) = (S.last t, S.last ps)

rinit :: RSig -> RSig
rinit (t,ps) = (S.init t, S.init ps)
-}


rviewL :: RSig -> Maybe (RSamp1, RSig)
rviewL (t,ps) =
   liftM2 zipPairs (S.viewL t) (S.viewL ps)

rviewR :: RSig -> Maybe (RSig, RSamp1)
rviewR (t,ps) =
   liftM2 zipPairs (S.viewR t) (S.viewR ps)

zipPairs :: (a,b) -> (c,d) -> ((a,c), (b,d))
zipPairs (a,b) (c,d) = ((a,c), (b,d))

rlen :: RSig -> Int
rlen  (t,ps) = min (S.len t) (S.len ps)

rsingleton :: RSamp1 -> RSig
rsingleton (t,ps) = (S.singleton t, S.singleton ps)



