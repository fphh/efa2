{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Signal.SequenceData where

import EFA.Graph.Topology (FlowTopology)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Data as D
import qualified EFA.Signal.Vector as V
import EFA.Report.Base (DispStorage1)
import EFA.Signal.Signal
          (TC, Signal, SignalIdx, DTVal, FVal, DTFSig, FFSig, neg)
import qualified EFA.Signal.Signal as Sig (map)
          
import EFA.Signal.Typ (Typ, A, P, T, Tt, UT)
import EFA.Signal.Data (Data, (:>), Nil)
import EFA.Signal.Base (Sign, Val, BSum, DArith0,BProd)

import EFA.Report.Report (ToTable(toTable), Table(..), TableData(..), toDoc, tvcat, autoFormat)
import Text.Printf (PrintfArg)

import qualified Test.QuickCheck as QC
import System.Random (Random)

import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector as VB
import qualified Data.List.HT as HTL
import qualified Data.List as L
-- import qualified Data.String.Utils as Str
import qualified Data.List.Match as Match
import Data.NonEmpty ((!:))
import Data.Ratio (Ratio, (%))
import Data.List (transpose)
import Data.Tuple.HT (mapFst)
import Control.Monad (liftM2)
import Control.Applicative (Applicative(pure, (<*>)), liftA, liftA2)
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap, fold)
import EFA.Utility (checkedLookup)


-----------------------------------------------------------------------------------
-- | Indices for Power Position
data PPosIdx = PPosIdx !Idx.Node !Idx.Node deriving (Show, Eq, Ord)


-----------------------------------------------------------------------------------
-- | Signal Record & Power Record & Flow -- Structure for Handling recorded data

-- | Signal record to contain original time signals
-- data Record = Record TSig (M.Map SigId UTSigL) deriving (Show)

data Record v a =
   Record
      (TC Signal (Typ A T Tt) (Data (v :> Nil) a))
      (M.Map SigId (TC Signal (Typ UT UT UT) (Data (v :> Nil) a)))
   deriving (Show, Eq)

type instance D.Value (Record v a) = a

data SigId = SigId String deriving (Show, Eq, Ord)

data PowerRecord v a =
   PowerRecord
      (TC Signal (Typ A T Tt) (Data (v :> Nil) a))
      (M.Map PPosIdx (TC Signal (Typ A P Tt) (Data (v :> Nil) a)))
   deriving (Show, Eq)

type instance D.Value (PowerRecord v a) = a

-- | Power record to contain power signals assigned to the tree
type ListPowerRecord = PowerRecord [] Val

-- | Power Record to contain Power signals after cutting
type SecPowerRecord = PowerRecord UV.Vector Val


type SequPwrRecord = SequData SecPowerRecord

-- | Flow record to contain flow signals assigned to the tree
data FlRecord a b = FlRecord a (M.Map PPosIdx b) deriving (Show)
type FlowRecord = FlRecord DTFSig FFSig
type FlowValRecord = FlRecord DTVal FVal


{-
-- | Flow record to contain flow signals assigned to the tree
type PPosFlows = PPosData FSig
data FlowRecord = FlowRecord DTSig (PPosData FSig) deriving (Show)

data FlowValRecord = FlowValRecord DTVal (PPosData FVal) deriving (Show)
-}

type SequFlowRecord a = SequData a

-- | Flow record to contain flow signals assigned to the tree
--type SequFlowValRecord = SequData [FlowValRecord]

newtype FlowState = FlowState (M.Map PPosIdx Sign) deriving (Show)
type SequFlowState = SequData FlowState
type SequFlowTops = SequData FlowTopology

-----------------------------------------------------------------------------------
-- Section and Sequence -- Structures to handle Sequence Information and Data
-- | Section analysis result
newtype Sequ = Sequ [Sec] deriving Show
type Sec = (SignalIdx,SignalIdx)


-- | Sequence Vector to Store Section Data
newtype SequData a = SequData [a] deriving (Show, Eq)

type instance D.Value (SequData a) = D.Value a

instance Functor SequData where
   fmap f (SequData xs) = SequData (map f xs)

instance Applicative SequData where
   pure = SequData . repeat
   SequData f <*> SequData x = SequData $ zipWith ($) f x

instance Foldable SequData where
   foldMap = foldMapDefault

instance Traversable SequData where
   sequenceA (SequData xs) = liftA SequData $ sequenceA xs

{-
We could also define a top-level variable for (SequData [Idx.Section 0 ..]),
but it would be memorized and thus causes a space leak.
-}
zipWithSecIdxs :: (Idx.Section -> a -> b) -> SequData a -> SequData b
zipWithSecIdxs f =
   liftA2 f (SequData [Idx.Section 0 ..])

-----------------------------------------------------------------------------------
-- Utility Functions on Sequence Data
         
   
         
-- | PG Diskussion : 
-- |        Sollte Sequ nicht auch SequData verwenden, dann würden die Utility - Funktionen hier auch funktionieren           
-- |        brauchen wir eventuell eine Überstruktur, da das Filtern der Sequenz und der Sequenzdaten irgendwie zusammen         
-- |        gehört
         
-- | Get Number of Sections after cutting 
sequLength :: Sequ -> Int
sequLength (Sequ xs) = length xs

-- | Filter Sequence and SequenceData with a Filterfunktion
-- | Allows to e.q. filter Sequ and SequPwrRecord
filterSequWithSequData :: ((Sec,a) -> Bool) -> (Sequ,SequData a) ->   (Sequ,SequData a)
filterSequWithSequData f (Sequ xs, SequData ys) = (Sequ xsf, SequData ysf)
   where (xsf,ysf) = unzip $ filter f $ zip xs ys  

-- | Filter Sequence and SequenceData with a Filterfunktion
-- | Allows e.g. to filter Sequ, SeqPwrRecord and SequFlowRecord         
filterSequWithSequData2 :: ((Sec,a,b) -> Bool) -> (Sequ,SequData a,SequData b) -> (Sequ,SequData a,SequData b)
filterSequWithSequData2 f (Sequ xs, SequData ys, SequData zs) = (Sequ xsf, SequData ysf, SequData zsf )
   where (xsf,ysf,zsf) = unzip3 $ filter f $ zip3 xs ys zs  

-----------------------------------------------------------------------------------
-- Utility Functions on Records 
         

getTime :: Record v a ->  TC Signal (Typ A T Tt) (Data (v :> Nil) a) 
getTime (Record time _) = time

getPTime :: PowerRecord v a ->  TC Signal (Typ A T Tt) (Data (v :> Nil) a) 
getPTime (PowerRecord time _) = time

getSig :: Show (v a) => Record v a -> SigId -> TC Signal (Typ UT UT UT) (Data (v :> Nil) a)   
getSig (Record _ sigMap) sigId = checkedLookup sigMap sigId

getPSig :: Show (v a) => PowerRecord v a -> PPosIdx -> TC Signal (Typ A P Tt) (Data (v :> Nil) a)   
getPSig (PowerRecord _ pMap) idx = checkedLookup pMap idx

-- | Use carefully -- removes signal jitter around zero 
removeZeroNoise :: (V.Walker v, V.Storage v a, Ord a, Num a) => PowerRecord v a -> a -> PowerRecord v a        
removeZeroNoise (PowerRecord time pMap) threshold = PowerRecord time (M.map f pMap)
  where f sig = Sig.map g sig
        g x | abs x < threshold = 0 
            | otherwise = x

-- | Generate a new Record with selected signals
selectRecord :: Show (v a) => Record v a -> [SigId] ->  Record v a
selectRecord rec@(Record time _ ) xs = Record time  (M.fromList $ zip xs (map f xs))
  where f x = getSig rec x
        
        
-- | Split Record in even Junks                          
splitRecord ::  (Num a, Ord a, V.Walker v, V.Storage v a, BSum a) => Record v a -> Int -> [Record v a]                          
splitRecord (Record time pMap) n  = recList
  where (recList, _) = f ([],sortSigList $ M.toList pMap)
        f (rs, []) = (rs,[])        
        f (rs, xs) = f (rs ++ [Record time (M.fromList $ take n xs)], drop n xs)
        

sortSigList ::  (Num a,
                      Ord a,
                      V.Walker v,
                      V.Storage v a,
                      BSum a) =>
                [ (SigId,TC Signal (Typ UT UT UT) (Data (v :> Nil) a))] ->  [(SigId, TC Signal (Typ UT UT UT) (Data (v :> Nil) a))]
sortSigList  sigList = L.sortBy g  sigList
  where g (_,x) (_,y) = compare (S.sigSum x) (S.sigSum y) 

-- | Split PowerRecord in even Junks                          
splitPowerRecord ::  (Num a, Ord a, V.Walker v, V.Storage v a, BSum a) => PowerRecord v a -> Int -> [PowerRecord v a]                          
splitPowerRecord (PowerRecord time pMap) n  = recList
  where (recList, _) = f ([],M.toList pMap)
        f (rs, []) = (rs,[])        
        f (rs, xs) = f (rs ++ [PowerRecord time (M.fromList $ take n xs)], drop n xs)
 

-----------------------------------------------------------------------------------
-- Functions to support Signal Selection
 
-- | List of Operations for pre-processing signals
-- | TODO - a gadt construction would be awesome to provide parameters        
data SignalOps = Negate | RemoveZeroNoise | Filter | Offset          
        
-- | create a Record of selected, and sign corrected signals
extractLogSignals ::  (V.Walker v,
                      V.Storage v a,
                      DArith0 a, 
                      Show (v a)) => 
                      Record v a -> [(SigId, [SignalOps])] -> Record  v a        
extractLogSignals rec@(Record time _) idList = Record time (M.fromList $ map f idList)
  where f (SigId sigId,opList) = (SigId sigId, foldl g  signal opList)
          where -- newId = SigId $ Str.replace "." "_" sigId              
                signal = getSig rec (SigId sigId)              
                g  sig Negate = neg sig
                g  sig _ = sig

data PowerCalc a = Take a | Extra a | Mult a | Add a | Subtract a

generatePowerRecord :: (Show (v a),
                        V.Zipper v,
                      V.Walker v,
                      V.Storage v a,
                      BProd a a, 
                      BSum a) => 
                       Record v a -> Record v a -> [(PPosIdx, [PowerCalc String],[PowerCalc String])] -> PowerRecord v a 
generatePowerRecord rec@(Record time _) eRec idList = PowerRecord time (M.fromList $ concat $ map f idList) 
  where f (pposIdx, calcListA, calcListB) = [(pposIdx, S.setType $ foldl g  (S.untype time) calcListA),
                                 (swap pposIdx, S.setType $ foldl g  (S.untype time) calcListB)]
          where 
                g  _   (Take idx) = getSig rec (SigId idx)
                g  _   (Extra idx) = getSig eRec (SigId idx)
                g  sig (Mult idx) = sig S..* (getSig rec (SigId idx))
                g  sig (Add idx) = sig S..+ (getSig rec (SigId idx))
                g  sig (Subtract idx) = sig S..- (getSig rec (SigId idx))
                swap (PPosIdx n1 n2) = PPosIdx n2 n1
          
-----------------------------------------------------------------------------------
-- Various Class and Instance Definition for the different Sequence Datatypes 

instance QC.Arbitrary PPosIdx where
   arbitrary = liftM2 PPosIdx QC.arbitrary QC.arbitrary
   shrink (PPosIdx from to) = map (uncurry PPosIdx) $ QC.shrink (from, to)

instance
   (Show (v a), Sample a, V.FromList v, V.Storage v a) =>
      QC.Arbitrary (PowerRecord v a) where
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

{-
instance ToTable Record where
   toTable os (ti, Record time sigs) =
      [Table {
         tableTitle = "Record - " ++ ti ,
         tableData = tableData t,
         tableFormat = tableFormat t,
         tableSubTitle = ""}]

      where t = tvcat $ S.toTable os ("Time",time) !:
                        concatMap (toTable os . mapFst show) (M.toList sigs)
-}

instance
   (V.Walker v, V.Singleton v, V.FromList v, V.Storage v a, DispStorage1 v,
    Ord a, Fractional a, PrintfArg a) =>
   ToTable (Record v a) where
   toTable os (ti, Record time sigs) =
      [Table {
         tableTitle = "Record - " ++ ti ,
         tableData = tableData t,
         tableFormat = tableFormat t,
         tableSubTitle = ""}]

      where t = tvcat $ S.toTable os ("Time",time) !:
                        concatMap (toTable os . mapFst show) (M.toList sigs)

instance
   (V.Walker v, V.Singleton v, V.FromList v, V.Storage v a, DispStorage1 v,
    Ord a, Fractional a, PrintfArg a) =>
   ToTable (PowerRecord v a) where
   toTable os (ti, PowerRecord time sigs) =
      [Table {
         tableTitle = "PowerRecord - " ++ ti ,
         tableData = tableData t,
         tableFormat = tableFormat t,
         tableSubTitle = ""}]

      where t = tvcat $ S.toTable os ("Time",time) !:
                        concatMap (toTable os . mapFst show) (M.toList sigs)

instance (ToTable a) => ToTable (SequData a) where
   toTable os (_ti, rs) =
      fold $ zipWithSecIdxs (\sec r -> toTable os (show sec, r)) rs


instance ToTable Sequ where
   toTable _os (ti, Sequ xs) =
      [Table {
         tableTitle = "Sequence: " ++ ti,
         tableData = td,
         tableFormat = autoFormat td,
         tableSubTitle = ""}]
      where
         td = TableData {
                 tableBody = [map f xs],
                 titleRow  = [
                    map (toDoc id) $
                       "Section:" :
                       map (\(Idx.Section x) -> "Sec" ++ show x)
                          (Match.take xs [Idx.Section 0 ..])],
                 titleCols = [[toDoc id "Index"]],
                 endCols  = []
              }

         -- f :: Sec -> TableData
         f (i1, i2) = toDoc id $ show i1 ++ " - " ++ show i2


