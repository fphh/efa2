{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module EFA2.Signal.SequenceData where

-- import EFA2.Interpreter.Arith
import EFA2.Topology.TopologyData (FlowTopology)

import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Data as D
import qualified EFA2.Signal.Vector as V
import EFA2.Display.DispBase (DispStorage1)
import EFA2.Signal.Signal
          (TC, Signal, SignalIdx, DTVal, FVal, TSig, DTFSig, FFSig, UTSigL)
import EFA2.Signal.Typ (Typ, A, P, T, Tt)
import EFA2.Signal.Data (Data, (:>), Nil)
import EFA2.Signal.Base (Sign, Val)

import EFA2.Display.Report (ToTable(toTable), Table(..), TableData(..), toDoc, tvcat, autoFormat)
import Text.Printf (PrintfArg)

import qualified Test.QuickCheck as QC
import System.Random (Random)

import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV
import qualified Data.List.HT as HTL
import qualified Data.List.Match as Match
import Data.NonEmpty ((!:))
import Data.Ratio (Ratio, (%))
import Data.List (transpose)
import Data.Tuple.HT (mapFst)
import Control.Monad (liftM2)

-----------------------------------------------------------------------------------
-- | Indices for Record, Section and Power Position
newtype RecIdx = RecIdx Int deriving (Show, Eq, Ord) -- dataset Index
newtype SecIdx = SecIdx Int deriving (Show, Eq, Ord, Enum)
data PPosIdx = PPosIdx !Int !Int deriving (Show, Eq, Ord)

-----------------------------------------------------------------------------------
-- | Signal Record & Power Record & Flow -- Structure for Handling recorded data

-- | Signal record to contain original time signals
data Record = Record TSig (M.Map SigId UTSigL) deriving (Show)
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
data FlRecord a b = FlRecord a (M.Map PPosIdx b)
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


instance QC.Arbitrary PPosIdx where
   arbitrary = liftM2 PPosIdx (QC.choose (0,10)) (QC.choose (0,10))
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


instance ToTable Record where
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
   toTable os (_ti, SequData rs) =
      concatMap (toTable os) (zip (map f [0..]) rs)
         where f :: Int -> String
               f idx = "Section " ++ show idx


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
                 titleRow  = [[toDoc id "Section:"]++map (\x -> toDoc id ("Sec" ++ show x)) [0..(length xs -1)]],
                 titleCols = [[toDoc id "Index"]],
                 endCols  = []
              }

         -- f :: Sec -> TableData
         f (i1, i2) = toDoc id $ show i1 ++ " - " ++ show i2
