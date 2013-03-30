{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}


module EFA.Signal.SequenceData where

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Data as D
import qualified EFA.Signal.Vector as V
import qualified EFA.Signal.Record as Record

import qualified EFA.Report.Report as Report
import EFA.Report.Report (Table(..), TableData(..), toDoc, autoFormat)
import EFA.Report.Typ (TDisp)
import EFA.Report.Base (DispStorage1)

import Text.Printf (PrintfArg)

import EFA.Signal.Signal(SignalIdx)

import qualified Data.List as List
import qualified Data.Foldable as Fold
import Data.Traversable (Traversable, traverse, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Tuple.HT (mapPair)

import Prelude hiding (unzip, length, filter)


-----------------------------------------------------------------------------------
-- Section and Sequence -- Structures to handle Sequence Information and Data
-- | Section analysis result
type Sequ = SequData Range
type Range = (SignalIdx, SignalIdx)

{- |
Sequence Vector to Store Section Data

It could also be a Map, but we need the laziness of the list type.
-}
newtype SequData a = SequData [Section a] deriving (Show, Eq)

data Section a = Section Idx.Section Range a
   deriving (Eq, Show)

type instance D.Value (SequData a) = D.Value a

instance Functor SequData where
   fmap f (SequData xs) = SequData (map (fmap f) xs)

instance Foldable SequData where
   foldMap = foldMapDefault

instance Traversable SequData where
   sequenceA (SequData xs) = fmap SequData $ traverse sequenceA xs


instance Functor Section where
   fmap f (Section s rng a) = Section s rng (f a)

instance Foldable Section where
   foldMap = foldMapDefault

instance Traversable Section where
   sequenceA (Section s rng a) = fmap (Section s rng) a


fromList :: [a] -> SequData a
fromList =
   SequData .
   zipWith
      (\s -> Section (Idx.Section s) (case fromIntegral s of r -> (r,r)))
      [0 ..]

fromRangeList :: [(Range, a)] -> SequData a
fromRangeList =
   SequData . zipWith (uncurry . Section) [Idx.Section 0 ..]

fromLengthList :: [(Int, a)] -> SequData a
fromLengthList =
   fromRangeList . snd .
   List.mapAccumL (\time (len, x) -> (time+len-1, ((time, time+len-1), x))) 0

unzip :: SequData (a, b) -> (SequData a, SequData b)
unzip (SequData xs) =
   mapPair (SequData, SequData) $ List.unzip $
   map (\x -> (fmap fst x, fmap snd x)) xs

mapWithSection :: (Idx.Section -> a -> b) -> SequData a -> SequData b
mapWithSection f (SequData xs) =
   SequData $ map (\(Section s rng a) -> Section s rng $ f s a) xs

mapWithSectionRange ::
   (Idx.Section -> Range -> a -> b) -> SequData a -> SequData b
mapWithSectionRange f (SequData xs) =
   SequData $ map (\(Section s rng a) -> Section s rng $ f s rng a) xs


-- | Get Number of Sections after cutting
length :: SequData a -> Int
length (SequData xs) = List.length xs

-- | Filter Sequence and SequenceData with a filter function
-- | Allows to e.g. filter Sequ and SequPwrRecord
filter :: (a -> Bool) -> SequData a -> SequData a
filter f (SequData xs) =
   SequData $ List.filter (\(Section _ _ a) -> f a) xs

filterRange :: (Range -> Bool) -> SequData a -> SequData a
filterRange f (SequData xs) =
   SequData $ List.filter (\(Section _ rng _) -> f rng) xs


class ToTable a where
   toTable :: Report.ROpts -> (String, SequData a) -> [Table]

instance ToTable a => Report.ToTable (SequData a) where
   toTable = toTable


instance
   (V.Walker v, V.Singleton v, V.FromList v, V.Storage v a, DispStorage1 v,
    Ord a, Fractional a, PrintfArg a, Show id,
    S.DispApp s, TDisp t1, TDisp t2) =>
      ToTable (Record.Record s t1 t2 id v a) where
   toTable os (_ti, rs) =
      Fold.fold $ mapWithSection (\ sec r -> Report.toTable os (show sec, r)) rs

instance ToTable Range where
   toTable _os (ti, xs) =
      [Table {
         tableTitle = "Sequence: " ++ ti,
         tableData = td,
         tableFormat = autoFormat td,
         tableSubTitle = ""}]
      where
         td = TableData {
                 tableBody = [Fold.toList $ fmap f xs],
                 titleRow  = [
                    map (toDoc id) $
                       "Section:" :
                       (Fold.toList $
                        mapWithSection (\(Idx.Section x) _ -> "Sec" ++ show x) xs)],
                 titleCols = [[toDoc id "Index"]],
                 endCols  = []
              }

         -- f :: Range -> TableData
         f (i1, i2) = toDoc id $ show i1 ++ " - " ++ show i2


