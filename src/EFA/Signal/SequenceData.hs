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

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Foldable as Fold
import Data.Traversable (Traversable, traverse, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Tuple.HT (mapPair)

import Prelude hiding (unzip, length, filter,zip)


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


zip :: SequData a -> SequData b -> SequData (a, b)
zip (SequData xs) (SequData ys) = SequData $ List.zipWith f xs ys
  where
    f (Section s1 r1 x1) (Section s2 r2 x2) = if s1==s2 || r1 ==r2
                                               then (Section s1 r1 (x1,x2))
                                                    else error("SequenceData zip -- not same section or range")


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

filter2 :: ((a,b) -> Bool) -> (SequData a,SequData b)  -> (SequData a, SequData b)
filter2 f (xs,ys) = unzip $ filter f $ zip xs ys


filterRange :: (Range -> Bool) -> SequData a -> SequData a
filterRange f (SequData xs) =
   SequData $ List.filter (\(Section _ rng _) -> f rng) xs

partition :: (a -> Bool) -> SequData a -> (SequData a, SequData a)
partition f (SequData xs) =
   mapPair (SequData, SequData) $
   ListHT.partition (\(Section _ _ a) -> f a) xs

partition2 :: ((a, b) -> Bool) -> (SequData a,SequData b) -> ((SequData a, SequData b),(SequData a, SequData b))
partition2 f (xs,ys) = (filter2 f (xs,ys), filter2 (not . f) (xs,ys))


{-
-- | Filter Sequence and SequenceData with a Filterfunktion
-- | Allows e.g. to filter Sequ, SeqPwrRecord, SequFlowRecord with FlowState
filterSequWithSequData2 :: ((Section Range,Section a, Section b) -> Bool) ->
                           (SequData Range,SequData a,SequData b) ->
                           (SequData Range,SequData a,SequData b)
filterSequWithSequData2 f (SequData xs, SequData ys, SequData zs) =
  (SequData xsf, SequData ysf, SequData  zsf)
   where (xsf,ysf,zsf) = List.unzip4 $ filter f $ List.zip4 xs ys zs
-}
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


