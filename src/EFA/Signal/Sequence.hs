{-# LANGUAGE TypeFamilies #-}

module EFA.Signal.Sequence where

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Data as D
import qualified EFA.Signal.Vector as V
import qualified EFA.Signal.Record as Record

import qualified EFA.Report.Report as Report
import EFA.Report.Report (Table, toDoc, autoFormat)
import EFA.Report.Typ (TDisp)
import EFA.Report.Base (DispStorage1)

import Text.Printf (PrintfArg)

import EFA.Signal.Signal(SignalIdx)

import qualified Data.List.HT as ListHT
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Traversable (Traversable, traverse, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Tuple.HT (mapPair)

import Prelude hiding (unzip, length, filter,zip)


-----------------------------------------------------------------------------------
-- Section and Sequence -- Structures to handle Sequence Information and Data
-- | Section analysis result

{- |
Sequence Vector to Store Section Data

It could also be a Map, but we need the laziness of the list type.
-}
newtype List a = List [Section a] deriving (Show, Eq)

data Section a = Section Idx.Section Range a
   deriving (Eq, Show)

data Range = Range SignalIdx SignalIdx
   deriving (Eq, Show)

rangeSingleton :: SignalIdx -> Range
rangeSingleton n = Range n n

rangeIsSingleton :: Range -> Bool
rangeIsSingleton (Range from to) = from==to

type instance D.Value (List a) = D.Value a

instance Functor List where
   fmap f (List xs) = List (map (fmap f) xs)

instance Foldable List where
   foldMap = foldMapDefault

instance Traversable List where
   sequenceA (List xs) = fmap List $ traverse sequenceA xs


instance Functor Section where
   fmap f (Section s rng a) = Section s rng (f a)

instance Foldable Section where
   foldMap = foldMapDefault

instance Traversable Section where
   sequenceA (Section s rng a) = fmap (Section s rng) a


fromList :: [a] -> List a
fromList =
   List .
   zipWith
      (\s ->
         Section (Idx.Section s)
            (rangeSingleton $ S.SignalIdx $ fromIntegral s))
      [0 ..]

fromRangeList :: [(Range, a)] -> List a
fromRangeList =
   List . zipWith (uncurry . Section) [Idx.Section 0 ..]

fromLengthList :: [(Int, a)] -> List a
fromLengthList =
   fromRangeList . snd .
   List.mapAccumL
      (\(S.SignalIdx idx) (len, x) ->
         (S.SignalIdx $ idx+len-1,
          (Range (S.SignalIdx idx) (S.SignalIdx $ idx+len-1), x)))
      (S.SignalIdx 0)


unzip :: List (a, b) -> (List a, List b)
unzip (List xs) =
   mapPair (List, List) $ List.unzip $
   map (\x -> (fmap fst x, fmap snd x)) xs


type Map a = Map.Map Idx.Section (Range, a)

lookup :: Idx.Section -> Map a -> Maybe a
lookup sec = fmap snd . Map.lookup sec

toMap :: List a -> Map a
toMap (List xs) =
   Map.fromListWith (error "duplicate section") $
   map (\(Section sec rng a) -> (sec, (rng, a))) xs

fromMap :: Map a -> List a
fromMap =
   List .
   map (\(sec, (rng, a)) -> Section sec rng a) .
   Map.toList


mapWithSection :: (Idx.Section -> a -> b) -> List a -> List b
mapWithSection f (List xs) =
   List $ map (\(Section s rng a) -> Section s rng $ f s a) xs

mapWithSectionRange ::
   (Idx.Section -> Range -> a -> b) -> List a -> List b
mapWithSectionRange f (List xs) =
   List $ map (\(Section s rng a) -> Section s rng $ f s rng a) xs


-- | Get Number of Sections after cutting
length :: List a -> Int
length (List xs) = List.length xs

-- | Filter Sequence and SequenceData with a filter function
-- | Allows to e.g. filter Sequ and SequPwrRecord
filter :: (a -> Bool) -> List a -> List a
filter f (List xs) =
   List $ List.filter (\(Section _ _ a) -> f a) xs


filterRange :: (Range -> Bool) -> List a -> List a
filterRange f (List xs) =
   List $ List.filter (\(Section _ rng _) -> f rng) xs

partition :: (a -> Bool) -> List a -> (List a, List a)
partition f (List xs) =
   mapPair (List, List) $
   ListHT.partition (\(Section _ _ a) -> f a) xs


class ToTable a where
   toTable :: Report.ROpts -> (String, List a) -> [Table]

instance ToTable a => Report.ToTable (List a) where
   toTable = toTable


instance
   (V.Walker v,
    V.Singleton v,
    V.FromList v,
    V.Storage v d2,
    DispStorage1 v,
    Ord d2,
    Fractional d2,
    PrintfArg d2,
    Fractional d1,
    Ord d1,
    V.Storage v d1,
    PrintfArg d1,
    Show id,
    S.DispApp s1,
    S.DispApp s2,
    TDisp t1,
    TDisp t2) =>
      ToTable (Record.Record s1 s2 t1 t2 id v d1 d2) where
   toTable os (_ti, rs) =
      Fold.fold $ mapWithSection (\ sec r -> Report.toTable os (show sec, r)) rs

instance ToTable Range where
   toTable _os (ti, xs) =
      [Report.Table {
         Report.tableTitle = "Sequence: " ++ ti,
         Report.tableData = td,
         Report.tableFormat = autoFormat td,
         Report.tableSubTitle = ""}]
      where
         td = Report.TableData {
                 Report.tableBody = [Fold.toList $ fmap f xs],
                 Report.titleRow  = [
                    map (toDoc id) $
                       "Section:" :
                       (Fold.toList $
                        mapWithSection (\(Idx.Section x) _ -> "Sec" ++ show x) xs)],
                 Report.titleCols = [[toDoc id "Index"]],
                 Report.endCols  = []
              }

         -- f :: Range -> TableData
         f (Range i1 i2) = toDoc id $ show i1 ++ " - " ++ show i2


{-# DEPRECATED reIndex "pg: new Index type required which shows the reIndexing" #-}

reIndex :: [Int] -> List a -> List a
reIndex xs (List ys) = List (zipWith f xs ys)
  where f newIdx (Section (Idx.Section _) range a) = Section (Idx.Section $ fromIntegral newIdx) range a
