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

import Prelude hiding (unzip)


-----------------------------------------------------------------------------------
-- Section and Sequence -- Structures to handle Sequence Information and Data
-- | Section analysis result
type Sequ = SequData Sec
type Sec = (SignalIdx,SignalIdx)

{- |
Sequence Vector to Store Section Data

It could also be a Map, but we need the laziness of the list type.
-}
newtype SequData a = SequData [Section a] deriving (Show, Eq)

data Section a = Section Idx.Section a
   deriving (Eq, Show)

type instance D.Value (SequData a) = D.Value a

instance Functor SequData where
   fmap f (SequData xs) = SequData (map (fmap f) xs)

instance Foldable SequData where
   foldMap = foldMapDefault

instance Traversable SequData where
   sequenceA (SequData xs) = fmap SequData $ traverse sequenceA xs


instance Functor Section where
   fmap f (Section s a) = Section s (f a)

instance Foldable Section where
   foldMap = foldMapDefault

instance Traversable Section where
   sequenceA (Section s a) = fmap (Section s) a


fromList :: [a] -> SequData a
fromList = SequData . zipWith Section [Idx.Section 0 ..]

unzip :: SequData (a, b) -> (SequData a, SequData b)
unzip (SequData xs) =
   mapPair (SequData, SequData) $ List.unzip $
   map (\x -> (fmap fst x, fmap snd x)) xs

mapWithSection :: (Idx.Section -> a -> b) -> SequData a -> SequData b
mapWithSection f (SequData xs) =
   SequData $ map (\(Section s a) -> Section s $ f s a) xs


-----------------------------------------------------------------------------------
-- Utility Functions on Sequence Data

-- | PG Diskussion :
-- |        Sollte Sequ nicht auch SequData verwenden, dann würden die Utility - Funktionen hier auch funktionieren
-- |        brauchen wir eventuell eine Überstruktur, da das Filtern der Sequenz und der Sequenzdaten irgendwie zusammen
-- |        gehört

-- | Get Number of Sections after cutting
sequLength :: Sequ -> Int
sequLength (SequData xs) = length xs

-- | Filter Sequence and SequenceData with a filter function
-- | Allows to e.q. filter Sequ and SequPwrRecord
filterSequWithSequData :: ((Sec,a) -> Bool) -> (Sequ,SequData a) ->   (Sequ,SequData a)
filterSequWithSequData f (SequData xs, SequData ys) = (SequData xsf, SequData ysf)
   where (xsf,ysf) = List.unzip $ filter (\(Section _si i, Section _s a) -> f (i,a)) $ zip xs ys

-- | Filter Sequence and SequenceData with a filter function
-- | Allows e.g. to filter Sequ, SeqPwrRecord and SequFlowRecord
filterSequWithSequData2 :: ((Sec,a,b) -> Bool) -> (Sequ,SequData a,SequData b) -> (Sequ,SequData a,SequData b)
filterSequWithSequData2 f (SequData xs, SequData ys, SequData zs) = (SequData xsf, SequData ysf, SequData zsf )
   where (xsf,ysf,zsf) = unzip3 $ filter (\(Section _si i, Section _sa a, Section _sb b) -> f (i,a,b)) $ zip3 xs ys zs


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

instance ToTable Sec where
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

         -- f :: Sec -> TableData
         f (i1, i2) = toDoc id $ show i1 ++ " - " ++ show i2


