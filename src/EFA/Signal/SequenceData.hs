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

import qualified Data.List.Match as Match
import qualified Data.List as List
import Control.Applicative (Applicative(pure, (<*>)), liftA, liftA2)
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap, fold)
import Data.Tuple.HT (mapPair)

import Prelude hiding (unzip)


-----------------------------------------------------------------------------------
-- Section and Sequence -- Structures to handle Sequence Information and Data
-- | Section analysis result
type Sequ = SequData Sec
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

unzip :: SequData (a, b) -> (SequData a, SequData b)
unzip (SequData xs) = mapPair (SequData, SequData) $ List.unzip xs


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
   where (xsf,ysf) = List.unzip $ filter f $ zip xs ys

-- | Filter Sequence and SequenceData with a Filterfunktion
-- | Allows e.g. to filter Sequ, SeqPwrRecord and SequFlowRecord
filterSequWithSequData2 :: ((Sec,a,b) -> Bool) -> (Sequ,SequData a,SequData b) -> (Sequ,SequData a,SequData b)
filterSequWithSequData2 f (SequData xs, SequData ys, SequData zs) = (SequData xsf, SequData ysf, SequData zsf )
   where (xsf,ysf,zsf) = unzip3 $ filter f $ zip3 xs ys zs

-- | Filter Sequence and SequenceData with a Filterfunktion
-- | Allows e.g. to filter Sequ, SeqPwrRecord, SequFlowRecord with FlowState
filterSequWithSequData3 :: ((Sec,a,b,c) -> Bool) -> (Sequ,SequData a,SequData b,SequData c) -> (Sequ,SequData a,SequData b,SequData c)
filterSequWithSequData3 f (SequData xs, SequData ys, SequData zs, SequData us) = (SequData xsf, SequData ysf, SequData zsf,SequData usf  )
   where (xsf,ysf,zsf,usf) = List.unzip4 $ filter f $ List.zip4 xs ys zs us

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
      fold $ zipWithSecIdxs (\sec r -> Report.toTable os (show sec, r)) rs

instance ToTable Sec where
   toTable _os (ti, SequData xs) =
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


