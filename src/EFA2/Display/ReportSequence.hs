{-# LANGUAGE FlexibleInstances, GADTs #-}

module EFA2.Display.ReportSequence (module EFA2.Display.ReportSequence) where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV
import Data.Monoid
import qualified Text.PrettyPrint.HughesPJ as PP
import qualified Text.Show.Pretty as SP

import EFA2.Signal.Signal
import EFA2.Signal.Data
import EFA2.Display.DispSignal

import System.IO

-- import EFA.Graph.GraphData
-- import EFA.Signal.SignalData
-- import EFA2.Utils.Utils
import EFA2.Signal.SequenceData
import EFA2.Display.DispSignal

{-
type Title = String
type ColumnTitle = String


-- | Table Data Type --------------------------------------------
data Table = Table Title [ColumnTitle] [Row]
           | TableDoc [Table] deriving (Show)

instance Monoid Table  where
         mempty = TableDoc []
         mappend (TableDoc as) (TableDoc bs) = TableDoc (as ++ bs)
         mappend (TableDoc as) t@(Table _ _ _) = TableDoc (as ++ [t])
         mappend t@(Table _ _ _) (TableDoc bs) = TableDoc (t:bs)
         mappend t1@(Table _ _ _) t2@(Table _ _ _) = TableDoc [t1, t2]

data Row = VectorRow Title (UV.Vector Double)
         | ListRow Title [Double] 
         | SigRange Title String deriving (Show)
                                           

-- | Formatting Functions  --------------------------------------------
formatDocHor :: Table -> String
formatDocHor (TableDoc ts) = PP.render $ PP.vcat rows'
  where rows = L.transpose $ map formatTable ts
        rows' = map f (zip tabBegins rows)
        f (x, t) = PP.nest x (foldl (PP.$$) PP.empty t)
        tabBegins = 0:(map (+60) tabBegins)

formatDocVer :: Table -> String
formatDocVer (TableDoc ts) = PP.render $ PP.vcat (L.intersperse PP.space (map PP.vcat rows))
  where rows = map formatTable ts

formatTable :: Table -> [PP.Doc]
formatTable (Table ti _ rs) = map formatRow rs

formatRow :: Row -> PP.Doc
formatRow (VectorRow ti vec) = (PP.nest 0 (PP.text ti)) PP.$$ (foldl (PP.$$) PP.empty (map f (zip colBegins lst)))
  where lst = map PP.double (UV.toList vec)
        f (x, t) = PP.nest x t
        colBegins = 10:(map (+22) colBegins)

formatRow (SigRange ti vec) = (PP.nest 0 (PP.text ti)) PP.$$ (foldl (PP.$$) PP.empty (map f (zip colBegins lst)))
  where lst = map PP.text [vec]
        f (x, t) = PP.nest x t
        colBegins = 5:(map (+22) colBegins)

--formatRow (ListRow ti xs) = (PP.nest 0 (PP.text ti)) PP.$$ (foldl (PP.$$) PP.empty (map f (zip colBegins lst)))

formatRow (ListRow ti xs) = (foldl (PP.$$) (PP.nest 0 (PP.text ti)) (map f (zip colBegins lst)))
  where lst = map PP.double xs
        f (x, t) = PP.nest x t
        colBegins = 10:(map (+10) colBegins)


-- | OutPut Functions  --------------------------------------------
-- | TODO: write formatDocHor versions of this functions.
printTable :: (ToTable a) => Handle -> a -> IO ()
printTable h = hPutStrLn h . formatDocHor . toTable

printTableToScreen :: (ToTable a) => a -> IO ()
printTableToScreen = printTable stdout

printTableToFile :: (ToTable a) => FilePath -> a -> IO ()
printTableToFile fileName t = do
  h <- openFile fileName WriteMode
  printTable h t
  hClose h


-- | To Table Class to defining generation of Documents  --------------------------------------------
class ToTable a where
      toTable :: a -> Table
{-
instance ToTable (TC Signal typ (Data ([] :> Nil) Val) where
         toTable x = TableDoc [Table "Signal" [] rows]
                  where rows =  


instance ToTable PowerRecord where
         toTable (PowerRecord time sigs) = TableDoc [Table "Signals" [] rows]
           where rows = map (\(x, y) ->  SigRange (show x) (sdisp y)) $ M.toList sigs
-} 



instance ToTable PowerRecord where
         toTable (PowerRecord time sigs) = TableDoc [Table "Signals" [] rows]
           where rows = map (\(x, y) ->  ListRow (show x) (S.toList y)) $ M.toList sigs
-}  