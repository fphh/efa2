{-# LANGUAGE FlexibleInstances, GADTs, TypeOperators,FlexibleContexts #-}

module EFA2.Display.Report (module EFA2.Display.Report) where

import qualified Data.List as L
import Data.Monoid
import qualified Text.PrettyPrint as PP
-- import qualified Text.Show.Pretty as SP

import EFA2.Signal.Signal
import EFA2.Signal.Data
import EFA2.Signal.Vector

import EFA2.Signal.Base

import EFA2.Display.DispSignal
import EFA2.Display.DispBase
import EFA2.Display.DispTyp


import EFA2.Signal.SequenceData
import EFA2.Display.DispSignal
import EFA2.Utils.Utils

import System.IO

-- | Table with Table Format and Table Data
type Table = (TableFormat, TableData) 

-- | Table Format
type TableFormat = (ColFormat, RowFormat) 
-- col-Format
data ColFormat = ColFormat [(Width,Align)] deriving Show
type Width = Int 
data Align = HLeft | HMid | HRight deriving Show
-- row-Format
data RowFormat = RowFormat [Rows]  deriving Show
type Rows = Int -- Nr of Rows to be left free before

-- | Table-Data including string length
data TableData = TableData [[(Length,PP.Doc)]] deriving Show
type Length = Int

-- | Generate doc from Table
makeTable :: Table -> PP.Doc
makeTable  ((cf,rf),TableData t) = makeCol rf $ map (makeRow cf) t 
    
-- | Generate Table Row     
makeRow :: ColFormat -> [(Length,PP.Doc)] -> PP.Doc    
makeRow (ColFormat cf) cs = PP.hcat (zipWith makeCell cf cs)  

-- | Generate Table Cell
makeCell :: (Width,Align) -> (Length,PP.Doc) -> PP.Doc
makeCell (w,HLeft) (l,c) = PP.hcat (replicate (w-l) PP.space ++[c])                        
makeCell (w,HRight) (l,c) = PP.hcat ([c]++replicate (w-l) PP.space)
--formatCell (w,HMid) (l,c) = PP.hcat ([c]++replicate (w-l) PP.space) where h = (w-l)/2
        
-- | Generate Table Column                            
makeCol :: RowFormat -> [PP.Doc] -> PP.Doc 
makeCol rf rs = PP.vcat rs

-- | To Table Class to defining generation of Documents  --------------------------------------------
class ToTable a where
      toTable :: a -> Table

instance ToTable [[Double]] where
      toTable xs = (tf,td)
        where td = TableData $ map (map (toDoc show)) xs
              tf = autoFormat td

instance (DeltaDisp t, DisplayTyp t, PartDisp t, VSingleton v Double) => ToTable (TC s t (Data (v :> Nil) Val)) where
      toTable x = (tf,td)
        where td = TableData $ map (map (toDoc id)) [xs]
              xs = ["Sig1",tdisp x,disp f s min, " - ",disp f s max, udisp x]
              t = getDisplayType x
              u = getDisplayUnit t              
              s = getUnitScale u 
              f = getDisplayFormat dispLength t u
              max = fromScalar $ smaximum x :: Double
              min = fromScalar $ sminimum x :: Double
              tf = autoFormat td

dispRange x y t = disp f s x ++ " - " ++ disp f s y ++ " " ++ show u
           where u = getDisplayUnit t
                 s = getUnitScale u
                 f = getDisplayFormat dispLength t u

-- | convert raw data to doc elements, using given function 
toDoc :: (a->String) -> a -> (Length,PP.Doc)
toDoc f xs = (length $ f xs, PP.text $ f xs) 

-- | generate Auto Format from Table data
autoFormat :: TableData -> TableFormat
autoFormat (TableData td) = (ColFormat $ zip cf (repeat HLeft), RowFormat rf)
  where                      
    cf = map f $ L.transpose td
    f col = (maximum $ map fst col)+2
    rf = replicate (length td) 0
    

-- | OutPut Functions  --------------------------------------------
-- | TODO: write formatDocHor versions of this functions.
report :: (ToTable a) => a -> IO ()
report = putStrLn . PP.render . makeTable  . toTable 
  