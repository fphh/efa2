{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,FlexibleContexts,UndecidableInstances, TypeOperators, TypeSynonymInstances#-}


module EFA2.Display.Plot (module EFA2.Display.Plot) where

import Graphics.Gnuplot.Simple
import EFA2.Signal.Signal
import EFA2.Signal.Data
import EFA2.Signal.Vector
import EFA2.Signal.Base
import EFA2.Signal.Typ
import EFA2.Display.DispTyp
import EFA2.Display.DispBase
import EFA2.Signal.SequenceData

import qualified Data.Map as M


class SPlotData s typ c d where 
  sPlotData :: TC s typ (c d) -> [d]
  
instance (FromToList (Data (v1 :> Nil)) Val, DisplayTyp t) => SPlotData Signal t  (Data (v1 :> Nil)) Val where 
  sPlotData x = map (*s) $ stoList x  
    where t = getDisplayType x
          u = getDisplayUnit t
          (UnitScale s) = getUnitScale u
          
          
class Plot a where          
  sigPlot :: a -> IO ()

instance SPlotData Signal t (Data (v1 :> Nil)) Val => Plot (TC Signal t  (Data (v1 :> Nil) Val))  where 
  sigPlot x = do   
     plotList [] (sPlotData x)

instance Plot PowerRecord where   
  sigPlot (PowerRecord time pMap) = plotLists [] (map sPlotData $ M.elems pMap)
  
instance Plot SequPwrRecord where   
  sigPlot (SequData recs) = mapM_ sigPlot recs
  