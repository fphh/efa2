{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,FlexibleContexts,UndecidableInstances, TypeOperators, TypeSynonymInstances#-}


module EFA2.Display.Plot (module EFA2.Display.Plot) where

import Graphics.Gnuplot.Simple

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.X11 as X11
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle


import EFA2.Signal.Signal
import EFA2.Signal.Data
import EFA2.Signal.Vector
import EFA2.Signal.Base
import EFA2.Signal.Typ
import EFA2.Display.DispTyp
import EFA2.Display.DispBase
import EFA2.Signal.SequenceData

import qualified Data.Map as M

-- | Get Signal Plot Data (Unit Conversion)  ---------------------------------------------------------------

class SPlotData s typ c d where 
  sPlotData :: TC s typ (c d) -> [d]
  
instance (DFromList (Data (v1 :> Nil)) Val, DisplayTyp t) => SPlotData sig t  (Data (v1 :> Nil)) Val where 
  sPlotData x = map (*s) $ stoList x  
    where t = getDisplayType x
          u = getDisplayUnit t
          (UnitScale s) = getUnitScale u


-- | Simple Signal Plotting -- without time axis --------------------------------------------------------------

class Plot a where          
  sigPlot :: a -> IO ()

instance SPlotData Signal t (Data (v1 :> Nil)) Val => Plot (TC Signal t  (Data (v1 :> Nil) Val))  where 
  sigPlot x = plotList [] (sPlotData x)

instance (DisplayTyp t, VFromList v1 Double, VFromList v2 (TC Signal t (Data (v1 :> (Nil' :> Nil')) Val)),
                      VWalker v2 (v1 Val) (TC Signal t (Data (v1 :> (Nil' :> Nil')) Val))) => Plot (TC Signal t  (Data (v2 :> v1 :> Nil) Val))  where 
  sigPlot x = mapM_ sigPlot $ toSigList x  

instance SPlotData FSignal t (Data (v1 :> Nil)) Val => Plot (TC FSignal t  (Data (v1 :> Nil) Val))  where 
  sigPlot x = plotList [] (sPlotData x)

instance Plot (TC s0 TestRow (x (UVec Val))) where
  sigPlot x = undefined

instance SPlotData TestRow t (Data (v1 :> Nil)) Val => Plot (TC TestRow t  (Data (v1 :> Nil) Val))  where 
  sigPlot x = plotList [] (sPlotData x)

instance SPlotData TestRow t (Data (v1 :> Nil)) Val => Plot (TC TestRow t  (Data (v2 :> v1 :> Nil) Val))  where 
  sigPlot x = undefined -- plotList [] (sPlotData x)



-- | Plotting Signals against each other --------------------------------------------------------------

class XYPlot a b where
  xyplot :: a -> b -> IO ()
  
instance (DisplayTyp t, VFromList v1 Double) => XYPlot (TC Signal t (Data (v1 :> Nil) Val)) (TC Signal t (Data (v1 :> Nil) Val)) where 
  xyplot x y = plotPath [LineStyle 1 [PointSize 2]] (zip (sPlotData x) (sPlotData y))



-- | Plotting Records ---------------------------------------------------------------

-- | Line Style
rPlotStyle legend =  (PlotStyle {plotType = LinesPoints, lineSpec = CustomStyle [LineTitle legend,LineWidth 2, PointType 7, PointSize 1.5]})

-- | Plot Attributes
rPlotAttrs name = [Title ("PowerRecord: " ++ name), 
                   Grid $ Just [], 
                   XLabel ("Time [" ++ (show $ getDisplayUnit Typ_T) ++ "]"),
                   YLabel ("Power [" ++ (show $ getDisplayUnit Typ_P) ++ "]"), 
                   Size $ Scale 0.7]


-- | Class fror Plotting Records 
class RPlot a where
  rPlot :: (String,a) -> IO ()

instance RPlot PowerRecord where   
  rPlot (rName, (PowerRecord time pMap)) = plotPathsStyle (rPlotAttrs rName) (zip styleList xydata)
    where ydata = map sPlotData $ M.elems pMap
          xydata = map (zip (sPlotData time)) ydata 
          keys = map fst $ M.toList pMap
          styleList = map (rPlotStyle . show) keys

instance RPlot SecPowerRecord where   
  rPlot (rName, (SecPowerRecord time pMap)) = plotPathsStyle (rPlotAttrs rName) (zip styleList xydata)
    where ydata = map sPlotData $ M.elems pMap
          xydata = map (zip (sPlotData time)) ydata 
          keys = map fst $ M.toList pMap
          styleList = map (rPlotStyle . show) keys

instance RPlot SequPwrRecord where   
  rPlot (sqName, (SequData rs)) = mapM_ rPlot $ zip nameList rs
    where
      nameList = map (\ x -> "PowerRecord of Section: " ++ show x) [1..length rs]  
