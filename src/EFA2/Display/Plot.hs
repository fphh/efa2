{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,FlexibleContexts,UndecidableInstances, TypeOperators, TypeSynonymInstances#-}


module EFA2.Display.Plot (module EFA2.Display.Plot) where

import System.Process
import System.Exit
import Graphics.Gnuplot.Simple
import qualified Data.List as L

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
import EFA2.Utils.Utils

import qualified Data.Map as M

-- | Get Signal Plot Data (Unit Conversion)  ---------------------------------------------------------------

class SPlotData s typ c1 d e where 
  sPlotData :: TC s typ (c1 d) -> e
    
instance (DisplayTyp t) => SPlotData s t  (Data Nil) Val Val where 
  sPlotData x@(TC (Data (D0 v))) = v*s  
    where (UnitScale s) = getUnitScale $ getDisplayUnit $ getDisplayType x

instance (DFromList (Data (v1 :> Nil)) Val, DisplayTyp t) => SPlotData s t  (Data (v1 :> Nil)) Val [Val] where 
  sPlotData x = map (*s) $ (stoList x ::[Val])  
    where (UnitScale s) = getUnitScale $ getDisplayUnit $ getDisplayType x

instance (DFromList2 (Data (v2 :> v1 :> Nil)) Val, DisplayTyp t) => SPlotData s t  (Data (v2 :> v1 :> Nil)) Val [[Val]]where 
  sPlotData x = map (map (*s)) $ stoList2 x  
    where (UnitScale s) = getUnitScale $ getDisplayUnit $ getDisplayType x

-- | Simple Signal Plotting -- without time axis --------------------------------------------------------------

-- | Plotting Signals against each other --------------------------------------------------------------
sigPlotAttr :: (DisplayTyp t) => String -> TC s t (Data (v1 :> Nil) Val) ->  [Attribute]
sigPlotAttr ti x = [Title ti,LineStyle 1 [PointSize 2], XLabel $ "Sample-Nr []",YLabel $ genAxLabel x,Grid $ Just []]

class SigPlot a where          
  sigPlot :: String -> a -> IO ()

instance (SPlotData s t (Data (v1 :> Nil)) Val [Val],DisplayTyp t) => SigPlot (TC s t  (Data (v1 :> Nil) Val))  where 
  sigPlot ti x = plotList (sigPlotAttr ti x) (sPlotData x :: [Val])

instance (SPlotData s t (Data (v1 :> Nil)) Val [Val],DisplayTyp t) => SigPlot [(TC s t  (Data (v1 :> Nil) Val))]  where 
  sigPlot ti xs = plotLists (sigPlotAttr ti (head xs)) ((map sPlotData xs) :: [[Val]])

instance (DisplayTyp t, 
          VFromList v1 Double, 
          VFromList v2 (TC s t (Data (v1 :> (Nil' :> Nil')) Val)),
          VWalker v2 (v1 Val) (TC  s t (Data (v1 :> (Nil' :> Nil')) Val))) => SigPlot (TC s t (Data (v2 :> v1 :> Nil) Val))  where 
  sigPlot ti x = sigPlot ti $ toSigList x  

-- | Plotting Signals against each other --------------------------------------------------------------
xyPlotAttr :: (DisplayTyp t1, DisplayTyp t2) => String -> TC s t1 (Data (v1 :> Nil) Val) ->  TC s t2 (Data (v2 :> Nil) Val) -> [Attribute]
xyPlotAttr ti x y = [Title ti,LineStyle 1 [PointSize 2], XLabel $ genAxLabel x,YLabel $ genAxLabel y,Grid $ Just []]

xyPlotStyle legend =  (PlotStyle {plotType = LinesPoints, lineSpec = CustomStyle [LineTitle legend,LineWidth 2, PointType 7, PointSize 1.5]})

class XYPlot a b where
  xyplot ::  String -> a -> b -> IO ()
  
instance (DisplayTyp t1,
          DisplayTyp t2,
          VFromList v1 Double,
          VFromList v2 Double) => 
         XYPlot (TC Signal t1 (Data (v1 :> Nil) Val)) (TC Signal t2 (Data (v2 :> Nil) Val)) where 
  xyplot ti x y = plotPath (xyPlotAttr ti x y) (zip (sPlotData x ::[Val]) (sPlotData y :: [Val]))

instance (DisplayTyp t1,
          DisplayTyp t2, 
          VFromList v1 Double, 
          VFromList v2 Double) => XYPlot (TC s t1 (Data (v2 :> Nil) Val)) [(TC s t2 (Data (v1 :> Nil) Val))] where 
  xyplot ti x ys = plotPathsStyle (xyPlotAttr ti x (head ys)) (zip styleList (map (\ y -> zip (sPlotData x::[Val]) (sPlotData y::[Val])) ys))
    where styleList = map (xyPlotStyle . show) (map (\ n -> "Signal"++ show n) $ listIdx ys)  

instance (DisplayTyp t1,
          DisplayTyp t2, 
          VFromList v1 Double, 
          VFromList v2 Double) => XYPlot [(TC s t1 (Data (v2 :> Nil) Val))] [(TC s t2 (Data (v1 :> Nil) Val))] where 
  xyplot ti xs ys = plotPathsStyle (xyPlotAttr ti (head xs) (head ys)) (zip styleList (zipWith (\ x y -> zip (sPlotData x::[Val]) (sPlotData y::[Val])) xs ys))
    where styleList = map (xyPlotStyle . show) (map (\ n -> "Signal"++ show n) $ listIdx ys)

instance (DisplayTyp t1,
          DisplayTyp t2, 
          VFromList v1 Double,
          VFromList v3 Double,
          VFromList v4 (TC s t2 (Data (v3 :> (Nil' :> Nil')) Val)),
          VWalker v4 (v3 Val) (TC s t2 (Data (v3 :> (Nil' :> Nil')) Val))) => 
         XYPlot (TC s t1 (Data (v1 :> Nil) Val)) (TC s t2 (Data (v4 :> v3 :> Nil) Val)) where 
  xyplot ti x y = xyplot ti x (toSigList y)

instance (DisplayTyp t1,
          DisplayTyp t2, 
          VFromList v1 Double, 
          VFromList v3 Double,
          VFromList v2 (TC s t1 (Data (v1 :> (Nil' :> Nil')) Val)),
          VWalker v2 (v1 Val) (TC s t1 (Data (v1 :> (Nil' :> Nil')) Val)), 
          VFromList v4 (TC s t2 (Data (v3 :> (Nil' :> Nil')) Val)),
          VWalker v4 (v3 Val) (TC s t2 (Data (v3 :> (Nil' :> Nil')) Val))) => 
         XYPlot (TC s t1 (Data (v2 :> v1 :> Nil) Val)) (TC s t2 (Data (v4 :> v3 :> Nil) Val)) where 
  xyplot ti x y = xyplot ti (toSigList x) (toSigList y)


-- | Plotting Surfaces
class SurfPlot a b c where
  surfPlot :: String -> a -> b -> c -> IO ()
  
instance (VFromList v2 (v1 Double),
          VFromList v1 Double,
          VFromList v2 [Double],
          VWalker v2 (v1 Double) [Double], 
          VFromList v3 [Double],
          VFromList v4 Double,
          VFromList v3 (v4 Double),
          VWalker v3 (v4 Double) [Double], 
          VFromList v5 (v6 Double),
          VFromList v6 Double,
          VFromList v5 [Double],
          VWalker v5 (v6 Double) [Double], 
          DisplayTyp t1,
          DisplayTyp t2,
          DisplayTyp t3) => 
         SurfPlot (TC s1 t1 (Data (v2 :> v1 :> Nil) Val)) (TC s2 t2 (Data (v3 :> v4 :> Nil) Val)) (TC s3 t3 (Data (v5 :> v6 :> Nil) Val)) where
  surfPlot ti x y z = do
      clearCurves     
      let plotAttrs      = [Title ti, 
                            Grid $ Just [], 
                            XLabel $ genAxLabel x,
                            YLabel $ genAxLabel y,
                            Size $ Scale 1]
      plotMesh3d (plotAttrs) [Plot3dType Surface] (L.zipWith3 zip3 (stoList2 x) (stoList2 y) (stoList2 z))      
      saveCurves ti
      return ()


-- | Plotting Records ---------------------------------------------------------------

-- | Line Style
rPlotStyle legend = (PlotStyle {plotType = LinesPoints, 
                                lineSpec = CustomStyle [LineTitle legend,
                                                        LineWidth 2, 
                                                        PointType 7, 
                                                        PointSize 1.5]})

-- | Plot Attributes
rPlotAttrs name = [Title ("PowerRecord: " ++ name), 
                   Grid $ Just [], 
                   XLabel ("Time [" ++ (show $ getDisplayUnit Typ_T) ++ "]"),
                   YLabel ("Power [" ++ (show $ getDisplayUnit Typ_P) ++ "]"), 
                   Size $ Scale 0.7]


-- | Class for Plotting Records 
class RPlot a where
  rPlot :: (String,a) -> IO ()

instance RPlot PowerRecord where   
  rPlot (rName, (PowerRecord time pMap)) = plotPathsStyle (rPlotAttrs rName) (zip styleList xydata)
    where ydata = map sPlotData $ M.elems pMap :: [[Val]]
          xydata = map (zip (sPlotData time)) ydata 
          keys = map fst $ M.toList pMap
          styleList = map (rPlotStyle . show) keys

instance RPlot SecPowerRecord where   
  rPlot (rName, (SecPowerRecord time pMap)) = plotPathsStyle (rPlotAttrs rName) (zip styleList xydata)
    where ydata = map sPlotData $ M.elems pMap :: [[Val]]
          xydata = map (zip (sPlotData time)) ydata 
          keys = map fst $ M.toList pMap
          styleList = map (rPlotStyle . show) keys

instance RPlot SequPwrRecord where   
  rPlot (sqName, (SequData rs)) = mapM_ rPlot $ zip nameList rs
    where
      nameList = map (\ x -> "PowerRecord of Section: " ++ show x) [1..length rs]  


genAxLabel :: (DisplayTyp t) => TC s t (c d) -> String 
genAxLabel x = (getDisplayTypName $ getDisplayType x) ++ " [" ++ (show $ getDisplayUnit $ getDisplayType x) ++ "]"


-- | clean old gnu-Plot files from current dir
clearCurves ::  IO ExitCode
clearCurves = do
  system ("rm curve.gp")
  system ("rm curve*.csv")

saveCurves :: String -> IO ExitCode
saveCurves ti = do 
  system ("mkdir gnuplot")
  system ("mkdir gnuplot/"++ti) 
  system ("mv curve.gp gnuplot/" ++ ti) 
  system ("mv curve*.csv gnuplot/" ++ ti) 
  