{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module EFA2.Display.Plot (module EFA2.Display.Plot) where

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
-- import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Graph as Graph

import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Data as D
import qualified EFA2.Signal.Vector as SV
import EFA2.Signal.SequenceData (PowerRecord(PowerRecord), SequPwrRecord, SequData(SequData))
import EFA2.Signal.Signal (TC, Signal, toSigList, getDisplayType)
import EFA2.Signal.Data (Data, (:>), Nil, NestedList)
import EFA2.Signal.Base (Val)
import EFA2.Display.DispTyp (TDisp, DisplayType(Typ_P, Typ_T), getDisplayUnit, getDisplayTypName)
import EFA2.Display.DispBase (UnitScale(UnitScale), getUnitScale)

import System.Process (system)

import qualified Data.List as L
import qualified Data.Map as M
import Control.Functor.HT (void)
import Data.Monoid (mconcat)


-- | Get Signal Plot Data (Unit Conversion)  ---------------------------------------------------------------

sPlotData ::
   (D.FromList c, D.Map c, D.Storage c Val, TDisp typ) =>
   TC s typ (Data c Val) -> NestedList c Val
sPlotData x = S.toList $ S.map (*s) x
   where (UnitScale s) = getUnitScale $ getDisplayUnit $ getDisplayType x


-- | Simple Signal Plotting -- without time axis --------------------------------------------------------------

-- | Plotting Signals against each other --------------------------------------------------------------
sigPlotAttr ::
   (AxisLabel tc, Graph.C graph) =>
   String -> tc -> Opts.T graph
sigPlotAttr ti x =
   Opts.title ti $
   Opts.xLabel "Sample-Nr []" $
   Opts.yLabel (genAxLabel x) $
   Opts.grid True $
   Opts.deflt

sigPlotStyle :: Graph2D.T x y -> Graph2D.T x y
sigPlotStyle =
   Graph2D.lineSpec $
      LineSpec.pointSize 2 $
      LineSpec.deflt

sigPlot :: SigPlot a => String -> a -> IO ()
sigPlot ti x =
   void $ Plot.plotDefault $
   Frame.cons (sigPlotAttr ti x) $
   fmap sigPlotStyle $ sigPlotCore x

class AxisLabel a => SigPlot a where
   sigPlotCore :: a -> Plot2D.T Int Val

instance
   (SV.Walker v1, SV.FromList v1, SV.Storage v1 Val, TDisp t) =>
      SigPlot (TC s t (Data (v1 :> Nil) Val))  where
   sigPlotCore x =
      Plot2D.list Graph2D.listLines $ sPlotData x

instance (SigPlot tc) => SigPlot [tc]  where
   sigPlotCore xs =
      mconcat $ map sigPlotCore xs

instance
   (SV.Walker v1, SV.FromList v1, SV.Storage v1 Val, SV.FromList v2, SV.Storage v2 (v1 Val),
    TDisp t) =>
      SigPlot (TC s t (Data (v2 :> v1 :> Nil) Val))  where
   sigPlotCore x = sigPlotCore $ toSigList x


-- | Plotting Signals against each other --------------------------------------------------------------
xyPlotAttr ::
   (AxisLabel tcX, AxisLabel tcY, Graph.C graph) =>
   String -> tcX -> tcY -> Opts.T graph
xyPlotAttr ti x y =
   Opts.title ti $
   Opts.xLabel (genAxLabel x) $
   Opts.yLabel (genAxLabel y) $
   Opts.grid True $
   Opts.deflt

xyPlotStyle ::
   Int -> Plot2D.T x y -> Plot2D.T x y
xyPlotStyle n =
   fmap $ Graph2D.lineSpec $
      LineSpec.pointSize 1.5 $
      LineSpec.pointType 7 $
      LineSpec.lineWidth 2 $
      LineSpec.title (show $ "Signal" ++ show n) $
      LineSpec.deflt

xyplot :: (XYPlot a b) => String -> a -> b -> IO ()
xyplot ti x y =
   void $ Plot.plotDefault $
   Frame.cons (xyPlotAttr ti x y) $
   xyplotCore x y

class (AxisLabel a, AxisLabel b) => XYPlot a b where
   xyplotCore :: a -> b -> Plot2D.T Val Val

xyplotBasic ::
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 Val,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 Val) =>
   (TC s t1 (Data (v1 :> Nil) Val)) ->
   (TC s t2 (Data (v2 :> Nil) Val)) ->
   Plot2D.T Val Val
xyplotBasic x y =
   Plot2D.list Graph2D.linesPoints $ zip (sPlotData x) (sPlotData y)

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 Val,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 Val) =>
   XYPlot (TC Signal t1 (Data (v1 :> Nil) Val)) (TC Signal t2 (Data (v2 :> Nil) Val)) where
   xyplotCore = xyplotBasic

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 Val,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 Val) =>
   XYPlot (TC s t1 (Data (v2 :> Nil) Val)) [(TC s t2 (Data (v1 :> Nil) Val))] where
   xyplotCore x ys =
      mconcat $
      zipWith
         (\ n y -> xyPlotStyle n $ xyplotBasic x y)
         [(0::Int)..] ys

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 Val,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 Val) =>
   XYPlot [(TC s t1 (Data (v2 :> Nil) Val))] [(TC s t2 (Data (v1 :> Nil) Val))] where
   xyplotCore xs ys =
      mconcat $
      zipWith3
         (\ n x y -> xyPlotStyle n $ xyplotBasic x y)
         [(0::Int)..] xs ys

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 Val,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 Val,
    SV.FromList v3, SV.Storage v3 (v2 Val)) =>
   XYPlot (TC s t1 (Data (v1 :> Nil) Val)) (TC s t2 (Data (v3 :> v2 :> Nil) Val)) where
   xyplotCore x y = xyplotCore x (toSigList y)

instance (TDisp t1,
          TDisp t2,
          SV.Walker v1,
          SV.Walker v3,
          SV.FromList v1, SV.Storage v1 Val,
          SV.FromList v3, SV.Storage v3 Val,
          SV.FromList v2, SV.Storage v2 (v1 Val),
          SV.FromList v4, SV.Storage v4 (v3 Val)) =>
   XYPlot
      (TC s t1 (Data (v2 :> v1 :> Nil) Val))
      (TC s t2 (Data (v4 :> v3 :> Nil) Val)) where
   xyplotCore x y = xyplotCore (toSigList x) (toSigList y)


-- | Plotting Surfaces
surfPlot :: SurfPlot a b c => String -> a -> b -> c -> IO ()
surfPlot ti x y z = do
   clearCurves
   let attrs =
          Opts.title ti $
          Opts.xLabel (genAxLabel x) $
          Opts.yLabel (genAxLabel y) $
          Opts.grid True $
          Opts.size 1 1 $
          Opts.deflt
   void $ Plot.plotDefault $
      Frame.cons attrs $ surfPlotCore x y z
   saveCurves ti
   return ()

class (AxisLabel a, AxisLabel b, AxisLabel c) => SurfPlot a b c where
   surfPlotCore :: a -> b -> c -> Plot3D.T Val Val Val

instance
   (SV.FromList v1, SV.Storage v1 Val, SV.FromList v2, SV.Storage v2 (v1 Val), TDisp t1,
    SV.FromList v3, SV.Storage v3 Val, SV.FromList v4, SV.Storage v4 (v3 Val), TDisp t2,
    SV.FromList v5, SV.Storage v5 Val, SV.FromList v6, SV.Storage v6 (v5 Val), TDisp t3) =>
      SurfPlot
         (TC s1 t1 (Data (v2 :> v1 :> Nil) Val))
         (TC s2 t2 (Data (v4 :> v3 :> Nil) Val))
         (TC s3 t3 (Data (v6 :> v5 :> Nil) Val)) where
   surfPlotCore x y z =
      Plot3D.mesh $
      L.zipWith3 zip3 (S.toList2 x) (S.toList2 y) (S.toList2 z)


-- | Plotting Records ---------------------------------------------------------------

-- | Line Style
rPlotStyle :: (Show k) => k -> Plot2D.T x y -> Plot2D.T x y
rPlotStyle key =
   fmap $ Graph2D.lineSpec $
      LineSpec.pointSize 1.5 $
      LineSpec.pointType 7 $
      LineSpec.lineWidth 2 $
      LineSpec.title (show key) $
      LineSpec.deflt

-- | Plot Attributes
rPlotAttr ::
   (Graph.C graph) =>
   String -> Opts.T graph
rPlotAttr name =
   Opts.title ("PowerRecord: " ++ name) $
   Opts.grid True $
   Opts.xLabel ("Time [" ++ (show $ getDisplayUnit Typ_T) ++ "]") $
   Opts.yLabel ("Power [" ++ (show $ getDisplayUnit Typ_P) ++ "]") $
--   Opts.size (Scale 0.7) $
   Opts.deflt

rPlot :: (RPlot a) => (String, a) -> IO ()
rPlot (name, r) =
   mapM_ Plot.plotDefault $ rPlotCore name r

-- | Class for Plotting Records
class RPlot a where
   rPlotCore :: String -> a -> [Frame.T (Graph2D.T Val Val)]

instance
   (SV.Walker v, SV.FromList v, SV.Storage v Val) =>
      RPlot (PowerRecord v Double) where
   rPlotCore rName (PowerRecord time pMap) =
      [rPlotSingle rName time pMap]

rPlotSingle ::
   (Show k, TDisp typ0, TDisp typ1,
    SV.Walker v, SV.FromList v, SV.Storage v Val) =>
   String ->
   TC s typ0 (Data (v :> Nil) Val) ->
   M.Map k (TC s typ1 (Data (v :> Nil) Val)) ->
   Frame.T (Graph2D.T Val Val)
rPlotSingle rName time pMap =
   Frame.cons (rPlotAttr rName) $
   mconcat $
   map
      (\(key, sig) ->
         rPlotStyle key $
         Plot2D.list Graph2D.linesPoints $
         zip (sPlotData time) (sPlotData sig)) $
   M.toList pMap

instance RPlot SequPwrRecord where
   rPlotCore _sqName (SequData rs) = concat $ zipWith rPlotCore nameList rs
    where
      nameList = map (\ x -> "PowerRecord of Section: " ++ show x) [(1::Int) ..]


class AxisLabel tc where
   genAxLabel :: tc -> String

instance (TDisp t) => AxisLabel (TC s t c) where
   genAxLabel x =
      let dispType = getDisplayType x
      in  getDisplayTypName dispType ++
             " [" ++ (show $ getDisplayUnit dispType) ++ "]"

instance (AxisLabel tc) => AxisLabel [tc] where
   genAxLabel x = genAxLabel $ head x


-- | clean old gnuplot files from current dir
clearCurves ::  IO ()
clearCurves = do
  system ("rm curve.gp")
  system ("rm curve*.csv")
  return ()

saveCurves :: String -> IO ()
saveCurves ti = do
  system ("mkdir gnuplot")
  system ("mkdir gnuplot/"++ti)
  system ("mv curve.gp gnuplot/" ++ ti)
  system ("mv curve*.csv gnuplot/" ++ ti)
  return ()
