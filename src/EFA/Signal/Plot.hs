{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Signal.Plot where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Data as D
import qualified EFA.Signal.Vector as SV
import EFA.Signal.SequenceData (PowerRecord(PowerRecord), SequPwrRecord, SequData(SequData))
import EFA.Signal.Signal (TC, Signal, toSigList, getDisplayType)
import EFA.Signal.Data (Data, (:>), Nil, NestedList)
import EFA.Report.Typ (TDisp, DisplayType(Typ_P, Typ_T), getDisplayUnit, getDisplayTypName)
import EFA.Report.Base (UnitScale(UnitScale), getUnitScale)

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
-- import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts

import qualified Data.List as L
import qualified Data.Map as M
import Control.Functor.HT (void)
import Data.Foldable (foldMap)
import Data.Monoid (mconcat)


-- | Get Signal Plot Data (Unit Conversion)  ---------------------------------------------------------------

sPlotData ::
   (TDisp typ, D.FromList c, D.Map c, D.Storage c a, Fractional a) =>
   TC s typ (Data c a) -> NestedList c a
sPlotData x = S.toList $ S.map (* fromRational s) x
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
      {- not supported by "lines" style
      LineSpec.pointSize 2 $
      -}
      LineSpec.deflt

sigPlot ::
   (SigPlot signal) =>
   String -> signal -> IO ()
sigPlot ti x =
   void $ Plot.plotDefault $
   Frame.cons (sigPlotAttr ti x) $
   fmap sigPlotStyle $ sigPlotCore x

class AxisLabel tc => SigPlot tc where
   sigPlotCore :: tc -> Plot2D.T Int (Value tc)

instance
   (TDisp t, SV.Walker v1, SV.FromList v1, SV.Storage v1 y,
    Atom.C y, Tuple.C y, Fractional y) =>
      SigPlot (TC s t (Data (v1 :> Nil) y))  where
   sigPlotCore x =
      Plot2D.list Graph2D.listLines $ sPlotData x

instance (SigPlot tc) => SigPlot [tc]  where
   sigPlotCore = foldMap sigPlotCore

instance
   (TDisp t,
    SV.Walker v1, SV.FromList v1, SV.Storage v1 y,
    SV.FromList v2, SV.Storage v2 (v1 y),
    Atom.C y, Tuple.C y, Fractional y) =>
      SigPlot (TC s t (Data (v2 :> v1 :> Nil) y))  where
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

xyplot ::
   (XYPlot tcX tcY) =>
   String -> tcX -> tcY -> IO ()
xyplot ti x y =
   void $ Plot.plotDefault $
   Frame.cons (xyPlotAttr ti x y) $
   xyplotCore x y

class (AxisLabel tcX, AxisLabel tcY) => XYPlot tcX tcY where
   xyplotCore :: tcX -> tcY -> Plot2D.T (Value tcX) (Value tcY)

xyplotBasic ::
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   TC s t1 (Data (v1 :> Nil) x) ->
   TC s t2 (Data (v2 :> Nil) y) ->
   Plot2D.T x y
xyplotBasic x y =
   Plot2D.list Graph2D.linesPoints $ zip (sPlotData x) (sPlotData y)

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XYPlot (TC Signal t1 (Data (v1 :> Nil) x))
          (TC Signal t2 (Data (v2 :> Nil) y)) where
   xyplotCore = xyplotBasic

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XYPlot (TC s t1 (Data (v1 :> Nil) x))
          [TC s t2 (Data (v2 :> Nil) y)] where
   xyplotCore x ys =
      mconcat $
      zipWith
         (\ n y -> xyPlotStyle n $ xyplotBasic x y)
         [(0::Int)..] ys

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XYPlot [TC s t1 (Data (v1 :> Nil) x)]
          [TC s t2 (Data (v2 :> Nil) y)] where
   xyplotCore xs ys =
      mconcat $
      zipWith3
         (\ n x y -> xyPlotStyle n $ xyplotBasic x y)
         [(0::Int)..] xs ys

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    SV.FromList v3, SV.Storage v3 (v2 y),
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XYPlot (TC s t1 (Data (v1 :> Nil) x))
          (TC s t2 (Data (v3 :> v2 :> Nil) y)) where
   xyplotCore x y = xyplotCore x (toSigList y)

instance
   (TDisp t1,
    TDisp t2,
    SV.Walker v1,
    SV.Walker v3,
    SV.FromList v1, SV.Storage v1 x,
    SV.FromList v3, SV.Storage v3 y,
    SV.FromList v2, SV.Storage v2 (v1 x),
    SV.FromList v4, SV.Storage v4 (v3 y),
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XYPlot
      (TC s t1 (Data (v2 :> v1 :> Nil) x))
      (TC s t2 (Data (v4 :> v3 :> Nil) y)) where
   xyplotCore x y = xyplotCore (toSigList x) (toSigList y)


-- | Plotting Surfaces
surfPlot ::
   SurfPlot tcX tcY tcZ =>
   String -> tcX -> tcY -> tcZ -> IO ()
surfPlot ti x y z = do
   let attrs =
          Opts.title ti $
          Opts.xLabel (genAxLabel x) $
          Opts.yLabel (genAxLabel y) $
          Opts.grid True $
          Opts.size 1 1 $
          Opts.deflt
   void $ Plot.plotDefault $
      Frame.cons attrs $ surfPlotCore x y z

class
   (AxisLabel tcX, AxisLabel tcY, AxisLabel tcZ) =>
      SurfPlot tcX tcY tcZ where
   surfPlotCore :: tcX -> tcY -> tcZ -> Plot3D.T (Value tcX) (Value tcY) (Value tcZ)

instance
   (SV.FromList v1, SV.Storage v1 x, SV.FromList v2, SV.Storage v2 (v1 x), TDisp t1,
    SV.FromList v3, SV.Storage v3 y, SV.FromList v4, SV.Storage v4 (v3 y), TDisp t2,
    SV.FromList v5, SV.Storage v5 z, SV.FromList v6, SV.Storage v6 (v5 z), TDisp t3,
    Atom.C x, Tuple.C x,
    Atom.C y, Tuple.C y,
    Atom.C z, Tuple.C z) =>

      SurfPlot
         (TC s1 t1 (Data (v2 :> v1 :> Nil) x))
         (TC s2 t2 (Data (v4 :> v3 :> Nil) y))
         (TC s3 t3 (Data (v6 :> v5 :> Nil) z)) where

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
class (Atom.C (D.Value record)) => RPlot record where
   rPlotCore ::
      String -> record ->
      [Frame.T (Graph2D.T (D.Value record) (D.Value record))]

instance
   (SV.Walker v, SV.FromList v,
    SV.Storage v y, Fractional y, Atom.C y, Tuple.C y) =>
      RPlot (PowerRecord v y) where
   rPlotCore rName (PowerRecord time pMap) =
      [rPlotSingle rName time pMap]

rPlotSingle ::
   (Show k, TDisp typ0, TDisp typ1,
    SV.Walker v, SV.FromList v,
    SV.Storage v x, Fractional x, Atom.C x, Tuple.C x,
    SV.Storage v y, Fractional y, Atom.C y, Tuple.C y) =>
   String ->
   TC s typ0 (Data (v :> Nil) x) ->
   M.Map k (TC s typ1 (Data (v :> Nil) y)) ->
   Frame.T (Graph2D.T x y)
rPlotSingle rName time pMap =
   Frame.cons (rPlotAttr rName) $
   foldMap
      (\(key, sig) ->
         rPlotStyle key $
         Plot2D.list Graph2D.linesPoints $
         zip (sPlotData time) (sPlotData sig)) $
   M.toList pMap

instance RPlot SequPwrRecord where
   rPlotCore _sqName (SequData rs) = concat $ zipWith rPlotCore nameList rs
    where
      nameList = map (\ x -> "PowerRecord of " ++ show x) [Idx.Section 1 ..]


class Atom.C (Value tc) => AxisLabel tc where
   type Value tc :: *
   genAxLabel :: tc -> String

instance (TDisp t, Atom.C (D.Value c)) => AxisLabel (TC s t c) where
   type Value (TC s t c) = D.Value c
   genAxLabel x =
      let dispType = getDisplayType x
      in  getDisplayTypName dispType ++
             " [" ++ (show $ getDisplayUnit dispType) ++ "]"

instance (AxisLabel tc) => AxisLabel [tc] where
   type Value [tc] = Value tc
   genAxLabel x = genAxLabel $ head x
