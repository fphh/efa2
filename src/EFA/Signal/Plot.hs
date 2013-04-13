{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances#-}


module EFA.Signal.Plot (
   run,
   signal, signalAttr, signalStyle, signalIO,
   xy, xyBasic, xyAttr, xyStyle, xyIO,
   surface, surfaceIO,
   record, recordStyle, recordAttr, recordIO, recordIOList,
   sequenceIO,
   recordSplitPlus, recordSplit, sequenceSplit,
   recordSelect, sequenceSelect,
   stack, stackAttr, stackIO,
   stacks, stacksAttr, stacksIO,
   getData,
   ) where

import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Data as D
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Colour as Colour

import EFA.Signal.SequenceData (SequData)

import EFA.Signal.Record (Record(Record))
-- import EFA.Signal.SequenceData (SequData(..))

import EFA.Signal.Signal (TC, toSigList, getDisplayType)
-- import EFA.Signal.Base (BSum)

import EFA.Signal.Data (Data, (:>), Nil, NestedList)
import EFA.Report.Typ (TDisp, DisplayType(Typ_T), getDisplayUnit, getDisplayTypName)
import EFA.Report.Base (UnitScale(UnitScale), getUnitScale)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)


import qualified Graphics.Gnuplot.Advanced as Plot
-- import qualified Graphics.Gnuplot.Advanced as AGP

-- import qualified Graphics.Gnuplot.Terminal.X11 as X11
-- import qualified Graphics.Gnuplot.Terminal.WXT as WXT

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Plot as Plt
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.ColorSpecification as ColourSpec

import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Foldable as Fold
import Control.Monad (zipWithM_)
import Control.Functor.HT (void)
import Data.Foldable (foldMap)
import Data.Monoid (mconcat)

-- import Control.Concurrent (threadDelay)

{-
import EFA.Signal.Plot.Global as Global
import EFA.Signal.Plot.Window as Window
import EFA.Signal.Plot.Record as PlRecord
-}

-- import qualified EFA.Signal.Plot.Options as PlOpts

-- | Get Signal Plot Data (Unit Conversion)  ---------------------------------------------------------------

getData ::
   (TDisp typ, D.FromList c, D.Map c, D.Storage c a, Fractional a) =>
   TC s typ (Data c a) -> NestedList c a
getData x = S.toList $ S.map (* fromRational s) x
   where (UnitScale s) = getUnitScale $ getDisplayUnit $ getDisplayType x


run ::
   (Terminal.C term, Graph.C graph) =>
   term -> Opts.T graph -> Plt.T graph -> IO ()
run terminal opts plt =
   void $ Plot.plot terminal $ Frame.cons opts plt


-- | Simple Signal Plotting -- without time axis --------------------------------------------------------------

-- | Plotting Signals against each other --------------------------------------------------------------
signalAttr ::
   (AxisLabel tc, Graph.C graph) =>
   String -> tc -> Opts.T graph
signalAttr ti x =
   Opts.title ti $
   Opts.xLabel "Sample-Nr []" $
   Opts.yLabel (genAxLabel x) $
   Opts.grid True $
   Opts.deflt

signalStyle :: Graph2D.T x y -> Graph2D.T x y
signalStyle =
   Graph2D.lineSpec $
      {- not supported by "lines" style
      LineSpec.pointSize 2 $
      -}
      LineSpec.deflt


signalIO ::
   (Signal signal) =>
   String -> signal -> IO ()
signalIO ti x =
   void $ Plot.plotDefault $
   Frame.cons (signalAttr ti x) $
   fmap signalStyle $ signal x

class AxisLabel tc => Signal tc where
   signal :: tc -> Plot2D.T Int (Value tc)

instance
   (TDisp t, SV.Walker v1, SV.FromList v1, SV.Storage v1 y,
    Atom.C y, Tuple.C y, Fractional y) =>
      Signal (TC s t (Data (v1 :> Nil) y))  where
   signal x =
      Plot2D.list Graph2D.listLines $ getData x

instance (Signal tc) => Signal [tc]  where
   signal = foldMap signal

instance
   (TDisp t,
    SV.Walker v1, SV.FromList v1, SV.Storage v1 y,
    SV.FromList v2, SV.Storage v2 (v1 y),
    Atom.C y, Tuple.C y, Fractional y) =>
      Signal (TC s t (Data (v2 :> v1 :> Nil) y))  where
   signal x = signal $ toSigList x


-- | Plotting Signals against each other --------------------------------------------------------------
xyAttr ::
   (AxisLabel tcX, AxisLabel tcY, Graph.C graph) =>
   String -> tcX -> tcY -> Opts.T graph
xyAttr ti x y =
   Opts.title ti $
   Opts.xLabel (genAxLabel x) $
   Opts.yLabel (genAxLabel y) $
   Opts.grid True $
   Opts.deflt

xyStyle ::
   Int -> Plot2D.T x y -> Plot2D.T x y
xyStyle n =
   fmap $ Graph2D.lineSpec $
      LineSpec.pointSize 0.1 $
      LineSpec.pointType 7 $
      LineSpec.lineWidth 1 $
      LineSpec.title (show $ "Signal" ++ show n) $
      LineSpec.deflt

xyIO ::
   (XY tcX tcY) =>
   String -> tcX -> tcY -> IO ()
xyIO ti x y =
   void $ Plot.plotDefault $
   Frame.cons (xyAttr ti x y) $
   xy x y

class (AxisLabel tcX, AxisLabel tcY) => XY tcX tcY where
   xy :: tcX -> tcY -> Plot2D.T (Value tcX) (Value tcY)

xyBasic ::
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   TC s t1 (Data (v1 :> Nil) x) ->
   TC s t2 (Data (v2 :> Nil) y) ->
   Plot2D.T x y
xyBasic x y =
--   Plot2D.list Graph2D.linesPoints $ zip (getData x) (getData y)
   Plot2D.list Graph2D.lines $ zip (getData x) (getData y)

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XY (TC s t1 (Data (v1 :> Nil) x))
      (TC s t2 (Data (v2 :> Nil) y)) where
   xy = xyBasic

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XY (TC s t1 (Data (v1 :> Nil) x))
      [TC s t2 (Data (v2 :> Nil) y)] where
   xy x ys =
      mconcat $
      zipWith
         (\ n y -> xyStyle n $ xyBasic x y)
         [(0::Int)..] ys

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XY [TC s t1 (Data (v1 :> Nil) x)]
      [TC s t2 (Data (v2 :> Nil) y)] where
   xy xs ys =
      mconcat $
      zipWith3
         (\ n x y -> xyStyle n $ xyBasic x y)
         [(0::Int)..] xs ys

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    SV.FromList v3, SV.Storage v3 (v2 y),
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XY (TC s t1 (Data (v1 :> Nil) x))
      (TC s t2 (Data (v3 :> v2 :> Nil) y)) where
   xy x y = xy x (toSigList y)

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
   XY
      (TC s t1 (Data (v2 :> v1 :> Nil) x))
      (TC s t2 (Data (v4 :> v3 :> Nil) y)) where
   xy x y = xy (toSigList x) (toSigList y)


-- | Plotting Surfaces
surfaceIO ::
   Surface tcX tcY tcZ =>
   String -> tcX -> tcY -> tcZ -> IO ()
surfaceIO ti x y z = do
   let attrs =
          Opts.title ti $
          Opts.xLabel (genAxLabel x) $
          Opts.yLabel (genAxLabel y) $
          Opts.grid True $
          Opts.size 1 1 $
          Opts.deflt
   void $ Plot.plotDefault $
      Frame.cons attrs $ surface x y z

class
   (AxisLabel tcX, AxisLabel tcY, AxisLabel tcZ) =>
      Surface tcX tcY tcZ where
   surface :: tcX -> tcY -> tcZ -> Plot3D.T (Value tcX) (Value tcY) (Value tcZ)

instance
   (SV.FromList v1, SV.Storage v1 x, SV.FromList v2, SV.Storage v2 (v1 x), TDisp t1,
    SV.FromList v3, SV.Storage v3 y, SV.FromList v4, SV.Storage v4 (v3 y), TDisp t2,
    SV.FromList v5, SV.Storage v5 z, SV.FromList v6, SV.Storage v6 (v5 z), TDisp t3,
    Atom.C x, Tuple.C x,
    Atom.C y, Tuple.C y,
    Atom.C z, Tuple.C z) =>

      Surface
         (TC s1 t1 (Data (v2 :> v1 :> Nil) x))
         (TC s2 t2 (Data (v4 :> v3 :> Nil) y))
         (TC s3 t3 (Data (v6 :> v5 :> Nil) z)) where

   surface x y z =
      Plot3D.mesh $
      L.zipWith3 zip3 (S.toList2 x) (S.toList2 y) (S.toList2 z)


-- | Plotting Records ---------------------------------------------------------------

-- | Line Style
recordStyle :: (Show k) => k -> String -> Plot2D.T x y -> Plot2D.T x y
recordStyle key colour =
   fmap $ Graph2D.lineSpec $
      LineSpec.pointSize 0.3$
      LineSpec.pointType 1 $
      LineSpec.lineWidth 1.6 $
      lineColour colour $
      LineSpec.title (show key) $
      LineSpec.deflt

-- | Plot Attributes
recordAttr ::
   (Graph.C graph) =>
   String -> Opts.T graph
recordAttr name =
   Opts.title (name) $
   Opts.grid True $
   Opts.xLabel ("Time [" ++ (show $ getDisplayUnit Typ_T) ++ "]") $
   Opts.yLabel ("")
--    Opts.yLabel (show  "[" ++ (show $ getDisplayUnit Typ_P) ++ "]") $
--   Opts.size (Scale 0.7) $
   Opts.deflt


record ::
   (Show id, Ord id, TDisp typ0, TDisp typ1,
    SV.Walker v, SV.FromList v,
    SV.Storage v a, Fractional a, Atom.C a, Tuple.C a) =>
   Record s1 s2 typ0 typ1 id v a -> Plot2D.T a a
record (Record time pMap) =
   Fold.fold $
   M.mapWithKey
      (\key (col, sig) ->
         recordStyle key col $
         Plot2D.list Graph2D.linesPoints $
         zip (getData time) (getData sig)) $
   Colour.adorn pMap

recordIO ::
   (Fractional y,
    Show id, Ord id,
    SV.Walker v, SV.Storage v y, SV.FromList v,
    TDisp t2, TDisp t1,
    Tuple.C y, Atom.C y) =>
   String -> Record s1 s2 t1 t2 id v y -> IO ()
recordIO name =
   void . Plot.plotDefault . Frame.cons (recordAttr name) . record


recordIOList ::
   (Fractional y,
    Show id, Ord id,
    SV.Walker v, SV.Storage v y, SV.FromList v,
    TDisp t2, TDisp t1,
    Tuple.C y, Atom.C y) =>
   String -> [Record s1 s2 t1 t2 id v y] -> IO ()
recordIOList name recList =
   void $ Plot.plotDefault $ Frame.cons (recordAttr name) $ foldMap record recList



recordSplitPlus ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional y,
    Tuple.C y, Atom.C y,
    SV.Walker v,
    SV.Storage v y,
    SV.FromList v,
    SV.Len (v y)) =>
   Int -> String -> Record s1 s2 t1 t2 id v y ->
   [(id, TC s2 t2 (Data (v :> Nil) y))] -> IO ()
recordSplitPlus n name r list =
   zipWithM_
      (\k -> recordIO (name ++ " - Part " ++ show (k::Int)))
      [0 ..] (map (Record.addSignals list) (Record.split n r))


--------------------------------------------
-- recordIO command to show max n signals per window

recordSplit ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional y,
    Tuple.C y, Atom.C y,
    SV.Walker v,
    SV.Storage v y,
    SV.FromList v) =>
   Int -> String -> Record s1 s2 t1 t2 id v y -> IO ()
recordSplit n name r =
   zipWithM_
      (\k -> recordIO (name ++ " - Part " ++ show (k::Int)))
      [0..] (Record.split n r)


--------------------------------------------
-- recordIO command to plot selected Signals only

recordSelect ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional y,
    Tuple.C y, Atom.C y,
    SV.Walker v,
    SV.Storage v y,
    SV.FromList v) =>
   [id] -> String -> Record s1 s2 t1 t2 id v y -> IO ()
recordSelect idList name = recordIO name . Record.extract idList




sequenceFrame ::
   (Fractional y,
    Show id, Ord id,
    SV.Walker v, SV.Storage v y, SV.FromList v,
    TDisp t2, TDisp t1,
    Tuple.C y, Atom.C y) =>
   String -> SequData (Record s1 s2 t1 t2 id v y) ->
   SequData (Frame.T (Graph2D.T y y))
sequenceFrame sqName =
   SD.mapWithSection
      (\x ->
         Frame.cons (recordAttr ("Sequence " ++ sqName ++ ", Record of " ++ show x)) .
         record)

sequenceIO ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional y,
    Tuple.C y, Atom.C y,
    SV.Walker v,
    SV.Storage v y,
    SV.FromList v) =>
   String -> SequData (Record s1 s2 t1 t2 id v y) -> IO ()
sequenceIO name =
   Fold.mapM_ Plot.plotDefault . sequenceFrame name

sequenceSplit ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional y,
    Tuple.C y, Atom.C y,
    SV.Walker v,
    SV.Storage v y,
    SV.FromList v) =>
   Int -> String -> SequData (Record s1 s2 t1 t2 id v y) -> IO ()
sequenceSplit n name =
   Fold.sequence_ .
   SD.mapWithSection (\ x -> recordSplit n (name ++ " - " ++ show x))

sequenceSelect ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional y,
    Tuple.C y, Atom.C y,
    SV.Walker v,
    SV.Storage v y,
    SV.FromList v) =>
   [id] -> String -> SequData (Record s1 s2 t1 t2 id v y) ->  IO ()
sequenceSelect idList name =
   sequenceIO name . fmap (Record.extract idList)


optKeyOutside :: Opts.T graph -> Opts.T graph
optKeyOutside =
   Opts.add (Opt.custom "key" "position") ["outside"]

lineColour :: String -> LineSpec.T -> LineSpec.T
lineColour = LineSpec.lineColor . ColourSpec.name

stackLineSpec ::
   (FormatValue term, Show term) => term -> String -> Plot2D.T x y -> Plot2D.T x y
stackLineSpec term colour =
   fmap (Graph2D.lineSpec (LineSpec.title (Format.unASCII $ formatValue term)
          (lineColour colour $ LineSpec.deflt)))

stackAttr ::
   (FormatValue var) =>
   String -> var -> Opts.T (Graph2D.T Int Double)
stackAttr title var =
   Opts.title title $
      Histogram.rowstacked $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      optKeyOutside $
      Opts.xTicks2d [(Format.unASCII $ formatValue var, 0)] $
      Opts.deflt

stack ::
   (FormatValue term, Show term, Ord term) =>
   M.Map term Double -> Plot2D.T Int Double
stack =
   Fold.fold .
   M.mapWithKey
      (\term (col, val) ->
         stackLineSpec term col $
           Plot2D.list Graph2D.histograms [val]) .
   Colour.adorn


stackIO ::
   (FormatValue var, FormatValue term, Show term, Ord term) =>
   String -> var -> M.Map term Double -> IO ()
stackIO title var m =
   void . Plot.plotDefault . Frame.cons (stackAttr title var) . stack $ m


stacksAttr ::
   (FormatValue var) =>
   String -> [var] -> Opts.T (Graph2D.T Int Double)
stacksAttr title vars =
   Opts.title title $
      Histogram.rowstacked $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      optKeyOutside $
      Opts.boxwidthAbsolute 0.9 $
      Opts.xTicks2d (zip (map (Format.unASCII . formatValue) vars) [0..]) $
      Opts.deflt

stacks ::
   (Ord term, FormatValue term, Show term) =>
   M.Map term [Double] -> Plot2D.T Int Double
stacks =
   Fold.fold .
   M.mapWithKey
      (\term (col, vals) ->
         stackLineSpec term col $
         Plot2D.list Graph2D.histograms vals) .
   Colour.adorn

{- |
The length of @[var]@ must match the one of the @[Double]@ lists.
-}
stacksIO ::
   (FormatValue var, Ord term, FormatValue term, Show term) =>
   String -> [var] -> M.Map term [Double] -> IO ()
stacksIO title vars xs =
   void . Plot.plotDefault .
   Frame.cons (stacksAttr title vars) . stacks $ xs

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

