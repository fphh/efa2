{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | PlotBase provide the basic functions to build Plots
module EFA.Signal.Plot (
   run,
   signal,
   signalFrameAttr,
   Signal,
   xy,
   xyBasic,
   xyStyle,
   xyFrameAttr,
   XY,
   surface,
   Surface,
   record,
   recordStyle,
   recordFrame,
   recordList,
   sequence,
   optKeyOutside,
   stack,
   stackFrame,
   stacks,
   stacksFrame,
   getData,
   genAxLabel
   ) where

import Prelude hiding (sequence)
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
import EFA.Report.Typ (TDisp, getDisplayUnit, getDisplayTypName,DisplayType(Typ_T))
import EFA.Report.Base (UnitScale(UnitScale), getUnitScale)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)


import qualified Graphics.Gnuplot.Advanced as Plot
-- import qualified Graphics.Gnuplot.Advanced as AGP

--import qualified Graphics.Gnuplot.Terminal.X11 as X11
--import qualified Graphics.Gnuplot.Terminal.WXT as WXT

import qualified Graphics.Gnuplot.Terminal as Terminal
-- import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
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
import qualified Data.List.Key as Key
-- import Control.Monad (zipWithM_)
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

-- | Generic IO Commands ---------------------------------------------------------------
run ::
   (Terminal.C term, Graph.C graph) =>
   term -> Opts.T graph -> Plt.T graph -> IO ()
run terminal frameAttr plt =
   void $ Plot.plotSync terminal $ Frame.cons frameAttr plt


{-
-- | Example how to generate frame attributes

frameAttr ::
   (AxisLabel tc, Graph.C graph) =>
   String -> tc -> Opts.T graph
framAttr ti x =
   Opts.title ti $
   Opts.xLabel "Signal Index []" $
   Opts.yLabel (genAxLabel x) $
   Opts.grid True $
   Opts.deflt
-}

-- | Class to generate Axis Labels

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

-- | Get Signal Plot Data (Unit Conversion)  ---------------------------------------------------------------

getData ::
   (TDisp typ, D.FromList c, D.Map c, D.Storage c a, Fractional a) =>
   TC s typ (Data c a) -> NestedList c a
getData x = S.toList $ S.map (* fromRational s) x
   where (UnitScale s) = getUnitScale $ getDisplayUnit $ getDisplayType x


-- | Function to simplify linecolor setting

lineColour :: String -> LineSpec.T -> LineSpec.T
lineColour = LineSpec.lineColor . ColourSpec.name


-- | Simple Signal Plotting -- plot signal values against signal index --------------------------------------------------------------
-- | All styles can be overloaded by opts

signalFrameAttr ::
   (AxisLabel tc, Graph.C graph) =>
   String -> tc -> Opts.T graph
signalFrameAttr ti x =
   Opts.title ti $
   Opts.xLabel "Signal Index []" $
   Opts.yLabel (genAxLabel x) $
   Opts.grid True $
   Opts.deflt

signalStyle :: (LineSpec.T -> LineSpec.T) -> Graph2D.T x y -> Graph2D.T x y
signalStyle opts =
   Graph2D.lineSpec $
   {- not supported by "lines" style
      LineSpec.pointSize 2 $
      -}
   opts $ -- default settings can be overloaded here
   LineSpec.lineWidth 1 $
   LineSpec.deflt

class AxisLabel tc => Signal tc where
   signal :: (LineSpec.T -> LineSpec.T) -> tc -> Plot2D.T Int (Value tc)

instance
   (TDisp t, SV.Walker v1, SV.FromList v1, SV.Storage v1 y,
    Atom.C y, Tuple.C y, Fractional y) =>
      Signal (TC s t (Data (v1 :> Nil) y))  where
   signal opts x =
      fmap (signalStyle opts) $ Plot2D.list Graph2D.listLines $ getData x

instance (Signal tc) => Signal [tc]  where
   signal opts =  foldMap (signal opts)

instance
   (TDisp t,
    SV.Walker v1, SV.FromList v1, SV.Storage v1 y,
    SV.FromList v2, SV.Storage v2 (v1 y),
    Atom.C y, Tuple.C y, Fractional y) =>
      Signal (TC s t (Data (v2 :> v1 :> Nil) y))  where
   signal opts x = signal opts $ toSigList x


-- | Plotting Signals against each other, can be also used for time plots and curves over power -----------------------------

xyFrameAttr ::
   (AxisLabel tcX, AxisLabel tcY, Graph.C graph) =>
   String -> tcX -> tcY -> Opts.T graph
xyFrameAttr ti x y =
   Opts.title ti $
   Opts.xLabel (genAxLabel x) $
   Opts.yLabel (genAxLabel y) $
   Opts.grid True $
   Opts.deflt

xyStyle ::
   (LineSpec.T -> LineSpec.T) -> Plot2D.T x y -> Plot2D.T x y
xyStyle opts =
   fmap $ Graph2D.lineSpec $
      opts $
      LineSpec.pointSize 0.1 $
      LineSpec.pointType 7 $
      LineSpec.lineWidth 1 $
      LineSpec.deflt


class (AxisLabel tcX, AxisLabel tcY) => XY tcX tcY where
   xy :: (LineSpec.T -> LineSpec.T) -> 
         (Int -> String) -> tcX -> tcY -> Plot2D.T (Value tcX) (Value tcY)

xyBasic ::
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   (LineSpec.T -> LineSpec.T) ->
   TC s t1 (Data (v1 :> Nil) x) ->
   TC s t2 (Data (v2 :> Nil) y) ->
   Plot2D.T x y
xyBasic opts x y =
   (xyStyle opts) $ Plot2D.list Graph2D.lines $ zip (getData x) (getData y)

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XY (TC s t1 (Data (v1 :> Nil) x))
      (TC s t2 (Data (v2 :> Nil) y)) where
   xy opts legend = xyBasic ((LineSpec.title $ legend 0) . opts)

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XY (TC s t1 (Data (v1 :> Nil) x))
      [TC s t2 (Data (v2 :> Nil) y)] where
   xy opts legend x ys =
      mconcat $
      zipWith
         (\ n y -> xyBasic ((LineSpec.title $ legend n) . opts) x y)
         [(0::Int)..] ys

instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XY [TC s t1 (Data (v1 :> Nil) x)]
      [TC s t2 (Data (v2 :> Nil) y)] where
   xy opts legend xs ys =
      mconcat $
      zipWith3
         (\ n x y -> xyBasic ((LineSpec.title $ legend n) . opts) x y)
         [(0::Int)..] xs ys


instance
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    SV.FromList v3, SV.Storage v3 (v2 y),
    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y) =>
   XY (TC s t1 (Data (v1 :> Nil) x))
      (TC s t2 (Data (v3 :> v2 :> Nil) y)) where
   xy opts legend x y = xy opts legend x (toSigList y)

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
   xy opts legend x y = xy opts legend (toSigList x) (toSigList y)


-- | Plotting Records ---------------------------------------------------------------

recordFrame ::
   (Graph.C graph) =>
   String -> Opts.T graph
recordFrame ti =
   Opts.title (ti) $
   Opts.grid True $
   Opts.xLabel ("Time [" ++ (show $ getDisplayUnit Typ_T) ++ "]") $
   Opts.yLabel ("")
   Opts.deflt

recordStyle :: (LineSpec.T -> LineSpec.T) -> Plot2D.T x y -> Plot2D.T x y
recordStyle opts =
   fmap $ Graph2D.lineSpec $
      opts $
      LineSpec.pointSize 0.3$
      LineSpec.pointType 1 $
      LineSpec.lineWidth 1.6 $
      LineSpec.deflt

record ::
   (Show id, Ord id, TDisp typ0, TDisp typ1,
    SV.Walker v, SV.FromList v,
    SV.Storage v d1, Fractional d1, Atom.C d1, Tuple.C d1, Atom.C d2, Tuple.C d2, Fractional d2, SV.Storage v d2) =>
   (id -> String) ->
   (LineSpec.T -> LineSpec.T) ->
   Record s1 s2 typ0 typ1 id v d1 d2 -> Plot2D.T d1 d2
record showKey opts (Record time pMap) =
   Fold.fold $
   M.mapWithKey
      (\key (col, sig) ->
         recordStyle (opts . (LineSpec.title $ showKey key) . (lineColour $ Colour.unC col)) $
         Plot2D.list Graph2D.linesPoints $
         zip (getData time) (getData sig)) $
   Colour.adorn pMap

recordList ::
   (Ord id,
    Show id,
    SV.Walker v,
    SV.FromList v,
    TDisp t2,
    TDisp t1,
    Fractional d2,
    Fractional d1,
    SV.Storage v d2,
    SV.Storage v d1,
    Atom.C d2,
    Atom.C d1,
    Tuple.C d2,
    Tuple.C d1) =>
   (id -> String) ->
   (LineSpec.T -> LineSpec.T) ->
   (Int -> LineSpec.T -> LineSpec.T) ->
   [(Record.Name, Record s1 s2 t1 t2 id v d1 d2)] -> Plot2D.T d1 d2
recordList showKey opts varOpts xs =
    Fold.fold $ zipWith (\(Record.Name name,x) k -> record (\ key -> name ++ "-" ++ showKey key) ((varOpts k). opts) x) xs [0..]

-- | Plotting Sequences ---------------------------------------------------------------


sequence :: (Fractional d2, Fractional d1, Ord id,
             Show id, SV.Walker v, SV.Storage v d2,
             SV.Storage v d1, SV.FromList v, TDisp typ1,
             TDisp typ2, Atom.C d2, Atom.C d1, Tuple.C d2,
             Tuple.C d1) =>
            (id -> String) ->
            (LineSpec.T -> LineSpec.T) ->
            (Int -> LineSpec.T -> LineSpec.T) ->
            SequData (Record s1 s2 typ1 typ2 id v d1 d2) ->
            Plot2D.T d1 d2
sequence showKey opts varOpts (SD.SequData xs) =
  Fold.fold $ zipWith (\(SD.Section s _ x) k -> record (\key -> show "Sec " ++ show s ++ "-" ++ showKey key) ((varOpts k). opts) x) xs [0..]

-- | Plotting Stacks ---------------------------------------------------------------

optKeyOutside :: Opts.T graph -> Opts.T graph
optKeyOutside =
   Opts.add (Opt.custom "key" "position") ["outside"]

stackFrame ::
   String -> Format.ASCII -> Opts.T (Graph2D.T Int Double)
stackFrame title var =
   Opts.title title $
      Histogram.rowstacked $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      optKeyOutside $
      Opts.xTicks2d [(Format.unASCII var, 0)] $
      Opts.deflt

stackLineSpec ::
   (FormatValue term, Show term) => term -> String -> Plot2D.T x y -> Plot2D.T x y
stackLineSpec term colour =
   fmap (Graph2D.lineSpec (LineSpec.title (Format.unASCII $ formatValue term)
          (lineColour colour $ LineSpec.deflt)))

stack ::
   (FormatValue term, Num d, Ord d,
    Show term,
    Ord term,
    Atom.C d,
    Tuple.C d) =>
   M.Map term d -> Plot2D.T Int d
stack =
   foldMap
      (\(col, (term, val)) ->
         stackLineSpec term (Colour.unC col) $
           Plot2D.list Graph2D.histograms [val]) .
   Colour.adorn .
   reverse .
   Key.sort (abs . snd) .
   M.toList

stacksFrame ::
   String -> [Format.ASCII] -> Opts.T (Graph2D.T Int Double)
stacksFrame title vars =
   Opts.title title $
      Histogram.rowstacked $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      optKeyOutside $
      Opts.boxwidthAbsolute 0.9 $
      Opts.xTicks2d (zip (map Format.unASCII vars) [0..]) $
      Opts.deflt

stacks ::
   (Ord term,
    Atom.C d,
    Tuple.C d,
    Ord d,
    Num d,
    FormatValue term,
    Show term) =>
   M.Map term [d] -> Plot2D.T Int d
stacks =
   foldMap
      (\(col, (term, vals)) ->
         stackLineSpec term (Colour.unC col) $
         Plot2D.list Graph2D.histograms vals) .
   Colour.adorn .
   reverse .
   Key.sort (maximum . map abs . snd) .
   M.toList




-- | Plotting Surfaces -------------------------------------------------------------------------

{-
xyzBasic ::
   (TDisp t1, SV.Walker v1, SV.FromList v1, SV.Storage v1 x,
    TDisp t2, SV.Walker v2, SV.FromList v2, SV.Storage v2 y,
    TDisp t3, SV.Walker v3, SV.FromList v3, SV.Storage v3 y,

    Atom.C x, Tuple.C x, Fractional x,
    Atom.C y, Tuple.C y, Fractional y,
    Atom.C z, Tuple.C z, Fractional z ) =>
   (LineSpec.T -> LineSpec.T) ->
   (TC s1 t1 (Data (v2 :> v1 :> Nil) x)) ->
   (TC s2 t2 (Data (v4 :> v3 :> Nil) y)) ->
   (TC s3 t3 (Data (v6 :> v5 :> Nil) z)) ->
   Plot3D.T x y z
-}

{-
surfaceStyle ::
   (LineSpec.T -> LineSpec.T) -> Plot3D.T x y z -> Plot3D.T x y z
surfaceStyle opts =
   fmap $ Graph2D.lineSpec $
      opts $
      LineSpec.pointSize 0.1 $
      LineSpec.pointType 7 $
      LineSpec.lineWidth 1 $
      LineSpec.deflt

-}

xyzBasic opts x y z =
   -- (xyStyle opts) $ Plot2D.list Graph2D.lines $ zip (getData x) (getData y)
   Plot3D.mesh $ L.zipWith3 zip3 (getData x) (getData y) (getData z)
                            -- (S.toList2 x) (S.toList2 y) (S.toList2 z)

class
   (AxisLabel tcX, AxisLabel tcY, AxisLabel tcZ) =>
      Surface tcX tcY tcZ where
   surface ::
      (LineSpec.T -> LineSpec.T) -> 
      (Int -> String) ->
      tcX -> tcY -> tcZ -> Plot3D.T (Value tcX) (Value tcY) (Value tcZ)

instance
   ( Fractional x, Fractional y, Fractional z,
     SV.Walker v2, SV.Walker v1, SV.Walker v4, SV.Walker v3,
     SV.Walker v6, SV.Walker v5, SV.FromList v1,
     SV.Storage v1 x, SV.FromList v2, SV.Storage v2 (v1 x), TDisp t1,
    SV.FromList v3, SV.Storage v3 y, SV.FromList v4, SV.Storage v4 (v3 y), TDisp t2,
    SV.FromList v5, SV.Storage v5 z, SV.FromList v6, SV.Storage v6 (v5 z), TDisp t3,
    Atom.C x, Tuple.C x,
    Atom.C y, Tuple.C y,
    Atom.C z, Tuple.C z) =>

      Surface
         (TC s1 t1 (Data (v2 :> v1 :> Nil) x))
         (TC s2 t2 (Data (v4 :> v3 :> Nil) y))
         (TC s3 t3 (Data (v6 :> v5 :> Nil) z)) where

   surface opts _ = xyzBasic opts
{-
   surface _ _ x y z =
      Plot3D.mesh $
      L.zipWith3 zip3 (S.toList2 x) (S.toList2 y) (S.toList2 z)
-}

{-
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
         [TC s3 t3 (Data (v6 :> v5 :> Nil) z)] where

   surface _ _ x y zs =
      Plot3D.mesh $ Fold.foldMap (surface x y) zs
 
      --void $ Plot.plotSync DefaultTerm.cons $
      --   Frame.cons attrs (Fold.foldMap (surface x y) zs)




      Plot3D.mesh $
      L.zipWith3 zip3 (S.toList2 x) (S.toList2 y) (S.toList2 z)






surfaceList ::
  Plot.Surface tcX tcY tcZ =>
  String -> tcX -> tcY -> [tcZ] -> IO ()
surfaceList ti x y zs = do
   let attrs =
          Opts.title ti $
          Opts.xLabel (Plot.genAxLabel x) $
          Opts.yLabel (Plot.genAxLabel y) $
          Opts.grid True $
          Opts.size 1 1 $
          Opts.deflt
   void $ Plot.plotSync DefaultTerm.cons $
      Frame.cons attrs (Fold.foldMap (Plot.surface x y) zs)


-}