{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances#-}


module EFA.Signal.PlotNeu (
   run,
   signal, signalFrame, signalStyle, signalIO,
   xy, xyBasic, xyFrame, xyStyle, xyIO,
   surface, surfaceIO,
   recordBasic, recordStyle, recordFrame, recordIO, recordIOList,
   sequenceIO,
   recordSplitPlus, recordSplit, sequenceSplit,
   recordSelect, sequenceSelect,
   stack, stackFrame, stackIO,
   stacks, stacksFrame, stacksIO,
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

--import qualified Graphics.Gnuplot.Terminal.X11 as X11
--import qualified Graphics.Gnuplot.Terminal.WXT as WXT

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
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

-- | Different function to show plots   ---------------------------------------------------------------

run ::
   (Terminal.C term, Graph.C graph) =>
   term -> Opts.T graph -> Plt.T graph -> IO ()
run terminal opts plt =
   void $ Plot.plotSync terminal $ Frame.cons opts plt


(+++) :: (LineSpec.T -> LineSpec.T) -> (LineSpec.T -> LineSpec.T) -> (LineSpec.T -> LineSpec.T)
(+++) opts change = opts . change  

-- | Simple Signal Plotting -- plot signal values against signal index --------------------------------------------------------------


-- | Default Signal Plot
signalIO ::
   (Signal signal, Terminal.C term) =>
   String -> term -> (LineSpec.T -> LineSpec.T) -> signal -> IO ()
signalIO ti terminal opts x = run terminal (signalFrame ti x)  (signal opts x)

-- | Default Frame Options
signalFrame ::
   (AxisLabel tc, Graph.C graph) =>
   String -> tc -> Opts.T graph
signalFrame ti x =
   Opts.title ti $
   Opts.xLabel "Signal Index []" $
   Opts.yLabel (genAxLabel x) $
   Opts.grid True $
   Opts.deflt

-- | Default Signal Line Style
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

xyIO ::
   (XY tcX tcY, Terminal.C term) =>
   String -> term -> (LineSpec.T -> LineSpec.T)-> (Int -> String) -> tcX -> tcY -> IO ()
xyIO ti terminal opts legend x y =
   run terminal (xyFrame ti x y) (xy opts legend x y)

xyFrame ::
   (AxisLabel tcX, AxisLabel tcY, Graph.C graph) =>
   String -> tcX -> tcY -> Opts.T graph
xyFrame ti x y =
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
   xy :: (LineSpec.T -> LineSpec.T) -> (Int -> String) -> tcX -> tcY -> Plot2D.T (Value tcX) (Value tcY)

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
   xy opts legend = xyBasic ((LineSpec.title $ legend 0) +++ opts)

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
         (\ n y -> xyBasic ((LineSpec.title $ legend n) +++ opts) x y)
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
         (\ n x y -> xyBasic ((LineSpec.title $ legend n) +++ opts) x y)
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

recordIO ::
   (Fractional d,
    Show id, Ord id,
    SV.Walker v, SV.Storage v d, SV.FromList v,
    TDisp t2, TDisp t1,
    Tuple.C d, Atom.C d, 
    Terminal.C term) =>
   String -> 
   term -> 
   (LineSpec.T -> LineSpec.T) ->
   Record s1 s2 t1 t2 id v d d -> IO ()
recordIO ti term opts x =
   run term (recordFrame ti) (recordBasic opts x)

recordIOList ::
   (Fractional d,
    Show id, Ord id,
    SV.Walker v, SV.Storage v d, SV.FromList v,
    TDisp t2, TDisp t1,
    Tuple.C d, Atom.C d,
    Terminal.C term) =>
   String -> 
   term -> 
   (LineSpec.T -> LineSpec.T) ->
   [Record s1 s2 t1 t2 id v d d] -> IO ()
recordIOList ti term opts xs =
    run term (recordFrame ti) $ foldMap (recordBasic opts) xs
  
-- | Line Style
recordStyle :: (LineSpec.T -> LineSpec.T) -> Plot2D.T x y -> Plot2D.T x y   
recordStyle opts = 
   fmap $ Graph2D.lineSpec $
      opts $
      LineSpec.pointSize 0.3$
      LineSpec.pointType 1 $
      LineSpec.lineWidth 1.6 $
      LineSpec.deflt

-- | Plot Frameibutes
recordFrame ::
   (Graph.C graph) =>
   String -> Opts.T graph
recordFrame ti =
   Opts.title (ti) $
   Opts.grid True $
   Opts.xLabel ("Time [" ++ (show $ getDisplayUnit Typ_T) ++ "]") $
   Opts.yLabel ("")
   Opts.deflt

recordBasic ::
   (Show id, Ord id, TDisp typ0, TDisp typ1,
    SV.Walker v, SV.FromList v,
    SV.Storage v d, Fractional d, Atom.C d, Tuple.C d) =>
   (LineSpec.T -> LineSpec.T) -> 
   Record s1 s2 typ0 typ1 id v d d -> Plot2D.T d d
recordBasic opts (Record time pMap) =
   Fold.fold $
   M.mapWithKey
      (\key (col, sig) ->
         recordStyle (opts +++ (LineSpec.title $ show key) +++ (lineColour $ Colour.unC col)) $
         Plot2D.list Graph2D.linesPoints $
         zip (getData time) (getData sig)) $
   Colour.adorn pMap

--------------------------------------------
-- recordIO command to show max n signals per window

recordSplit ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional d,
    Tuple.C d, Atom.C d,
    SV.Walker v,
    SV.Storage v d,
    SV.FromList v, 
    Terminal.C term) =>
   Int -> 
   String -> 
   term ->
   (LineSpec.T -> LineSpec.T) -> 
   Record s1 s2 t1 t2 id v d d -> IO ()
recordSplit n ti term opts r =
   zipWithM_
      (\k -> recordIO (ti ++ " - Part " ++ show (k::Int)) term opts)
      [0..] (Record.split n r)

recordSplitPlus ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional d,
    Tuple.C d, Atom.C d,
    SV.Walker v,
    SV.Storage v d,
    SV.FromList v,
    Terminal.C term,
    SV.Len (v d)) =>
   Int -> 
   String -> 
   term ->
   (LineSpec.T -> LineSpec.T) -> 
   Record s1 s2 t1 t2 id v d d ->
   [(id, TC s2 t2 (Data (v :> Nil) d))] -> IO ()
recordSplitPlus n ti term opts r list =
   zipWithM_
      (\k -> recordIO (ti ++ " - Part " ++ show (k::Int)) term opts)
      [0 ..] (map (Record.addSignals list) (Record.split n r))

--------------------------------------------
-- recordIO command to plot selected Signals only

recordSelect ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional d,
    Tuple.C d, Atom.C d,
    SV.Walker v,
    SV.Storage v d,
    Terminal.C term,
    SV.FromList v) =>
   [id] -> 
   String -> 
   term ->
   (LineSpec.T -> LineSpec.T) ->    
   Record s1 s2 t1 t2 id v d d -> IO ()
recordSelect idList ti term opts = recordIO ti term opts . Record.extract idList

sequenceFrame ::
   (Fractional d,
    Show id, Ord id,
    SV.Walker v, SV.Storage v d, SV.FromList v,
    TDisp t2, TDisp t1,
    Tuple.C d, Atom.C d) =>
   String -> 
   (LineSpec.T -> LineSpec.T) -> 
   SequData (Record s1 s2 t1 t2 id v d d) ->
   SequData (Frame.T (Graph2D.T d d))
sequenceFrame ti opts =
   SD.mapWithSection
      (\x ->
         Frame.cons (recordFrame ("Sequence " ++ ti ++ ", Record of " ++ show x)) .
         recordBasic opts)

sequenceIO ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional d,
    Tuple.C d, Atom.C d,
    SV.Walker v,
    SV.Storage v d,
    SV.FromList v, 
    Terminal.C term) =>
   String -> 
   term ->
   (LineSpec.T -> LineSpec.T) -> 
   SequData (Record s1 s2 t1 t2 id v d d) -> IO ()
sequenceIO ti term opts =
   Fold.mapM_ (Plot.plotSync term) . sequenceFrame ti opts

sequenceSplit ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional d,
    Tuple.C d, Atom.C d,
    SV.Walker v,
    SV.Storage v d,
    Terminal.C term,
    SV.FromList v) =>
   Int -> 
   String -> 
   term ->
   (LineSpec.T -> LineSpec.T) -> 
   SequData (Record s1 s2 t1 t2 id v d d) -> IO ()
sequenceSplit n ti term opts =
   Fold.sequence_ .
   SD.mapWithSection (\ x -> recordSplit n (ti ++ " - " ++ show x) term opts)

sequenceSelect ::
   (TDisp t1, TDisp t2,
    Show id, Ord id,
    Fractional d,
    Tuple.C d, Atom.C d,
    SV.Walker v,
    SV.Storage v d,
    SV.FromList v, 
    Terminal.C term) =>
   [id] -> 
   String -> 
   term ->
   (LineSpec.T -> LineSpec.T) -> 
   SequData (Record s1 s2 t1 t2 id v d d) ->  IO ()
sequenceSelect idList ti term opts =
   sequenceIO ti term opts . fmap (Record.extract idList)


-- | Plotting Stacks ---------------------------------------------------------------

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

stack ::
   (FormatValue term, Show term, Ord term) =>
   M.Map term Double -> Plot2D.T Int Double
stack =
   foldMap
      (\(col, (term, val)) ->
         stackLineSpec term (Colour.unC col) $
           Plot2D.list Graph2D.histograms [val]) .
   Colour.adorn .
   reverse .
   Key.sort (abs . snd) .
   M.toList


stackIO ::
   (FormatValue term, Show term, Ord term) =>
   String -> Format.ASCII -> M.Map term Double -> IO ()
stackIO title var m =
   void .  Plot.plotSync DefaultTerm.cons . Frame.cons (stackFrame title var) . stack $ m

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
   (Ord term, FormatValue term, Show term) =>
   M.Map term [Double] -> Plot2D.T Int Double
stacks =
   foldMap
      (\(col, (term, vals)) ->
         stackLineSpec term (Colour.unC col) $
         Plot2D.list Graph2D.histograms vals) .
   Colour.adorn .
   reverse .
   Key.sort (maximum . map abs . snd) .
   M.toList

{- |
The length of @[var]@ must match the one of the @[Double]@ lists.
-}
stacksIO ::
   (Ord term, FormatValue term, Show term) =>
   String -> [Format.ASCII] -> M.Map term [Double] -> IO ()
stacksIO title vars xs =
   void . Plot.plotSync DefaultTerm.cons . -- Plot.plotDefault .
   Frame.cons (stacksFrame title vars) . stacks $ xs

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



-- | Plotting Surfaces ------------------------------------------------------------------------- 

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
   void $ Plot.plotSync DefaultTerm.cons $
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
