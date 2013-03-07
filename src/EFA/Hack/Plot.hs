module EFA.Hack.Plot where
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified EFA.Graph.Topology.Index as Idx

import EFA.Report.FormatValue (FormatValue, formatValue)
import qualified Graphics.Gnuplot.Advanced as AGP

import qualified EFA.Report.Format as Format
import qualified EFA.Equation.Variable as Var

import qualified EFA.Graph.Topology.Node as TN 

import Control.Functor.HT (void)
import qualified Data.Foldable as Fold

-- | Draw a histogram of a flow change stack

{-
histogram ::
   (Fold.Foldable f, FormatValue term) =>
   (Idx.Energy node) ->  f (term, Double) ->  Frame.T (Graph2D.T Int Double)
histogram key =
   Frame.cons (
      Opts.title "Decomposition of total output energy" $
      Histogram.rowstacked $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      Opts.xTicks2d [(Format.unASCII $ formatValue $
                      Idx.delta $ Var.mkIdx key, 0)] $
      Opts.xRange2d (-1,3) $
      Opts.deflt) .
   Fold.foldMap (\(term,val) ->
      fmap (Graph2D.lineSpec
              (LineSpec.title (Format.unASCII $ formatValue term) LineSpec.deflt)) $
      Plot2D.list Graph2D.histograms [val])
-}   



histogram ::
   (Fold.Foldable f, 
    FormatValue term, 
    TN.C node) =>
   (Idx.Energy node) -> f (term, Double) -> Frame.T (Graph2D.T Int Double)
histogram key =
   Frame.cons (
      Opts.title "Decomposition of total output energy" $
      Histogram.rowstacked $
      OptsStyle.fillBorderLineType (-1) $
      OptsStyle.fillSolid $
      Opts.xTicks2d [(Format.unASCII $ formatValue $
                      Idx.delta $ Var.index key, 0)] $
      Opts.xRange2d (-1,3) $
      Opts.deflt) .
   Fold.foldMap (\(term,val) ->
      fmap (Graph2D.lineSpec
              (LineSpec.title (Format.unASCII $ formatValue term) LineSpec.deflt)) $
      Plot2D.list Graph2D.histograms [val])


histogrammIO :: (Fold.Foldable f, TN.C node, FormatValue term) =>
                f (term, Double) -> Idx.Energy node -> IO ()
histogrammIO  stack key = do
  void $ AGP.plotDefault $ histogram key stack



