{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Data.Plot.D2 where

import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Plot as Plot

import qualified Data.Foldable as Foldable
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified EFA.Data.Plot as DataPlot
--import EFA.Utility(Caller,
--                 --  merror,(|>),
--                   ModuleName(..),FunctionName, genCaller)

data PlotData id info label a b =
  PlotData 
  {
    accessPlotInfo :: DataPlot.PlotInfo id info,  
    accessRange :: RangeInfo label a b, 
    accessPlot ::  Plot2D.T a b}

class ToPlotData odContainer info label vec a b where
  toPlotData :: Maybe id ->
             odContainer inst label (vec :: * -> *)  a b ->
             [PlotData id info label a b]

getId :: PlotData id info label a b -> Maybe id
getId plotData = DataPlot.accessId $ accessPlotInfo plotData

class GetRangeInfo d2data label vec a b where
  getRangeInfo ::
    (d2data :: * -> * -> (* -> *) -> * -> * -> *) typ label vec a b
--    d2data typ dim label vec a b
    -> RangeInfo label a b


data RangeInfo label a b = RangeInfo
  (DataPlot.AxisInfo label a)
  (DataPlot.AxisInfo label b) deriving Show


plotInfo2lineTitle :: (Show id, Show a) => DataPlot.PlotInfo id a -> (LineSpec.T -> LineSpec.T)
plotInfo2lineTitle (DataPlot.PlotInfo ident _)  =  LineSpec.title $ show ident

plotInfo3lineTitles :: (Show label, Show id,Show a) => Int -> PlotData id id label a b -> (LineSpec.T -> LineSpec.T)
plotInfo3lineTitles _ (PlotData info _ _) = plotInfo2lineTitle info

labledFrame ::
  (Ord a, Ord b, Show label, Show id, Atom.C a, Atom.C b) =>
  String -> [PlotData id id label a b] -> Opts.T (Graph2D.T a b)
labledFrame title xs =
  Opts.xLabel (DataPlot.makeAxisLabel ax) $
  Opts.yLabel (DataPlot.makeAxisLabelWithIds plotIds ax1) $
  Opts.title title $ defaultFrameAttr
  where
    RangeInfo ax ax1 = combineRangeList (head rs) (tail rs)
    rs = map f xs
    f (PlotData _ rangeInfo _) = rangeInfo
    plotIds = collectPlotIds xs

combineRangeList :: (Ord b, Ord a) => RangeInfo label a b -> [RangeInfo label a b] -> RangeInfo label a b
combineRangeList x xs = foldl combineRange x xs

defaultFrameAttr :: (Atom.C a, Atom.C b) => Opts.T (Graph2D.T a b)
defaultFrameAttr =
   Opts.add (Opt.custom "hidden3d" "") ["back offset 1 trianglepattern 3 undefined 1 altdiagonal bentover"] $
   Opts.grid True $
   Opts.deflt

collectPlotIds ::  (Show id) => [PlotData id id label a b] -> [Maybe id]
collectPlotIds xs = map f xs
  where   f (PlotData (DataPlot.PlotInfo x _) _ _) = x

combineRange :: (Ord a, Ord b) =>
  RangeInfo label a b ->
  RangeInfo label a b ->
  RangeInfo label a b
combineRange (RangeInfo x y) (RangeInfo x1 y1) =
  RangeInfo
  (DataPlot.combine x x1)
  (DataPlot.combine y y1)


allInOneIO ::(Terminal.C terminal, Atom.C b,Atom.C a)=>
  terminal ->
  ([PlotData info id label a b] ->  Opts.T (Graph2D.T a b)) ->
  (Int -> PlotData info id label a b -> (LineSpec.T -> LineSpec.T)) ->
  [PlotData info id label a b] ->
  IO()
allInOneIO terminal makeFrameStyle setGraphStyle xs =
  DataPlot.run terminal (makeFrameStyle xs) $ (Foldable.fold $ map g $ zip [0..] xs)
  where g (idx,plotData@(PlotData _ _ plot)) = fmap (Graph2D.lineSpec $ setGraphStyle idx plotData  $ LineSpec.deflt) plot

allInOne ::
  ([PlotData info id label a b] ->  Opts.T (Graph2D.T a b)) ->
  (Int -> PlotData info id label a b -> (LineSpec.T -> LineSpec.T)) ->
  [PlotData info id label a b] -> 
  Frame.T (Graph2D.T a b)
allInOne makeFrameStyle setGraphStyle xs = 
  Frame.cons  (makeFrameStyle xs) $ (Foldable.fold $ map g $ zip [0..] xs)
  where g (idx,plotData@(PlotData _ _ plot)) = fmap (Graph2D.lineSpec $ setGraphStyle idx plotData  $ LineSpec.deflt) plot