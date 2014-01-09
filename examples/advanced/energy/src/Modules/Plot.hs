{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Modules.Plot where

import Modules.System (Node)
import Modules.Setting (varRestPower, varLocalPower)
import qualified Modules.Utility as ModUt
import qualified Modules.Setting as ModSet
import qualified Modules.Types as Types

import qualified Modules.Optimisation as Optimisation

import qualified EFA.Application.Plot as AppPlot
import qualified EFA.Application.Sweep as Sweep

import qualified EFA.Application.OneStorage as One
import EFA.Application.Sweep (Sweep)

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as Vector

import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Draw as Draw
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State.SystemEta as StateEta

import qualified EFA.Graph as Graph

import EFA.Equation.Result (Result(Determined))
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Report.FormatValue as FormatValue

import EFA.Utility.Async (concurrentlyMany_)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.ColorSpecification as ColorSpec

import Data.GraphViz.Attributes.Colors.X11 (X11Color(LimeGreen, Orchid1))


import qualified Data.Map as Map; import Data.Map (Map)
import Data.GraphViz.Types.Canonical (DotGraph)
import Data.Text.Lazy (Text)
import qualified Data.Vector.Unboxed as UV
import Data.Vector (Vector)

import Data.Monoid ((<>), mconcat)
import Control.Applicative (liftA2)


frameOpts ::
  Opts.T (Graph3D.T Double Double Double) ->
  Opts.T (Graph3D.T Double Double Double)
frameOpts =
--  Plot.heatmap .
--  Plot.xyzrange3d (0.2, 2) (0.3, 3.3) (0, 1) .
  -- Plot.cbrange (0.2, 1) .
  Plot.xyzlabel "Local [W]" "Rest [W]" ""
  . Plot.missing "NaN"
  . Plot.paletteGH
  . Plot.depthorder



plotMaps ::
  (Show state,
   Plot.Surface
     (Sig.PSignal2 Vector Vector Double)
     (Sig.PSignal2 Vector Vector Double)
     tcZ,
   Plot.Value tcZ ~ Double) =>
  (a -> tcZ) ->
  String ->
  Map state a ->
  IO ()
plotMaps func title =
  concurrentlyMany_ . Map.elems . Map.mapWithKey f
  where f k mat =
          AppPlot.surfaceWithOpts (title ++ ", " ++ show k)
            DefaultTerm.cons
            id
            id
            frameOpts varRestPower varLocalPower
            (func mat)


plotSweeps ::
  (Show state,
   Plot.Surface
     (Sig.PSignal2 Vector Vector Double)
     (Sig.PSignal2 Vector Vector Double)
     tcZ,
   Plot.Value tcZ ~ Double) =>
  (a -> tcZ) ->
  String ->
  Map state a ->
  IO ()
plotSweeps func title =
  concurrentlyMany_ . Map.elems . Map.mapWithKey f
  where f k mat =
          AppPlot.surfaceWithOpts (title ++ ", " ++ show k)
            DefaultTerm.cons
            id
            (Graph3D.typ "lines")
            (Opts.key False . frameOpts) varRestPower varLocalPower
            (func mat)



plotMapOfMaps ::
  (Show state, Show k1) =>
  Map state (Map k1 (Sig.PSignal2 Vector Vector (Maybe (Result Double)))) -> IO ()
plotMapOfMaps =
  concurrentlyMany_
  . Map.elems
  . Map.mapWithKey (plotMaps (Sig.map ModUt.nothing2Nan) . show)

plotGraphMaps ::
  (FormatValue.FormatValue a, Show a) =>
  String ->
  Map [a] (Maybe (a, a, Optimisation.EnvResult a)) ->
  IO ()
plotGraphMaps title =
  sequence_ . Map.elems . Map.mapWithKey
    (\reqs -> maybe (return ())
        (\(objVal, eta, graph) ->
              Draw.xterm $ Draw.bgcolour Orchid1
                         $ Draw.title ( title
                                        ++ "\\lreqs " ++ show reqs
                                        ++ "\\lObjective Value " ++ show objVal
                                        ++ "\\leta " ++ show eta
                                        ++ "\\l")
                         $ Draw.stateFlowGraph Draw.optionsDefault graph))


plotGraphMapOfMaps ::
  (FormatValue.FormatValue a, Show a) =>
  Map Idx.State
      (Map [a] (Maybe (a, a, Optimisation.EnvResult a))) ->
  IO ()
plotGraphMapOfMaps =
  sequence_ . Map.elems . Map.mapWithKey (plotGraphMaps . show)

plotOptimalObjectivePerState ::
  (Show a, FormatValue.FormatValue a) =>
  Types.Optimisation sweep vec a -> IO ()
plotOptimalObjectivePerState =
  plotGraphMapOfMaps . Types.optimalObjectivePerState . Types.quasiStationary

plotPerEdge ::
 (Vector.Walker l, Vector.Storage l Double, Vector.FromList l) =>
  ModSet.Params Node list sweep vec Double ->
  Record.PowerRecord Node l Double ->
  IO ()
plotPerEdge params rec = do
  let recs = map f $ Graph.edges $ One.systemTopology params
      f (Graph.DirEdge fr to) =
        Record.extract [TopoIdx.ppos fr to, TopoIdx.ppos to fr] rec
  concurrentlyMany_ $
    map (AppPlot.record "plotSimulationPerEdge" DefaultTerm.cons show id) recs

plotSimulationSignalsPerEdge ::
  ModSet.Params Node list sweep vec Double ->
  Types.Optimisation sweep vec Double ->
  IO ()
plotSimulationSignalsPerEdge params =
  plotPerEdge params . Types.signals . Types.simulation


plotSimulationSignals ::
  ModSet.Params Node list sweep vec Double ->
  Types.Optimisation sweep vec Double ->
  IO ()
plotSimulationSignals _ =
  AppPlot.record "Signals" DefaultTerm.cons show id
  . Types.signals
  . Types.simulation

to2DMatrix :: (Ord b) => Map [b] a -> Sig.PSignal2 Vector Vector a
to2DMatrix = ModUt.to2DMatrix

{-
surfaceWithOptsWithGrid ::
  (Plot.Surface tcX tcY tcZ1) =>
  String -> term -> (LineSpec.T -> LineSpec.T) ->
  ( Opts.T graph ->
    Opts.T (Graph3D.T (Plot.Value tcX) (Plot.Value tcY) (Plot.Value tcZ1))) ->
  tcZ1 -> tcX -> tcY -> tcZ -> IO ()
surfaceWithOptsWithGrid ti terminal opts fopts grid x y z =
  Plot.run terminal
    (fopts $ Plot.xyFrameAttr ti x y)
    (Plot.surface opts x y z <> Plot.surface opts x y grid)
-}

defaultPlot :: String -> Sig.PSignal2 Vector Vector Double -> IO ()
defaultPlot title =
  AppPlot.surfaceWithOpts
    title DefaultTerm.cons id id frameOpts varRestPower varLocalPower

{-
defaultPlotWithGrid ::
  String -> Sig.PSignal2 Vector Vector Double -> Sig.PSignal2 Vector Vector Double -> IO ()
defaultPlotWithGrid title grid =
  surfaceWithOptsWithGrid
    title DefaultTerm.cons id frameOpts grid varRestPower varLocalPower
-}

withFuncToMatrix ::
  (Ord b, Arith.Constant a) =>
  ((b, b, Idx.State, Optimisation.EnvResult b) -> a) ->
  Types.Optimisation sweep vec b -> Sig.PSignal2 Vector Vector a
withFuncToMatrix func =
  to2DMatrix
  . Map.map (maybe ModUt.nan func)
  . Types.optimalState
  . Types.quasiStationary

plotMax ::
  String ->
  ((Double, Double, Idx.State, Optimisation.EnvResult Double) -> Double) ->
  Types.Optimisation sweep vec Double ->
  IO ()
plotMax title func =
  defaultPlot title
  . withFuncToMatrix func


{-
plotMax2 ::
  String ->
  ((Double, Double, Idx.State, Optimisation.EnvResult Double) -> Double) ->
  Types.Optimisation sweep vec Double ->
  IO ()
plotMax2 title func res =
  let grid = bestStateCurve res
  in defaultPlotWithGrid title grid
     $ withFuncToMatrix func res
-}

plotMaxEta :: Types.Optimisation sweep vec Double -> IO ()
plotMaxEta = plotMax "Maximal Eta of All States" ModUt.snd4

plotMaxObj :: Types.Optimisation sweep vec Double -> IO ()
plotMaxObj = plotMax "Maximal Objective of All States" ModUt.fst4

bestStateCurve ::
  (Ord b, Arith.Constant a, Num a) =>
  Types.Optimisation sweep vec b -> Sig.PSignal2 Vector Vector a
bestStateCurve =
  withFuncToMatrix ((\(Idx.State state) -> fromIntegral state) . ModUt.thd4)

plotMaxState :: Types.Optimisation sweep vec Double -> IO ()
plotMaxState =
  defaultPlot "Best State of All States"
  . bestStateCurve

plotMaxStateContour :: Types.Optimisation sweep vec Double -> IO ()
plotMaxStateContour =
  AppPlot.surfaceWithOpts
    "Best State of All States"
    DefaultTerm.cons id id (Plot.contour . frameOpts) varRestPower varLocalPower
  . bestStateCurve

plotMaxPerState ::
  String ->
  (Map Idx.State (Map [Double] (Maybe (Double, Double, Optimisation.EnvResult Double)))
     -> Map Idx.State (Map [Double] Double)) ->
  Types.Optimisation sweep vec Double ->
  IO ()
plotMaxPerState title func =
  plotMaps id title
  . Map.map to2DMatrix
  . func
  . Types.optimalObjectivePerState
  . Types.quasiStationary

plotMaxObjPerState ::
  Types.Optimisation sweep vec Double ->
  IO ()
plotMaxObjPerState = plotMaxPerState "Maximal Objective Per State" ModUt.getMaxObj

plotMaxEtaPerState ::
  Types.Optimisation sweep vec Double ->
  IO ()
plotMaxEtaPerState = plotMaxPerState "Maximal Eta Per State" ModUt.getMaxEta


plotMaxPosPerState ::
  (Show (qty Node), Show part, StateQty.Lookup (Idx.InPart part qty)) =>
  Idx.InPart part qty Node -> Types.Optimisation sweep vec Double -> IO ()
plotMaxPosPerState pos =
  plotMaxPerState
    ("Maximal Position " ++ show pos ++ " per state")
    (ModUt.getMaxPos pos)

matrix2ListOfMatrices ::
  Int -> Sig.PSignal2 Vector Vector [a] -> [Sig.PSignal2 Vector Vector a]
matrix2ListOfMatrices len = zipWith g [0 .. len-1] . repeat
  where g idx m = Sig.map (!! idx) m


sweepResultTo2DMatrix ::
  (Ord b, Sweep.SweepClass Sweep vec a, Arith.Constant a) =>
  Int ->
  Map [b] (Result (Sweep vec a)) -> Sig.PSignal2 Vector Vector (Sweep vec a)
sweepResultTo2DMatrix len = Sig.map f . ModUt.to2DMatrix
  where f (Determined x) = x
        f _ = Sweep.fromRational len ModUt.nan


plotPerStateSweep ::
  (Arith.Product (Sweep vec Double), Sweep.SweepVector vec Double, Show (vec Double),
   Sweep.SweepClass Sweep vec Double) =>
  Int -> String -> Types.Optimisation Sweep vec Double -> IO ()
plotPerStateSweep len title =
  plotSweeps id title
  . Map.map (matrix2ListOfMatrices len . Sig.map Sweep.toList . sweepResultTo2DMatrix len)
  . Map.map (Map.map StateEta.etaSys)
  . Types.perStateSweep
  . Types.quasiStationary


findTile :: Ord t => [t] -> [t] -> t -> t -> [(t, t)]
findTile xs ys x y =
  let (xa, xb) = findInterval x xs
      (ya, yb) = findInterval y ys

      findInterval :: (Ord a) => a -> [a] -> (a, a)
      findInterval z zs = (last as, head bs)
        where (as, bs) = span (<=z) zs

      sort [(x0, y0), (x1, y1), (x2, y2), (x3, y3)] =
           [(x0, y0), (x2, y2), (x3, y3), (x1, y1), (x0, y0)]
      sort _ = error "findTile: sort failed"

  in sort $ liftA2 (,) [xa, xb] [ya, yb]


plotReqs ::
  Sig.PSignal Vector Double ->
  Sig.PSignal Vector Double ->
  IO ()
plotReqs prest plocal = do
  let rs = Sig.toList prest
      ls = Sig.toList plocal

      ts :: [([Double], [Double])]
      ts = map unzip $ zipWith (findTile ModSet.rest ModSet.local) rs ls

{-
      xsSig, ysSig :: Sig.PSignal Vector Double
      xsSig = Sig.fromList xs
      ysSig = Sig.fromList ys
-}
      sigStyle _ =
        LineSpec.pointSize 1 $
        LineSpec.pointType 7 $
        LineSpec.lineWidth 1 $
          LineSpec.lineColor ColorSpec.red $
        LineSpec.deflt

      tileStyle _ =
          LineSpec.pointSize 5 $
          LineSpec.pointType 5 $
          LineSpec.lineWidth 5 $
          LineSpec.lineColor ColorSpec.springGreen $
          LineSpec.deflt

      f (xs, ys) =
        let
            xsSig, ysSig :: Sig.PSignal Vector Double
            xsSig = Sig.fromList xs
            ysSig = Sig.fromList ys

        in Plot.xy tileStyle xsSig [AppPlot.label "" ysSig]


  Plot.run DefaultTerm.cons (Plot.xyFrameAttr "Reqs" prest plocal)
    ( (mconcat $ map f ts)
      <> Plot.xy sigStyle [prest] [AppPlot.label "Power" plocal])


plotSimulationGraphs ::
  (FormatValue.FormatValue b, UV.Unbox b,
   Sweep.SweepClass sweep vec b, Sweep.SweepVector vec b) =>
  (String -> DotGraph Text -> IO ()) -> Types.Optimisation sweep vec b -> IO ()
plotSimulationGraphs terminal (Types.Optimisation _ sim) = do
  let g = fmap (head . Sweep.toList)


  terminal "seq_"
    $ Draw.bgcolour LimeGreen
    $ Draw.title "Sequence Flow Graph from Simulation"
    $ Draw.seqFlowGraph Draw.optionsDefault (Types.sequenceFlowGraph sim)


  terminal "state_"
    $ Draw.bgcolour Orchid1
    $ Draw.title "State Flow Graph from Simulation"
    $ Draw.stateFlowGraph Draw.optionsDefault
    $ StateQty.mapGraph g g (Types.stateFlowGraph sim)

