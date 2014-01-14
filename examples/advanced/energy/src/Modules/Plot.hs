{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Modules.Plot where

import Modules.System (Node)
import Modules.Setting (varRestPower, varLocalPower)
import qualified Modules.Utility as ModUt
import qualified Modules.Setting as ModSet
import qualified Modules.Types as Types

import qualified EFA.Application.Plot as AppPlot
import qualified EFA.Application.Sweep as Sweep

import qualified EFA.Application.OneStorage as One

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
import qualified EFA.Graph.Topology.Node as Node

import EFA.Equation.Result (Result(Determined))
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Report.FormatValue as FormatValue

import EFA.Utility.Async (concurrentlyMany_)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.ColorSpecification as ColorSpec
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom

import Data.GraphViz.Attributes.Colors.X11 (X11Color(LimeGreen, Orchid1))


import qualified Data.Map as Map; import Data.Map (Map)
import Data.GraphViz.Types.Canonical (DotGraph)
import Data.Text.Lazy (Text)
import qualified Data.Vector.Unboxed as UV
import Data.Vector (Vector)

import Data.Monoid ((<>), mconcat)
import Control.Applicative (liftA2)


frameOpts ::
  (Atom.C a) =>
  Opts.T (Graph3D.T a a a) ->
  Opts.T (Graph3D.T a a a)
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


{-
plotMapOfMaps ::
  (Show state, Show k, Tuple.C a, Atom.C a, Arith.Constant a) =>
  Map state (Map k (Sig.PSignal2 Vector Vector (Maybe (Result a)))) -> IO ()
-}
plotMapOfMaps ::
  (Show state, Show k) =>
  Map state (Map k (Sig.PSignal2 Vector Vector (Maybe (Result Double)))) ->
  IO ()
plotMapOfMaps =
  concurrentlyMany_
  . Map.elems
  . Map.mapWithKey (plotMaps (Sig.map ModUt.nothing2Nan) . show)

plotGraphMaps ::
  (FormatValue.FormatValue a, Show a, Node.C node) =>
  String ->
  Map [a] (Maybe (a, a, Types.EnvResult node a)) ->
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
  (FormatValue.FormatValue a, Show a, Node.C node) =>
  Map Idx.State
      (Map [a] (Maybe (a, a, Types.EnvResult node a))) ->
  IO ()
plotGraphMapOfMaps =
  sequence_ . Map.elems . Map.mapWithKey (plotGraphMaps . show)

plotOptimalObjectivePerState ::
  (Show a, FormatValue.FormatValue a, Node.C node) =>
  Types.Optimisation node sweep vec a -> IO ()
plotOptimalObjectivePerState =
  plotGraphMapOfMaps . Types.optimalObjectivePerState . Types.quasiStationary

plotPerEdge ::
  (Vector.Walker l, Vector.Storage l a, Vector.FromList l,
   Arith.Constant a,
   Tuple.C a, Atom.C a,
   Node.C node, Show node) =>
  One.OptimalEnvParams node list sweep vec a ->
  Record.PowerRecord node l a ->
  IO ()
plotPerEdge params rec = do
  let recs = map f $ Graph.edges $ One.systemTopology params
      f (Graph.DirEdge fr to) =
        Record.extract [TopoIdx.ppos fr to, TopoIdx.ppos to fr] rec
  concurrentlyMany_ $
    map (AppPlot.record "plotSimulationPerEdge" DefaultTerm.cons show id) recs

plotSimulationSignalsPerEdge ::
  (Show node, Node.C node, Arith.Constant a, Tuple.C a, Atom.C a) =>
  One.OptimalEnvParams node list sweep vec a ->
  Types.Optimisation node sweep vec a ->
  IO ()
plotSimulationSignalsPerEdge params =
  plotPerEdge params . Types.signals . Types.simulation


plotSimulationSignals ::
  (Show node, Ord node,
   Arith.Constant a,
   Tuple.C a, Atom.C a) =>
  One.OptimalEnvParams Node list sweep vec a ->
  Types.Optimisation node sweep vec a ->
  IO ()
plotSimulationSignals _ =
  AppPlot.record "Signals" DefaultTerm.cons show id
  . Types.signals
  . Types.simulation

to2DMatrix :: (Ord b) => Map [b] a -> Sig.PSignal2 Vector Vector a
to2DMatrix = ModUt.to2DMatrix



defaultPlot :: String -> Sig.PSignal2 Vector Vector Double -> IO ()
defaultPlot title =
  AppPlot.surfaceWithOpts
    title DefaultTerm.cons id id frameOpts varRestPower varLocalPower

withFuncToMatrix ::
  (Ord b, Arith.Constant a) =>
  ((b, b, Idx.State, Types.EnvResult node b) -> a) ->
  Types.Optimisation node sweep vec b -> Sig.PSignal2 Vector Vector a
withFuncToMatrix func =
  to2DMatrix
  . Map.map (maybe ModUt.nan func)
  . Types.optimalState
  . Types.quasiStationary

plotMax ::
  String ->
  ((Double, Double, Idx.State, Types.EnvResult node Double) -> Double) ->
  Types.Optimisation node sweep vec Double ->
  IO ()
plotMax title func =
  defaultPlot title
  . withFuncToMatrix func


plotMaxEta :: Types.Optimisation node sweep vec Double -> IO ()
plotMaxEta = plotMax "Maximal Eta of All States" ModUt.snd4

plotMaxObj :: Types.Optimisation node sweep vec Double -> IO ()
plotMaxObj = plotMax "Maximal Objective of All States" ModUt.fst4

bestStateCurve ::
  (Ord b, Arith.Constant a, Num a) =>
  Types.Optimisation node sweep vec b -> Sig.PSignal2 Vector Vector a
bestStateCurve =
  withFuncToMatrix ((\(Idx.State state) -> fromIntegral state) . ModUt.thd4)

plotMaxState :: Types.Optimisation node sweep vec Double -> IO ()
plotMaxState =
  defaultPlot "Best State of All States"
  . bestStateCurve

plotMaxStateContour ::
  (Ord a) => Types.Optimisation node sweep vec a -> IO ()
plotMaxStateContour =
  AppPlot.surfaceWithOpts
    "Best State of All States"
    DefaultTerm.cons id id (Plot.contour . frameOpts) varRestPower varLocalPower
  . bestStateCurve

{-
plotMaxPerState ::
  (Ord a, Arith.Constant a, Tuple.C a, Atom.C a) =>
  String ->
  (Map Idx.State
       (Map [a] (Maybe (a, a, Types.EnvResult node a)))
     -> Map Idx.State (Map [a] a)) ->
  Types.Optimisation node sweep vec a ->
  IO ()
-}
plotMaxPerState ::
  String ->
  (Map Idx.State
       (Map [Double] (Maybe (Double, Double, Types.EnvResult node Double)))
     -> Map Idx.State (Map [Double] Double)) ->
  Types.Optimisation node sweep vec Double ->
  IO ()
plotMaxPerState title func =
  plotMaps id title
  . Map.map to2DMatrix
  . func
  . Types.optimalObjectivePerState
  . Types.quasiStationary

plotMaxObjPerState ::
--  (Arith.Constant a, Ord a, Tuple.C a, Atom.C a) =>
  Types.Optimisation node sweep vec Double -> IO ()
plotMaxObjPerState =
  plotMaxPerState "Maximal Objective Per State" ModUt.getMaxObj

plotMaxEtaPerState ::
--  (Arith.Constant a, Ord a, Tuple.C a, Atom.C a) =>
  Types.Optimisation node sweep vec Double -> IO ()
plotMaxEtaPerState =
  plotMaxPerState "Maximal Eta Per State" ModUt.getMaxEta


plotMaxPosPerState ::
  (-- Arith.Constant a, Ord a, Tuple.C a, Atom.C a,
   Show (qty node), Ord node,
   Show part, StateQty.Lookup (Idx.InPart part qty)) =>
  Idx.InPart part qty node -> Types.Optimisation node sweep vec Double -> IO ()
plotMaxPosPerState pos =
  plotMaxPerState
    ("Maximal Position " ++ show pos ++ " per state")
    (ModUt.getMaxPos pos)

matrix2ListOfMatrices ::
  Int -> Sig.PSignal2 Vector Vector [a] -> [Sig.PSignal2 Vector Vector a]
matrix2ListOfMatrices len = zipWith g [0 .. len-1] . repeat
  where g idx m = Sig.map (!! idx) m


sweepResultTo2DMatrix ::
  (Ord b, Sweep.SweepClass sweep vec a, Arith.Constant a) =>
  Int ->
  Map [b] (Result (sweep vec a)) -> Sig.PSignal2 Vector Vector (sweep vec a)
sweepResultTo2DMatrix len = Sig.map f . ModUt.to2DMatrix
  where f (Determined x) = x
        f _ = Sweep.fromRational len ModUt.nan


plotPerStateSweep ::
  (--Ord a, Arith.Constant a, UV.Unbox a,
   Show (vec Double),
   Node.C node,
   Arith.Product (sweep vec Double),
   Sweep.SweepVector vec Double,
   Sweep.SweepClass sweep vec Double) =>
  Int -> String -> Types.Optimisation node sweep vec Double -> IO ()
plotPerStateSweep len title =
  plotSweeps id title
  . Map.map (matrix2ListOfMatrices len
             . Sig.map Sweep.toList
             . sweepResultTo2DMatrix len)
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
   Node.C node,
   Sweep.SweepClass sweep vec b,
   Sweep.SweepVector vec b) =>
  (String -> DotGraph Text -> IO ()) ->
  Types.Optimisation node sweep vec b ->
  IO ()
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

