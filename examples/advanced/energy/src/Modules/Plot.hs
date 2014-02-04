{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Modules.Plot where

import Modules.Setting (varRestPower, varLocalPower)
import qualified Modules.Utility as ModUt
import qualified Modules.Setting as ModSet
import qualified Modules.System as System

import qualified EFA.Application.Plot as AppPlot
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.Type as Type
import qualified EFA.Application.OneStorage as One

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as Vector

import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Draw as Draw
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.State.Index as StateIdx

import qualified EFA.Graph as Graph
import qualified EFA.Graph.Topology.Node as Node

import EFA.Equation.Result (Result(Determined))
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Report.FormatValue as FormatValue

import EFA.Signal.Plot (label)

import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility.List (vhead, vlast)
import EFA.Utility.Filename (filename, Filename)
import EFA.Utility.Show (showEdge, showNode)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Graphics.Gnuplot.Terminal.PNG as PNG
import qualified Graphics.Gnuplot.Terminal.SVG as SVG
import qualified Graphics.Gnuplot.Terminal.PostScript as PS

import qualified Graphics.Gnuplot.Terminal as Terminal

import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.ColorSpecification as ColorSpec
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom as Atom

import Data.GraphViz.Attributes.Colors.X11 (X11Color(DarkSeaGreen2, Lavender))


import qualified Data.Map as Map; import Data.Map (Map)
import Data.GraphViz.Types.Canonical (DotGraph)
import Data.Text.Lazy (Text)
import qualified Data.Vector.Unboxed as UV
import Data.Vector (Vector)
import Data.Tuple.HT (fst3, snd3, thd3)

import Data.Monoid ((<>), mconcat)
import Control.Applicative (liftA2)

import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>), (<.>))

import Text.Printf (printf)

frameOpts ::
  (Atom.C a, Fractional a, Tuple.C a) =>
--  Opts.T (Graph3D.T a a a) -> Opts.T (Graph3D.T a a a)) ->
  Opts.T (Graph3D.T a a a) ->
  Opts.T (Graph3D.T a a a)
frameOpts =
--  Plot.heatmap .
  -- Plot.xyzrange3d (1.9, 3) (0.1, 1.1) (0.16, 0.31) .
  -- Plot.cbrange (0.2, 1) .

  Plot.xyzlabelnode
      (Just System.LocalRest)
      (Just System.Rest)
      Nothing
  . Plot.missing "NaN"
  . Plot.paletteGH
  . Plot.depthorder

plotMaps ::
  (Filename state, Show state,
   Terminal.C term,
   Plot.Surface
     (Sig.PSignal2 Vector Vector Double)
     (Sig.PSignal2 Vector Vector Double)
     tcZ,
   Plot.Value tcZ ~ Double) =>
  (FilePath -> IO term) ->
  (a -> tcZ) ->
  String ->
  Map state a ->
  IO ()
plotMaps terminal func title =
  concurrentlyMany_ . Map.elems . Map.mapWithKey f
  where f state mat = do
          let str = filename (title, state)
          t <- terminal str
          AppPlot.surfaceWithOpts
            (title ++ ", " ++ show state)
            t
            id
            -- id
            (Graph3D.typ "lines")
            frameOpts varRestPower varLocalPower
            (func mat)


plotSweeps ::
  (Filename state, Show state,
   Terminal.C term,
   Plot.Surface
     (Sig.PSignal2 Vector Vector Double)
     (Sig.PSignal2 Vector Vector Double)
     tcZ,
   Plot.Value tcZ ~ Double) =>
  (FilePath -> IO term) ->
  (a -> tcZ) ->
  String ->
  Map state a ->
  IO ()
plotSweeps terminal func title =
  concurrentlyMany_ . Map.elems . Map.mapWithKey f
  where f state mat = do
          let str = filename (title, state)
          t <- terminal str
          AppPlot.surfaceWithOpts
            (title ++ ", " ++ show state)
            t
            id
            (Graph3D.typ "lines")
            (Opts.key False . frameOpts) varRestPower varLocalPower
            (func mat)



plotMapOfMaps ::
  (Show a, Terminal.C term, Show state, Filename state) =>
  (FilePath -> IO term) ->
  Map a (Map state (Sig.PSignal2 Vector Vector (Maybe (Result Double)))) ->
  IO ()
plotMapOfMaps terminal =
  concurrentlyMany_
  . Map.elems
  . Map.mapWithKey (plotMaps terminal (Sig.map ModUt.nothing2Nan) . show)

plotGraphMaps ::
  (FormatValue.FormatValue a, Show a, Filename [a], Node.C node) =>
  (String -> DotGraph Text -> IO ()) ->
  String ->
  Map [a] (Maybe (a, a, Type.EnvResult node a)) ->
  IO ()
plotGraphMaps terminal title =
  sequence_ . Map.elems . Map.mapWithKey
    (\reqs -> maybe (return ())
        (\(objVal, eta, graph) -> do
              let str = filename title </> filename reqs 
              terminal str
                   $ Draw.bgcolour Lavender
                   $ Draw.title (title ++ "\\lreqs " ++ show reqs
                                       ++ "\\lObjective Value " ++ show objVal
                                       ++ "\\leta " ++ show eta
                                       ++ "\\l")
                   $ Draw.stateFlowGraph Draw.optionsDefault graph))


plotGraphMapOfMaps ::
  (FormatValue.FormatValue a, Show a, Filename [a], Node.C node) =>
  (String -> DotGraph Text -> IO ()) ->
  Map Idx.State (Map [a] (Maybe (a, a, Type.EnvResult node a))) ->
  IO ()
plotGraphMapOfMaps terminal =
  sequence_ . Map.elems . Map.mapWithKey (plotGraphMaps terminal . show)

optimalObjectivePerState ::
  (Show a, FormatValue.FormatValue a, Filename [a], Node.C node) =>
  (String -> DotGraph Text -> IO ()) ->
  Type.Optimisation node sweep vec a -> IO ()
optimalObjectivePerState terminal =
  plotGraphMapOfMaps terminal . Type.optimalObjectivePerState . Type.quasiStationary

perEdge ::
  (Vector.Walker l, Vector.Storage l a, Vector.FromList l,
   Arith.Constant a,
   Tuple.C a, Atom.C a,
   Terminal.C term,
   Node.C node, Show node, Filename node) =>
  (FilePath -> IO term) ->
  One.OptimalEnvParams node list sweep vec a ->
  Record.PowerRecord node l a ->
  IO ()
perEdge terminal params rec =
  let recs = map f $ Graph.edges $ One.systemTopology params
      f (Graph.DirEdge fr to) =
        Record.extract [TopoIdx.ppos fr to, TopoIdx.ppos to fr] rec
      g r = do
        let ti = "Simulation Per Edge"
            str = filename ti </> (filename $ Map.keys $ Record.recordSignalMap r)
        t <- terminal str
        AppPlot.record ti t showEdge id r
  in concurrentlyMany_ $ map g recs

simulationSignalsPerEdge ::
  (Show node, Node.C node, Filename node,
   Arith.Constant a, Tuple.C a, Atom.C a,
   Terminal.C term) =>
  (FilePath -> IO term) ->
  One.OptimalEnvParams node list sweep vec a ->
  Type.Optimisation node sweep vec a ->
  IO ()
simulationSignalsPerEdge terminal params =
  perEdge terminal params . Type.signals . Type.simulation

record :: 
  (Ord node, Node.C node, Terminal.C term,
   Vector.Walker l, Vector.Storage l a,
   Vector.FromList l, Arith.Constant a,
   Tuple.C a, Atom.C a) =>
  (FilePath -> IO term) ->
  String -> Record.PowerRecord node l a -> IO ()
record terminal ti rec = do
  t <- terminal $ filename ti
  AppPlot.record ti t showEdge id rec

simulationSignals ::
  (Show node, Ord node, Node.C node,
   Terminal.C term,
   Arith.Constant a,
   Tuple.C a, Atom.C a) =>
  (FilePath -> IO term) ->
  Type.Optimisation node sweep vec a ->
  IO ()
simulationSignals terminal opt = do
  let str = "Simulation Signals"
  t <- terminal $ filename str
  AppPlot.record str t showEdge id
    $ Type.signals
    $ Type.simulation opt

givenSignals ::
  (Show node, Ord node, Node.C node,
   Terminal.C term,
   Arith.Constant a,
   Tuple.C a, Atom.C a) =>
  (FilePath -> IO term) ->
  Type.Optimisation node sweep vec a ->
  IO ()
givenSignals terminal opt = do
  let str = "Given Signals"
  t <- terminal $ filename str
  AppPlot.record str t showEdge id
    $ Type.givenSignals
    $ Type.simulation opt


to2DMatrix :: (Ord b) => Map [b] a -> Sig.PSignal2 Vector Vector a
to2DMatrix = ModUt.to2DMatrix

m2n :: (Arith.Constant a) => Maybe a -> a
m2n Nothing = ModUt.nan
m2n (Just x) = x

defaultPlot ::
  (Terminal.C term) =>
  IO term -> String -> Sig.PSignal2 Vector Vector Double -> IO ()
defaultPlot terminal title xs = do
  t <- terminal
  AppPlot.surfaceWithOpts
--    title t (LineSpec.title "") (Graph3D.typ "lines") frameOpts varRestPower varLocalPower xs
    title t (LineSpec.title "") id frameOpts varRestPower varLocalPower xs

withFuncToMatrix ::
  (Ord b, Arith.Constant a) =>
  ((b, b, Idx.State, Type.EnvResult node b) -> a) ->
  Type.Optimisation node sweep vec b -> Sig.PSignal2 Vector Vector a
withFuncToMatrix func =
  to2DMatrix
  . Map.map (maybe ModUt.nan func)
  . Type.optimalState
  . Type.quasiStationary



plotMax ::
  (Terminal.C term) =>
  IO term ->
  String ->
  ((Double, Double, Idx.State, Type.EnvResult node Double) -> Double) ->
  Type.Optimisation node sweep vec Double ->
  IO ()
plotMax term title func =
  defaultPlot term title
  . withFuncToMatrix func


maxPos ::
  (Ord node, Show node, Filename node, Node.C node,
   Terminal.C term) =>
  TopoIdx.Position node -> 
  (FilePath -> IO term) ->
  Type.Optimisation node sweep vec Double -> IO ()
maxPos pos@(TopoIdx.Position f t) terminal =
  plotMax (terminal $ filename ("maxPos", pos))
          ("Maximal Value for: " ++ showEdge pos)
          (\(_, _, st, env) -> g $ StateQty.lookup (StateIdx.power st f t) env)
  where g (Just (Determined x)) = x
        g _ = ModUt.nan

maxEta ::
  (Terminal.C term) =>
  (FilePath -> IO term) -> Type.Optimisation node sweep vec Double -> IO ()
maxEta term =
  plotMax (term "maxEta") "Maximal Eta of All States" 
    ModUt.snd4

maxObj ::
  (Terminal.C term) =>
  (FilePath -> IO term) -> Type.Optimisation node sweep vec Double -> IO ()
maxObj term =
  plotMax (term "maxObj") "Maximal Objective of All States" ModUt.fst4

bestStateCurve ::
  (Ord b, Arith.Constant a, Num a) =>
  Type.Optimisation node sweep vec b -> Sig.PSignal2 Vector Vector a
bestStateCurve =
  withFuncToMatrix ((\(Idx.State state) -> fromIntegral state) . ModUt.thd4)

maxState ::
  (Terminal.C term) =>
  (FilePath -> IO term) -> Type.Optimisation node sweep vec Double -> IO ()
maxState term =
  defaultPlot (term "maxState") "Best State of All States"
  . bestStateCurve

maxStateContour ::
  (Terminal.C term, Ord a) =>
  (FilePath -> IO term) -> Type.Optimisation node sweep vec a -> IO ()
maxStateContour terminal opt = do
  term <- terminal "maxStateContour"
  AppPlot.surfaceWithOpts
    "Best State of All States"
    term id id (Plot.contour . frameOpts) varRestPower varLocalPower
    $ bestStateCurve opt


maxPerState ::
  (Terminal.C term) =>
  (FilePath -> IO term) ->
  String ->
  (Map Idx.State
       (Map [Double] (Maybe (Double, Double, Type.EnvResult node Double)))
     -> Map Idx.State (Map [Double] Double)) ->
  Type.Optimisation node sweep vec Double ->
  IO ()
maxPerState terminal title func =
  plotMaps terminal id title
  . Map.map to2DMatrix
  . func
  . Type.optimalObjectivePerState
  . Type.quasiStationary

maxObjPerState, maxEtaPerState ::
  (Terminal.C term) =>
  (FilePath -> IO term) ->
  Type.Optimisation node sweep vec Double -> IO ()
maxObjPerState terminal =
  maxPerState terminal "Maximal Objective Per State" ModUt.getMaxObj

maxEtaPerState terminal =
  maxPerState terminal "Maximal Objective Per State" ModUt.getMaxEta


maxPosPerState ::
  (Show (qty node), Ord node,
   Show part, StateQty.Lookup (Idx.InPart part qty),
   Terminal.C term) =>
  (FilePath -> IO term) ->
  Idx.InPart part qty node -> Type.Optimisation node sweep vec Double -> IO ()
maxPosPerState terminal pos =
  maxPerState
    terminal
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
sweepResultTo2DMatrix len = Sig.map f . to2DMatrix
  where f (Determined x) = x
        f _ = Sweep.fromRational len ModUt.nan


perStateSweep ::
  (Show (vec Double),
   Node.C node,
   Arith.Product (sweep vec Double),
   Sweep.SweepVector vec Double,
   Sweep.SweepClass sweep vec Double,
   Terminal.C term) =>
  (FilePath -> IO term) ->
  One.OptimalEnvParams node f sweep vec a ->
  Type.Optimisation node sweep vec Double -> IO ()
perStateSweep terminal params =
  let len = One.sweepLength params
  in plotSweeps terminal id "Per State Sweep"
     . Map.map (matrix2ListOfMatrices len
                . Sig.map Sweep.toList
                . sweepResultTo2DMatrix len)
     . Map.map (Map.map Type.etaSys)
     . Type.perStateSweep
     . Type.quasiStationary

{-
expectedEtaPerState ::
  (Terminal.C term) =>
  (FilePath -> IO term) ->
  Type.Optimisation node sweep vec Double -> IO ()
expectedEtaPerState terminal =
  plotSweeps terminal id "Expected Value Per State"
  . Map.map (to2DMatrix . Map.map m2n)
  . Type.expectedEtaPerState
  . Type.quasiStationary
-}

plotOptimal ::
  (Terminal.C term, Ord b) =>
  term ->
  (Idx.State -> (b, b, Type.EnvResult node b) -> Double) ->
  String -> Type.Optimisation node sweep vec b -> IO ()
plotOptimal terminal f title =
  AppPlot.surfaceWithOpts title
            terminal
            id
            (Graph3D.typ "lines")
            frameOpts varRestPower varLocalPower
  . Map.elems
  . Map.mapWithKey (\state -> label (show state) . to2DMatrix . fmap (m2n . fmap (f state)))
  . Type.optimalObjectivePerState
  . Type.quasiStationary

optimalObjs, optimalEtas ::
  (Terminal.C term) =>
  (FilePath -> IO term) ->
  Type.Optimisation node sweep vec Double -> IO ()
optimalObjs terminal opt = do
  t <- terminal "optimalObjs"
  plotOptimal t (const fst3) "Maximal Objective Function Surfaces" opt

optimalEtas terminal opt = do
  t <- terminal "optimalEtas"
  plotOptimal t (const snd3) "Maximal Eta Surfaces" opt

optimalPos ::
  (Node.C node, Filename node, Terminal.C term) =>
  TopoIdx.Position node ->
  (FilePath -> IO term) ->
  Type.Optimisation node sweep vec Double -> IO ()
optimalPos pos@(TopoIdx.Position f t) terminal opt = do
  term <- terminal $ filename ("optimalPos", pos)
  let str = "Optimal " ++ showEdge pos
  plotOptimal term (\st -> (g . StateQty.lookup (StateIdx.power st f t) . thd3)) str opt
  where g (Just (Determined x)) = x
        g _ = ModUt.nan

findTile :: (Ord t) => [t] -> [t] -> t -> t -> [(t, t)]
findTile xs ys x y =
  let (xa, xb) = findInterval x xs
      (ya, yb) = findInterval y ys

      --findInterval :: (Ord a) => a -> [a] -> (a, a)
      findInterval z zs = (vlast "findTile" as, vhead "findTile" bs)
        where (as, bs) = span (<=z) zs

      sort [(x0, y0), (x1, y1), (x2, y2), (x3, y3)] =
           [(x0, y0), (x2, y2), (x3, y3), (x1, y1), (x0, y0)]
      sort _ = error "findTile: sort failed"

  in sort $ liftA2 (,) [xa, xb] [ya, yb]


requirements ::
  (Terminal.C term) =>
  (FilePath -> IO term) ->
  Sig.PSignal Vector Double ->
  Sig.PSignal Vector Double ->
  IO ()
requirements terminal prest plocal = do
  let rs = Sig.toList prest
      ls = Sig.toList plocal

      ts :: [([Double], [Double])]
      ts = map unzip $ zipWith (findTile ModSet.rest ModSet.local) rs ls

      sigStyle _ =
        LineSpec.pointSize 1 $
        LineSpec.pointType 7 $
        LineSpec.lineWidth 2 $
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

  t <- terminal "requirements"
  Plot.run t
    ( Opts.yLabel (showNode System.Rest) $
      Opts.xLabel (showNode System.LocalRest) $
      Plot.xyFrameAttr "Requirements" prest plocal)
    ( (mconcat $ map f ts)
      <> Plot.xy sigStyle [prest] [AppPlot.label "" plocal])


simulationGraphs ::
  (FormatValue.FormatValue b, UV.Unbox b,
   Node.C node,
   Sweep.SweepClass sweep vec b,
   Sweep.SweepVector vec b) =>
  (String -> DotGraph Text -> IO ()) ->
  Type.Optimisation node sweep vec b ->
  IO ()
simulationGraphs terminal (Type.Optimisation _ sim) = do
  let g = fmap (vhead "simulationGraphs" . Sweep.toList)

  terminal "simulationGraphsSequence"
    $ Draw.bgcolour DarkSeaGreen2
    $ Draw.title "Sequence Flow Graph from Simulation"
    $ Draw.seqFlowGraph Draw.optionsDefault (Type.sequenceFlowGraph sim)


  terminal "simulationGraphsState"
    $ Draw.bgcolour Lavender
    $ Draw.title "State Flow Graph from Simulation"
    $ Draw.stateFlowGraph Draw.optionsDefault
    $ StateQty.mapGraph g g (Type.stateFlowGraph sim)





dot ::
  (FilePath -> DotGraph Text -> IO ()) ->
  String -> String -> Int -> FilePath -> DotGraph Text -> IO ()
dot terminal suffix time n dir g = do
  let thisdir = "tmp" </> filename time </> dir
      fname = thisdir </> printf "%6.6d" n <.> suffix
  createDirectoryIfMissing True thisdir
  terminal fname g

dotXTerm :: b -> DotGraph Text -> IO ()
dotXTerm = const Draw.xterm

dotPNG :: String -> Int -> FilePath -> DotGraph Text -> IO ()
dotPNG = dot Draw.png "png"

dotSVG :: String -> Int -> FilePath -> DotGraph Text -> IO ()
dotSVG = dot Draw.svg "svg"

dotPS :: String -> Int -> FilePath -> DotGraph Text -> IO ()
dotPS = dot Draw.eps "eps"


gp :: (FilePath -> term) -> String -> String -> Int -> FilePath -> IO term
gp terminal suffix time n dir = do
  let thisdir = "tmp" </> filename time </> dir
      fname = thisdir </> printf "%6.6d" n <.> suffix
  createDirectoryIfMissing True thisdir
  return $ terminal fname

gpXTerm :: b -> IO (DefaultTerm.T)
gpXTerm = const $ return DefaultTerm.cons

gpPNG ::  String -> Int -> FilePath -> IO PNG.T
gpPNG = gp PNG.cons "png"

gpSVG ::  String -> Int -> FilePath -> IO SVG.T
gpSVG = gp SVG.cons "svg"

gpPS ::  String -> Int -> FilePath -> IO PS.T
gpPS = gp PS.cons "ps"

{-

class PNG a where
      png :: UTCTime -> Int -> FilePath -> a

type T a = DotGraph Text -> IO ()

instance PNG (DotGraph Text -> IO ()) where
         png = dotPNG

instance PNG (IO PNG.T) where
         png = gp PNG.cons

-}
