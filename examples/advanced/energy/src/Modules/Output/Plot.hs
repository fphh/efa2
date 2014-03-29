{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module Modules.Output.Plot where

import Modules.Input.Setting (varRestPower, varLocalPower)
import qualified EFA.Application.Utility as AppUt
import qualified Modules.Input.Setting as ModSet
import qualified Modules.Input.System as System
import qualified EFA.Application.Optimisation.Base as Base

import qualified EFA.Application.Plot as AppPlot
import qualified EFA.Application.Sweep as Sweep
import qualified EFA.Application.Type as Type
import qualified EFA.Application.OneStorage as One

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as Vector

import qualified EFA.Flow.SequenceState.Index as Idx

import qualified EFA.Flow.Sequence.Algorithm as SeqAlgo

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
--import Data.Tuple.HT (fst3, snd3, thd3)
import qualified Data.Maybe as Maybe

import Data.Monoid ((<>), mconcat)
import Control.Applicative (liftA2)

import System.Directory (createDirectoryIfMissing)
import System.FilePath.Posix ((</>), (<.>))

import Text.Printf (printf)

import Debug.Trace(trace)

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

{-plotMaps ::
  (Filename state, Show state,
   Terminal.C term,
   Plot.Surface
     (Sig.PSignal2 Vector vec a)
     (Sig.PSignal2 Vector vec a)
     tcZ,
   Plot.Value tcZ ~ Double) =>
  (FilePath -> IO term) ->
  (a -> tcZ) ->
  String ->
  Map state a ->
  IO ()-}

plotMaps ::
  (Show k, Filename k, Terminal.C term,
   Plot.Surface
   (Sig.PSignal2 Vector UV.Vector Double)
   (Sig.PSignal2 Vector UV.Vector Double)
   tcZ,
   Plot.Value tcZ ~ Double) =>
  (String -> IO term) -> (a -> tcZ) -> [Char] -> Map k a -> IO ()

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
            frameOpts varLocalPower varRestPower
            (func mat)

plotSweeps ::
  (Filename state, Show state,
   Terminal.C term,
   Plot.Surface
   (Sig.PSignal2 Vector Vector Double)
   (Sig.PSignal2 Vector Vector Double)
   tcZ,Plot.Surface
   (Sig.PSignal2 Vector UV.Vector Double)
   (Sig.PSignal2 Vector UV.Vector Double)
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
            (Opts.key False . frameOpts) varLocalPower varRestPower
            (func mat)



plotMapOfMaps ::
  (Show a, Terminal.C term, Show state, Filename state, a ~ Double) =>
  (FilePath -> IO term) ->
  Map a (Map state (Sig.PSignal2 Vector Vector (Maybe (Result a)))) ->
  IO ()
plotMapOfMaps terminal =
  concurrentlyMany_
  . Map.elems
  . Map.mapWithKey (plotMaps terminal (Sig.map AppUt.nothing2Nan) . show)

plotGraphMaps ::
  (FormatValue.FormatValue a, Show a, Filename [a], Node.C node) =>
  (String -> DotGraph Text -> IO ()) ->
  String ->
  Map [a] (Maybe (a, a, Int, Type.EnvResult node a)) ->
  IO ()
plotGraphMaps terminal title =
  sequence_ . Map.elems . Map.mapWithKey
    (\reqs -> maybe (return ())
        (\(objVal, eta, _, graph) -> do
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
  Type.OptimalSolutionPerState node a ->
  IO ()
plotGraphMapOfMaps terminal =
  sequence_ . Map.elems . Map.mapWithKey (plotGraphMaps terminal . show)

optimalObjectivePerState ::
  (Show a, FormatValue.FormatValue a, Filename [a], Node.C node) =>
  (String -> DotGraph Text -> IO ()) ->
  Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
  IO ()
optimalObjectivePerState terminal =
  plotGraphMapOfMaps terminal . Type.optimalSolutionPerState

perEdge ::
  (Vector.Walker l, Vector.Storage l a, Vector.FromList l,
   Arith.Constant a,
   Tuple.C a, Atom.C a,
   Terminal.C term,
   Node.C node, Show node, Filename node) =>
  (FilePath -> IO term) ->
  One.SystemParams node a ->
  Record.PowerRecord node l a ->
  IO ()
perEdge terminal sysParams rec =
  let recs = map f $ Graph.edges $ One.systemTopology sysParams
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
   Terminal.C term,
   Vector.Walker simVec,
   Vector.Storage simVec a,
   Vector.FromList simVec) =>
  (FilePath -> IO term) ->
  One.SystemParams node a ->
  Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
  IO ()
simulationSignalsPerEdge terminal sysParams =
  perEdge terminal sysParams . Type.signals . Type.simulation

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
   Tuple.C a, Atom.C a,
   Vector.Walker simVec,
   Vector.Storage simVec a,
   Vector.FromList simVec) =>
  (FilePath -> IO term) ->
  Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
  IO ()
simulationSignals terminal opt = do
  let str = "Simulation Signals"
  t <- terminal $ filename str
  AppPlot.record str t showEdge id
    $ Type.signals
    $ Type.simulation opt
{-
givenSignals ::
  (Show node, Ord node, Node.C node,
   Terminal.C term,
   Arith.Constant a,
   Tuple.C a, Atom.C a,
   Vector.Walker intVec,
   Vector.Storage intVec a,
   Vector.FromList intVec) =>
  (FilePath -> IO term) ->
  Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
  IO ()
givenSignals terminal opt = do
  let str = "Given Signals"
  t <- terminal $ filename str
  AppPlot.record str t showEdge id
    $ Type.reqsAndDofsSignals
    $ Type.interpolation opt
-}

to2DMatrix :: (Ord b) => Map [b] a -> Sig.PSignal2 Vector Vector a
to2DMatrix = AppUt.to2DMatrix

m2n :: (Arith.Constant a) => Maybe a -> a
m2n Nothing = AppUt.nan
m2n (Just x) = x

defaultPlot ::
  (Terminal.C term, a ~ Double) =>
  IO term -> String -> Sig.PSignal2 Vector Vector a -> IO ()
defaultPlot terminal title xs = do
  t <- terminal
  AppPlot.surfaceWithOpts
--    title t (LineSpec.title "") (Graph3D.typ "lines") frameOpts varLocalPower varRestPower xs
    title t (LineSpec.title "") id frameOpts varLocalPower varRestPower xs

{-
withFuncToMatrix ::
  (Ord b, Arith.Constant b, a~b) =>
  -- (Type.OptimalSolution node a -> a) ->   
  ((b, b, Idx.State, Int, Type.EnvResult node b) -> b) ->
  Type.SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d ->
  Sig.PSignal2 Vector Vector b
withFuncToMatrix func =
  to2DMatrix
  . Map.map (maybe AppUt.nan func)
  . Type.optimalSolution



plotMax ::
  (Terminal.C term, b ~ a, a ~ Double) =>
  IO term ->
  String ->
  ((b, b, Idx.State, Int, Type.EnvResult node b) -> b) ->
  Type.SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d ->
  IO ()
plotMax term title func =
  defaultPlot term title
  . withFuncToMatrix func

-- TODO: g Nothing = Arith.zero is dangerous -- better solution ? 
maxPos ::
  (Ord node, Show node, Filename node, Node.C node,Arith.Constant b,a ~ b,b ~ Double,
   Terminal.C term) =>
  TopoIdx.Position node ->
  (FilePath -> IO term) ->
  Type.SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d ->
  IO ()
maxPos pos@(TopoIdx.Position f t) terminal =
  plotMax (terminal $ filename ("maxPos", pos))
          ("Maximal Value for: " ++ showEdge pos)
          (\(_, _, st, _, env) -> g $ StateQty.lookup (StateIdx.power st f t) env)
  where g (Just (Determined x)) = x
        g Nothing = Arith.zero -- show Power of inactive Edges
        g (Just (Undetermined)) = AppUt.nan

maxEta ::
  (Terminal.C term, a ~ b,b ~ Double) =>
  (FilePath -> IO term) ->
  Type.SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d ->
  IO ()
maxEta term =
  plotMax (term "maxEta") "Maximal Eta of All States"
    AppUt.snd5

maxObj ::
  (Terminal.C term, a ~ b,b ~ Double) =>
  (FilePath -> IO term) ->
  Type.SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d ->
  IO ()
maxObj term =
  plotMax (term "maxObj") "Maximal Objective of All States" AppUt.fst5

bestStateCurve ::
  (Ord b, Arith.Constant a, Num a, a ~ b) =>
  Type.SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d ->
  Sig.PSignal2 Vector Vector a
bestStateCurve =
  withFuncToMatrix ((\(Idx.State state) -> fromIntegral state) . AppUt.thd5)
-}

stateRange2 ::
  (Ord a, Terminal.C term, b ~ a) =>
  ([Char] -> IO term) -> 
  Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
  IO ()
stateRange2 term opt = do
  t <- term "StateRanges"
  plotOptimal t (\(Idx.State st) _ ->  fromIntegral st) "stateRange2" opt


stateRange ::
  (Terminal.C term, Ord a) =>
  term ->
  Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d
  -> IO ()
stateRange terminal =
  AppPlot.surfaceWithOpts "StateRanges"
            terminal
            id
            (Graph3D.typ "lines")
            frameOpts varLocalPower varRestPower
  . Map.elems
  . Map.mapWithKey (\state@(Idx.State st)  -> label (show state) . to2DMatrix . fmap (m2n . fmap (f st)))
  . Type.optimalSolutionPerState
  where f st _ =  fromIntegral st



{-

maxState ::
  (Terminal.C term, a ~ b, b ~ Double) =>
  (FilePath -> IO term) ->
  Type.SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d ->
  IO ()
maxState term =
  defaultPlot (term "maxState") "Best State of All States"
  . bestStateCurve

maxStateContour ::
  (Terminal.C term, Ord a, a ~ b, a ~ Double, b ~ Double) =>
  (FilePath -> IO term) ->
  Type.SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d ->
  IO ()
maxStateContour terminal opt = do
  term <- terminal "maxStateContour"
  AppPlot.surfaceWithOpts
    "Best State of All States"
    term id id (Plot.contour . frameOpts) varLocalPower varRestPower
    $ bestStateCurve opt
-}

maxPerState ::
  (Terminal.C term,a ~ Double) =>
  (FilePath -> IO term) ->
  String ->
  (Type.OptimalSolutionPerState node a ->
   Map Idx.State (Map [Double] Double)) ->
  Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
  IO ()
maxPerState terminal title func =
  plotMaps terminal id title
  . Map.map to2DMatrix
  . func
  . Type.optimalSolutionPerState

maxObjPerState, maxEtaPerState,maxIndexPerState ::
  (Terminal.C term, a ~ Double) =>
  (FilePath -> IO term) ->
   Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
   IO ()
maxObjPerState terminal =
  maxPerState terminal "Maximal Objective Per State" AppUt.getMaxObj

maxEtaPerState terminal =
  maxPerState terminal "Chosen Eta Per State" AppUt.getMaxEta

maxIndexPerState terminal =
  maxPerState terminal "Chosen Index Per State" AppUt.getMaxIndex

expectedEtaPerState ::
  (Terminal.C term, a ~ Double) =>
  (FilePath -> IO term) ->
   Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
   IO ()
expectedEtaPerState terminal =
  plotSweeps terminal id "Expected Value Per State"
  . Map.map (to2DMatrix . Map.map m2n)
  . Type.averageSolutionPerState

expectedEtaDifferencePerState ::
  (Terminal.C term, a ~ Double) =>
  (FilePath -> IO term) ->
  Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
  IO ()
expectedEtaDifferencePerState terminal opt =
  plotSweeps terminal id "Difference Between Maximal Eta and Expected Value Per State"
  $ Map.map to2DMatrix mat
  where ev = Map.map (Map.map m2n) $
             Type.averageSolutionPerState opt
        eta = AppUt.getMaxEta
              $ Type.optimalSolutionPerState opt
        mat = Map.intersectionWith (Map.intersectionWith (Arith.~-)) eta ev

maxPosPerState ::
  (Show (qty node), Ord node,
   Show part, StateQty.Lookup (Idx.InPart part qty),
   Terminal.C term, a ~ Double) =>
  (FilePath -> IO term) ->
  Idx.InPart part qty node ->
  Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
  IO ()
maxPosPerState terminal pos =
  maxPerState
    terminal
    ("Maximal Position " ++ show pos ++ " per state")
    (AppUt.getMaxPos pos)


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
        f _ = Sweep.fromRational len AppUt.nan

sweepStackPerStateEta ::
  (Show (vec Double),a ~ Double,
   Node.C node,
   Arith.Product (sweep vec a),
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Terminal.C term) =>
  (FilePath -> IO term) ->
  One.OptimisationParams node f sweep vec a ->
  Type.Sweep node sweep vec a ->
  IO ()
sweepStackPerStateEta terminal params =
  let len = One.sweepLength params
  in plotSweeps terminal id "Per State Sweep -- Eta"
     . Map.map (matrix2ListOfMatrices len
                . Sig.map Sweep.toList
                . sweepResultTo2DMatrix len)
     . Map.map (Map.map Type.etaSys)


sweepStackPerStateStoragePower ::
  (Show (vec Double),a ~ Double,Show node,
   Node.C node,
   Arith.Product (sweep vec a),
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Terminal.C term) =>
  (FilePath -> IO term) ->
  One.OptimisationParams node f sweep vec a ->
  node ->
  Type.Sweep node sweep vec a ->
  IO ()
sweepStackPerStateStoragePower terminal params node =
  let len = One.sweepLength params
      f m = Map.lookup node m
      g (Just (Just x)) = x
      g _ = error ("Error in sweepStackPerStateStoragePower - no StoragePower found for node: " ++ show node)

  in plotSweeps terminal id "Per State Sweep -- StoragePower"
     . Map.map (matrix2ListOfMatrices len
                . Sig.map Sweep.toList
                . sweepResultTo2DMatrix len)
     . Map.map (Map.map (g . f . Type.storagePowerMap))

sweepStackPerStateOpt ::
  (Show (vec Double),a ~ Double,vec ~ UV.Vector,Show node,Sweep.SweepClass sweep UV.Vector (Double, Double),
   Node.C node,
   Arith.Product (sweep vec a),
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Terminal.C term) =>
  (FilePath -> IO term) ->
  One.OptimisationParams node f sweep vec a ->
  One.BalanceForcing node a ->
  Type.Sweep node sweep vec a ->
  IO ()
sweepStackPerStateOpt terminal params balanceForcing =
  let len = One.sweepLength params

  in plotSweeps terminal id "Per State Sweep -- Opt"
     . Map.map (matrix2ListOfMatrices len
                . Sig.map Sweep.toList
                . sweepResultTo2DMatrix len)
     . (Base.optStackPerState params balanceForcing)

sweepStackPerStateCondition ::
  (Show (vec Double),a ~ Double,Show node,sweep ~ Sweep.Sweep,vec ~ UV.Vector,
   Node.C node,
   Arith.Product (sweep vec a),
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Terminal.C term) =>
  (FilePath -> IO term) ->
  One.OptimisationParams node f sweep vec a ->
  Type.Sweep node sweep vec a ->
  IO ()
sweepStackPerStateCondition terminal params =
  let len = One.sweepLength params
      f (Determined (Sweep.Sweep vec)) = Determined $ Sweep.Sweep $ UV.imap g vec
      f _ = error "Error in sweepStackPerStateCondition - undetermined Condition"
      g idx True = fromIntegral idx
      g _ False = 0/0

  in plotSweeps terminal id "Per State Sweep -- Validity"
     . Map.map (matrix2ListOfMatrices len
                . Sig.map Sweep.toList
                . sweepResultTo2DMatrix len)
     . Map.map (Map.map (f . Type.condVec))

{-
sweepStackPerStatePowerPos ::
  (Show (vec Double),a ~ Double,Show node,sweep ~ Sweep.Sweep,vec ~ UV.Vector,
   Node.C node,
   Arith.Product (sweep vec a),
   Sweep.SweepVector vec a,
   Sweep.SweepClass sweep vec a,
   Terminal.C term) =>
  (FilePath -> IO term) ->
  One.OptimisationParams node f sweep vec a ->
  TopoIdx.Position node ->
  Type.Sweep node sweep vec a ->
  IO ()
sweepStackPerStatePowerPos terminal params pos@(TopoIdx.Position f t) =
  let len = One.sweepLength params
      g (Just (Just x)) = x
      g _ = error ("Error in sweepStackPerStateStoragePower - no Power found for Position: " ++ show pos)

  in plotSweeps terminal id ("Per State Sweep -- PowerPosition: " ++ show pos)
     . Map.map (matrix2ListOfMatrices len
                . Sig.map Sweep.toList
                . sweepResultTo2DMatrix len)
     . Map.mapWithKey (\st x -> (Map.map (g . StateQty.lookup (StateIdx.power st f t) . Type.envResult) x))
-}

{-
g $ StateQty.lookup (StateIdx.power st f t) env)
  where g (Just (Determined x)) = x
        g _ = AppUt.nan     
-}     

plotOptimal ::
  (Terminal.C term, Ord b, a~b) =>
  term ->
  (Idx.State -> (b, b, Int,Type.EnvResult node b) -> Double) ->
  String -> Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d -> IO ()
plotOptimal terminal f title =
  AppPlot.surfaceWithOpts title
            terminal
            id
            (Graph3D.typ "lines")
            frameOpts varLocalPower varRestPower
  . Map.elems
  . Map.mapWithKey (\state -> label (show state) . to2DMatrix . fmap (m2n . fmap (f state)))
  . Type.optimalSolutionPerState


optimalObjs, optimalEtas ::
  (Terminal.C term, a ~ Double,b~Double) =>
  (FilePath -> IO term) ->
   Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d -> 
  IO ()
optimalObjs terminal opt = do
  t <- terminal "optimalObjs"
  plotOptimal t (const AppUt.fst4) "Maximal Objective Function Surfaces" opt

optimalEtas terminal opt = do
  t <- terminal "optimalEtas"
  plotOptimal t (const AppUt.snd4) "Maximal Eta Surfaces" opt

optimalPos ::
  (Node.C node, Filename node, Terminal.C term, a ~ Double, b ~ Double) =>
  TopoIdx.Position node ->
  (FilePath -> IO term) ->
  Type.SignalBasedOptimisation node sweep vec a intVec b simVec c efaVec d ->
  IO ()
optimalPos pos@(TopoIdx.Position f t) terminal opt = do
  term <- terminal $ filename ("optimalPos", pos)
  let str = "Optimal " ++ showEdge pos
  plotOptimal term (\st -> (g . StateQty.lookup (StateIdx.power st f t) . AppUt.frth4)) str opt
  where g (Just (Determined x)) = x
        g _ = AppUt.nan

findTile :: (Ord t, Show t) => [t] -> [t] -> t -> t -> [(t, t)]
findTile xs ys x y =
  let (xa, xb) = findInterval x xs
      (ya, yb) = findInterval y ys

      --findInterval :: (Ord a) => a -> [a] -> (a, a)
      findInterval z zs = trace (show as ++ show bs) $ (vlast "findTile" as, vhead "findTile" bs)
        where (as, bs) = span (<=z) zs

      sort [(x0, y0), (x1, y1), (x2, y2), (x3, y3)] =
           [(x0, y0), (x2, y2), (x3, y3), (x1, y1), (x0, y0)]
      sort _ = error "findTile: sort failed"

  in sort $ liftA2 (,) [xa, xb] [ya, yb]


reqsRec ::
  (Show (v Double), Vector.Walker v, Vector.Storage v Double,
   Vector.FromList v, Terminal.C term) =>
  (FilePath -> IO term) ->
  Record.PowerRecord node v Double ->
  IO ()
reqsRec terminal (Record.Record _ pMap) = mapM_ f $ zip (init xs) (tail xs)
   where xs = Map.elems pMap
         f (x,y) = requirements terminal x y


requirements ::
  (Terminal.C term, Show (v Double),
   Vector.Walker v,
   Vector.Storage v Double,
   Vector.FromList v) =>
  (FilePath -> IO term) ->
  Sig.PSignal v Double ->
  Sig.PSignal v Double ->
  IO ()
requirements terminal plocal prest = do
  let rs = Sig.toList prest
      ls = Sig.toList plocal

      ts :: [([Double], [Double])]
      ts = map unzip $ zipWith (findTile ModSet.local ModSet.rest ) rs ls

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
   Vector.Storage efaVec d,
   Vector.FromList efaVec,
   Vector.Walker efaVec,
   Arith.ZeroTestable d,
   Arith.Constant d,
   FormatValue.FormatValue d,
   Node.C node) =>
  (String -> DotGraph Text -> IO ()) ->
  Type.SignalBasedOptimisation node sweep sweepVec a intVec b simVec c efaVec d ->
  IO ()
simulationGraphs terminal (Type.SignalBasedOptimisation _ _ _ _ efa _) = do
  let g = id -- fmap (vhead "simulationGraphs" . Sweep.toList)

  terminal "simulationGraphsSequence"
    $ Draw.bgcolour DarkSeaGreen2
    $ Draw.title "Sequence Flow Graph from Simulation"
    $ Draw.seqFlowGraph Draw.optionsDefault (Type.sequenceFlowGraph efa)

  terminal "simulationGraphsSequenceAccumulated"
    $ Draw.bgcolour DarkSeaGreen2
    $ Draw.title "Accumulated Sequence Flow Graph from Simulation"
    $ Draw.seqFlowGraph Draw.optionsDefault
    $ SeqAlgo.accumulate (Type.sequenceFlowGraph efa)

  terminal "simulationGraphsState"
    $ Draw.bgcolour Lavender
    $ Draw.title "State Flow Graph from Simulation"
    $ Draw.stateFlowGraph Draw.optionsDefault
    $ StateQty.mapGraph g g (Type.stateFlowGraph efa)





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

-- TODO: linearer sweepIndex der richtige Weg ?
drawSweepStateFlowGraph ::
  (Node.C node, FormatValue.FormatValue b,
  Sweep.SweepVector vec b, Sweep.SweepClass sweep vec b) =>
  String ->
  Int ->
  StateQty.Graph node (Result (sweep vec b)) (Result (sweep vec b)) ->
  IO ()
drawSweepStateFlowGraph title sweepIndex sfgSweep =
  Draw.xterm $ Draw.title title $ Draw.stateFlowGraph Draw.optionsDefault
  $ StateQty.mapGraph g g sfgSweep
     where g = fmap (vhead "simulationGraphs" . drop sweepIndex . Sweep.toList)

drawSweepStackStateFlowGraph ::
  (Ord [a], Node.C node,Show a,FormatValue.FormatValue b,
   Sweep.SweepVector vec b,
   Sweep.SweepClass sweep vec b) =>
  Idx.State ->
  [a] ->
  Int ->
  Map Idx.State (Map [a] (Type.SweepPerReq node sweep vec b)) ->
  IO ()
drawSweepStackStateFlowGraph state reqsPos sweepIndex sweep =
  drawSweepStateFlowGraph ("StateFlowGraph from SweepStack - State : " ++ show state ++
                           " - Requirement Position :" ++ show reqsPos ++ "- Sweep Index: " ++ show sweepIndex) sweepIndex sfgSweep
  where sfgSweep =  Type.envResult $
                    Maybe.maybe (error $ "drawSweepStackStateFlowGraph - Position not found: "++ show reqsPos)
                    id $ Map.lookup reqsPos $
                    maybe (error $ "drawSweepStackStateFlowGraph - State not found: " ++ show state)
                    id $ Map.lookup state sweep
 