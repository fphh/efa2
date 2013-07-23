{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module to offer most common plots
module EFA.Signal.PlotIO (
   signal,
   xy,
   surface, surfaceWithOpts,
   record,
   recordList,
   sequence,
--   recordSplitPlus,
   recordSplit,
   sequenceSplit,
   recordList_extract,
   recordList_extractWithLeadSignal,
--   recordSelect,
--   sequenceSelect,
   stack,
   stacks,
   stackfromEnv,
   recordStackRow,
   sectionStackRow,
   etaDistr1Dim,
   etaDistr1DimfromRecordList,
   aggregatedStack
   ) where

import qualified EFA.Application.AssignMap as AssignMap

import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Base as Base
import EFA.Signal.Record (Record)
import EFA.Signal.SequenceData (SequData)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Typ (TDisp)

import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Variable as Var
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as TDNode

import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
import qualified Graphics.Gnuplot.Graph.ThreeDimensional as Graph3D
--import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import qualified Graphics.Gnuplot.Frame as Frame

import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Data.Map (Map)

import Control.Monad (zipWithM_)
import Control.Functor.HT (void)
import Data.Monoid ((<>))

import Prelude hiding (sequence)


-- | Simple Signal Plotting -- plot signal values against signal index --------------------------------------------------------------


signal ::
   (Plot.Signal signal, Terminal.C term) =>
   String -> term -> (LineSpec.T -> LineSpec.T) -> signal -> IO ()
signal ti terminal opts x = Plot.run terminal (Plot.signalFrameAttr ti x) (Plot.signal opts x)

{-
tableLinear, tableLinear2D, tableSurface ::
  (Terminal.C term) =>
  term -> String -> Table.Map Double -> IO ()
tableLinear term str = run term . plotTable id str
tableLinear2D term str = run term . plotTable tail str
-}

-- | Plotting Surfaces


surfaceWithOpts ::
  (Plot.Surface tcX tcY tcZ, Terminal.C term) =>
  String -> term ->
  (LineSpec.T -> LineSpec.T) ->
  (Opts.T (Graph3D.T (Plot.Value tcX) (Plot.Value tcY) (Plot.Value tcZ)) ->
    Opts.T (Graph3D.T (Plot.Value tcX) (Plot.Value tcY) (Plot.Value tcZ))) ->
  (Int -> String) ->
  tcX -> tcY -> tcZ -> IO ()
surfaceWithOpts ti terminal opts fopts legend x y z =
  Plot.run terminal (fopts $ Plot.xyFrameAttr ti x y)
                    (Plot.surface opts legend x y z)

surface ::
  (Plot.Surface tcX tcY tcZ, Terminal.C term) =>
  String -> term ->
  (Int -> String) ->
  tcX -> tcY -> tcZ -> IO ()
surface ti terminal legend x y z = surfaceWithOpts ti terminal id id legend x y z

{-
combineWith ::
  (Plot.Surface tcX tcY tcZ, Terminal.C term) =>
  (tcZ -> tcZ -> tcZ) ->
  String -> term ->
  (LineSpec.T -> LineSpec.T) ->
  (Int -> String) ->
  tcX -> tcY -> tcZ -> tcZ -> IO ()
combineWith f ti terminal opts legend x y z0 z1 =
  Plot.run terminal (Plot.xyFrameAttr ti x y) $
    Plot.surface opts legend x y (Sig.zipWith f z0 z1)
-}


-- | Plotting Signals against each other -----------------------------


xy ::
   (Plot.XY tcX tcY, Terminal.C term) =>
   String -> term -> (LineSpec.T -> LineSpec.T)-> (Int -> String) -> tcX -> tcY -> IO ()
xy ti terminal opts legend x y =
   Plot.run terminal (Plot.xyFrameAttr ti x y) (Plot.xy opts legend x y)


-- | Plotting Records ---------------------------------------------------------------

record :: (Terminal.C term,
             Fractional d2,
             Fractional d1,
             Ord id,
             Show id,
             SV.Walker v,
             SV.Storage v d2,
             SV.Storage v d1,
             SV.FromList v,
             TDisp t2,
             TDisp t1,
             Atom.C d2,
             Atom.C d1,
             Tuple.C d2,
             Tuple.C d1) =>
            String ->
            term ->
            (id -> String) ->
            (LineSpec.T -> LineSpec.T) ->
            Record s1 s2 t1 t2 id v d1 d2 -> IO ()
record ti term showKey opts x =
   Plot.run term (Plot.recordFrameAttr ti) (Plot.record showKey opts x)



recordList ::
   (Ord id,
    Show id,
    SV.Walker v,
    SV.FromList v,
    TDisp t1,
    TDisp t2,
    Fractional d2,
    Fractional d1,
    SV.Storage v d2,
    SV.Storage v d1,
    Atom.C d2,
    Atom.C d1,
    Tuple.C d2,
    Tuple.C d1,
    Terminal.C term) =>
   String ->
   term ->
   (id -> String) ->
   (LineSpec.T -> LineSpec.T) ->
   [(Record.Name,Record s1 s2 t1 t2 id v d1 d2)] -> IO ()
recordList ti term showKey opts x =
   Plot.run term (Plot.recordFrameAttr ti) (Plot.recordList showKey opts varOpts x)
   where
     varOpts n = LineSpec.lineStyle n

recordList_extract ::
   (Ord id,
    Show id,
    SV.Walker v,
    SV.FromList v,
    TDisp t1,
    TDisp t2,
    Fractional d2,
    Fractional d1,
    SV.Storage v d2,
    SV.Storage v d1,
    Atom.C d2,
    Atom.C d1,
    Tuple.C d2,
    Tuple.C d1,
    Terminal.C term) =>
   String ->
   term ->
   (id -> String) ->
   (LineSpec.T -> LineSpec.T) ->
   [(Record.Name,Record s1 s2 t1 t2 id v d1 d2)] ->
   [id] ->
   IO ()
recordList_extract ti term showKey opts xs idList =
   Plot.run term (Plot.recordFrameAttr ti) (Plot.recordList showKey opts varOpts $ map (\(x,y) -> (x,Record.extract idList y)) xs)
   where
     varOpts n = LineSpec.lineStyle n

{-# WARNING  recordList_extractWithLeadSignal "pg: Lead signals still need to be highlighted or scale to be displayed " #-}
recordList_extractWithLeadSignal :: (Terminal.C term,
                                       Fractional d2,
                                       Fractional d1,
                                       Ord id,
                                       Show id,
                                       SV.Walker v,
                                       SV.Storage v d2,
                                       SV.Storage v d1,
                                       SV.FromList v,
                                       TDisp t2,
                                       TDisp t1,
                                       Atom.C d2,
                                       Atom.C d1,
                                       Tuple.C d2,
                                       Tuple.C d1,
                                       Ord d2,
                                       Show (v d2),
                                       SV.Singleton v) =>
                                      String ->
                                      term ->
                                      (id -> String) ->
                                      (LineSpec.T -> LineSpec.T) ->
                                      (Record.RangeFrom id, Record.ToModify id) ->
                                      [(Record.Name, Record s1 s2 t1 t2 id v d1 d2)] -> IO ()
recordList_extractWithLeadSignal ti term showKey opts (extract, leadIds) recList =
   Plot.run term (Plot.recordFrameAttr ti) (Plot.recordList showKey opts  (\n -> LineSpec.pointType n) $ finalRecs)
  where extractedRecList = case extract of
          Record.RangeFrom idList -> map (\(x,y) -> (x,Record.extract idList y)) recList
          Record.RangeFromAll -> recList

        finalRecs = map (\(x,y)->(x,Record.normSignals2Max75 (extract, leadIds) y)) extractedRecList

-------------------------------------------
-- Futher Record Plot Variants

recordSplit ::
   (Terminal.C term,
    Fractional d1,
    Fractional d2,
    Ord id,
    Show id,
    SV.Walker v,
    SV.Storage v d1,
    SV.Storage v d2,
    SV.FromList v,
    TDisp t1,
    TDisp t2,
    Atom.C d1,
    Atom.C d2,
    Tuple.C d1,
    Tuple.C d2) =>
   Int ->
   String ->
   term ->
   (id -> String) ->
   (LineSpec.T -> LineSpec.T) ->
   Record s1 s2 t1 t2 id v d1 d2 -> IO ()
recordSplit n ti term showKey opts r =
   zipWithM_
      (\k -> record (ti ++ " - Part " ++ show (k::Int)) term showKey opts)
      [0..] (Record.split n r)

{-
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
      (\k -> record (ti ++ " - Part " ++ show (k::Int)) term opts)
      [0 ..] (map (Record.addSignals list) (Record.split n r))
-}
--------------------------------------------
-- record command to plot selected Signals only
{-
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
         record opts)
-}

sequence ::
   (Fractional d1,
    Fractional d2,
    Ord id,
    Show id,
    SV.Walker v,
    SV.Storage v d1,
    SV.Storage v d2,
    SV.FromList v,
    TDisp t1,
    TDisp t2,
    Atom.C d1,
    Atom.C d2,
    Tuple.C d1,
    Tuple.C d2,
    Terminal.C term) =>
   String ->
   term ->
   (id -> String) ->
   (LineSpec.T -> LineSpec.T) ->
   SequData (Record s1 s2 t1 t2 id v d1 d2) -> IO ()
sequence ti term showKey opts =
  Fold.sequence_ .  SD.mapWithSection (\ x -> record (ti ++ " - "++ show x) term showKey opts)
   -- (Plot.plotSync term) . sequenceFrame ti opts

sequenceSplit ::
   (Fractional d2,
    Fractional d1,
    Ord id,
    Show id,
    SV.Walker v,
    SV.Storage v d2,
    SV.Storage v d1,
    SV.FromList v,
    TDisp t2,
    TDisp t1,
    Atom.C d2,
    Atom.C d1,
    Tuple.C d2,
    Tuple.C d1,
    Terminal.C term) =>
   Int ->
   String ->
   term ->
   (id -> String) ->
   (LineSpec.T -> LineSpec.T) ->
   SequData (Record s1 s2 t1 t2 id v d1 d2) -> IO ()
sequenceSplit n ti term showKey opts =
   Fold.sequence_ .
   SD.mapWithSection (\ x -> recordSplit n (ti ++ " - "++ show x) term showKey opts)

-- | Plotting Stacks ---------------------------------------------------------------

stack ::
   (FormatValue term, Show term, Ord term) =>
   String -> Format.ASCII -> Map term Double -> IO ()
stack title var m =
   void .  Plot.plotSync DefaultTerm.cons . Frame.cons (Plot.stackFrameAttr title var) . Plot.stack $ m


{- |
The length of @[var]@ must match the one of the @[Double]@ lists.
-}
stacks ::
   (Ord term, FormatValue term, Show term) =>
   String -> [Format.ASCII] -> Map term [Double] -> IO ()
stacks title vars xs =
   void . Plot.plotSync DefaultTerm.cons .
   Frame.cons (Plot.stacksFrameAttr title vars) . Plot.stacks $ xs


stackfromEnv:: (Show node, Ord i, FormatValue i, TDNode.C node, Show i) =>
        String ->
        Idx.InSection Idx.Energy node ->
        Double ->
        (Record.DeltaName, Env.Complete
        node t (EqRecord.Absolute (Result.Result (Stack.Stack i Double)))) ->
        IO ()

stackfromEnv ti energyIndex eps (Record.DeltaName recName, env) = do
  stack ("Record " ++ recName ++ "-" ++ ti)
    (formatValue $ Idx.delta $ Var.index energyIndex)
    (AssignMap.threshold eps $ AssignMap.lookupStack energyIndex env)

recordStackRow:: (TDNode.C node, Ord node, Ord i, Show i, Show node, FormatValue i) =>
                            String
                            -> [Record.DeltaName]
                            -> Idx.InSection Idx.Energy node
                            -> Double
                            -> [Env.Complete node t
                                   (EqRecord.Absolute (Result.Result (Stack.Stack i Double)))]
                            -> IO ()

recordStackRow ti deltaSets energyIndex eps envs =
   stacks ti
   (map (Format.literal . (\ (Record.DeltaName x) -> x)) deltaSets)
   (AssignMap.simultaneousThreshold eps .
   AssignMap.transpose .
   map (AssignMap.lookupStack energyIndex)
    $ envs)

sectionStackRow:: (Ord node, TDNode.C node,Show i, Ord i, FormatValue i) =>
                  String
                  -> Idx.Energy node
                  -> Double
                  -> Env.Complete node t
                         (EqRecord.Absolute (Result.Result (Stack.Stack i Double)))
                  -> IO ()
sectionStackRow ti energyIndex eps env =
   case unzip $ Map.toList $ AssignMap.lookupEnergyStacks energyIndex env of
      (idxs, energyStacks) ->
         stacks ti (map (Format.literal . show) idxs) $
         AssignMap.simultaneousThreshold eps . AssignMap.transpose $
         map (Map.mapKeys AssignMap.deltaIndexSet) energyStacks

aggregatedStack ::
   (TDNode.C node, Ord node, Show node,
    Ord i, Show i, FormatValue i) =>
   String ->
   Idx.Energy node ->
   Double ->
   Env.Complete node t
      (EqRecord.Absolute (Result.Result (Stack.Stack i Double))) ->
   IO ()

aggregatedStack ti energyIndex eps env =
  stack ti (formatValue $ Idx.delta energyIndex) $
  AssignMap.threshold eps $
  Map.mapKeys AssignMap.deltaIndexSet $ Fold.fold $
  AssignMap.lookupEnergyStacks energyIndex env


-- | Plotting Average Efficiency Curves over Energy Flow Distribution -------------------------------

-- | Simple plot with provided data p and n time signals, ideally sorted, fDist is energy distribution over power
-- | and nDist is the averaged efficiency over power
-- | pg: currently plots over input power, one should be able to choose
etaDistr1Dim :: (Fractional d,
                  SV.Walker v,
                  SV.Storage v d,
                  SV.FromList v,
                  Atom.C d,
                  Tuple.C d) =>
                 String -> Sig.PFSignal v d -> Sig.NFSignal v d ->
                 Sig.PDistr v d -> Sig.FDistr v d -> Sig.NDistr v d -> IO ()
etaDistr1Dim ti p n pDist fDist nDist =
  Plot.run DefaultTerm.cons
  (Plot.xyFrameAttr ti p n) $
  Plot.xy id (\_ -> "Efficiency Operation Points over Power")  p n <>
  Plot.xy id (\_ -> "Averaged Efficiency over Power") pDist nDist <>
  Plot.xy id (\_ -> "Input Energy Distribution over Power") pDist fDist


-- | Plot efficiency distribution from List of Records
-- | pg: currently plots over input power, one should be able to choose
-- | You however can choose which power you want to plot and classify over (abscissa)
etaDistr1DimfromRecordList :: (Ord id,
                             Show id,
                             Show (v d),
                             Base.BProd d d,
                             Ord d,
                             SV.Zipper v,
                             SV.Walker v,
                             SV.Storage v (d, d),
                             SV.Storage v d,
                             Fractional d,
                             SV.FromList v,
                             Atom.C d,
                             Tuple.C d,
                             SV.SortBy v,
                             SV.Unique v (Sig.Class d),
                             SV.Storage v Sig.SignalIdx,
                             SV.Storage v Int,
                             SV.Storage v (Sig.Class d),
                             SV.Storage v ([Sig.Class d], [Sig.SignalIdx]),
                             RealFrac d,
                             SV.Lookup v,
                             SV.Find v,
                             Base.BSum d,
                             Base.DArith0 d,
                             SV.Storage v (d, (d, d)),
                             SV.Singleton v) =>
                            String  -> d -> d -> [(Record.Name, Record.DTimeFlowRecord id v d)]
                            -> (String, (Idx.PPos id, Idx.PPos id, Idx.PPos id)) -> IO ()

etaDistr1DimfromRecordList ti  interval offset rList  (plotTitle, (idIn,idOut,idAbscissa)) = mapM_ f rList
  where f ((Record.Name recTitle), rec) = do
          let ein = Record.getSig rec idIn
              eout = Record.getSig rec idOut
              eAbscissa = Record.getSig rec idAbscissa
              pAbscissa = eAbscissa Sig../dtime
              dtime = Record.getTime rec
              eta = Sig.calcEtaWithSign eout ein eout
              (pDist, einDist , _ , nDist) = Sig.etaDistribution1D interval offset
                                                 dtime ein eout eout
              (x,y) = Sig.sortTwo (pAbscissa,eta)
          etaDistr1Dim (ti ++ "_" ++ plotTitle ++ "_" ++ recTitle) x y  pDist
            (Sig.scale (Sig.norm einDist) 100) nDist

