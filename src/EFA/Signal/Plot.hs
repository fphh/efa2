{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances#-}

-- | Module to offer most common plots
module EFA.Signal.Plot (module EFA.Signal.Plot) where
{-   run,
   -- signalIO,
   xy, xyBasic, xyFrame, xyStyle,
   -- xyIO,
   surface,
   --surfaceIO,
   record, recordStyle, recordFrame,
   -- recordIO, recordIOList,
   -- sequenceIO,
   -- recordSplitPlus, recordSplit, sequenceSplit,
   -- recordSelect, sequenceSelect,
   stack, stackFrame,
   -- stackIO,
   stacks, stacksFrame,
   -- stacksIO,
   getData,
   ) where

   ) where -}

import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Signal.Signal as S
--import qualified EFA.Signal.Data as D
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Record as Record
--import qualified EFA.Signal.Colour as Colour

import EFA.Signal.SequenceData (SequData)

import EFA.Signal.Record (Record)
-- import EFA.Signal.SequenceData (SequData(..))

--import EFA.Signal.Signal (TC, toSigList, getDisplayType)
-- import EFA.Signal.Base (BSum)

--import EFA.Signal.Data (Data, (:>), Nil, NestedList)
import EFA.Report.Typ (TDisp)
                       --DisplayType(Typ_T),
                       --getDisplayUnit,
                       --getDisplayTypName)
--import EFA.Report.Base (UnitScale(UnitScale), getUnitScale)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import EFA.Signal.PlotBase

import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Variable as Var
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as TDNode
import qualified EFA.Example.AssignMap as AssignMap

import qualified EFA.Signal.Signal as Sig
import qualified EFA.Signal.Vector as V
import qualified EFA.Signal.Base as Base

import qualified Graphics.Gnuplot.Advanced as Plot
-- import qualified Graphics.Gnuplot.Advanced as AGP

--import qualified Graphics.Gnuplot.Terminal.X11 as X11
--import qualified Graphics.Gnuplot.Terminal.WXT as WXT

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
--import qualified Graphics.Gnuplot.Plot as Plt
--import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
--import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
--import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

--import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import qualified Graphics.Gnuplot.LineSpecification as LineSpec
--import qualified Graphics.Gnuplot.ColorSpecification as ColourSpec

import qualified Graphics.Gnuplot.Frame as Frame
--import qualified Graphics.Gnuplot.Frame.Option as Opt
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
--import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
--import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram


import qualified Data.Map as M
--import qualified Data.List as L
import qualified Data.Foldable as Fold

import Control.Monad (zipWithM_)
import Control.Functor.HT (void)
--import Data.Foldable (foldMap)
--import Data.Monoid (mconcat)
import Data.Monoid ((<>))


-- import Control.Concurrent (threadDelay)


-- | Simple Signal Plotting -- plot signal values against signal index --------------------------------------------------------------


signalIO ::
   (Signal signal, Terminal.C term) =>
   String -> term -> (LineSpec.T -> LineSpec.T) -> signal -> IO ()
signalIO ti terminal opts x = plotOne terminal (signalFrameAttr ti x) (signal opts x)

{-
tableLinear, tableLinear2D, tableSurface ::
  (Terminal.C term) =>
  term -> String -> Table.Map Double -> IO ()
tableLinear term str = plotOne term . plotTable id str
tableLinear2D term str = plotOne term . plotTable tail str
-}


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
   void $ Plot.plotSync DefaultTerm.cons $
      Frame.cons attrs $ surface x y z




-- | Plotting Signals against each other -----------------------------


xyIO ::
   (XY tcX tcY, Terminal.C term) =>
   String -> term -> (LineSpec.T -> LineSpec.T)-> (Int -> String) -> tcX -> tcY -> IO ()
xyIO ti terminal opts legend x y =
   plotOne terminal (xyFrameAttr ti x y) (xy opts legend x y)


-- | Plotting Records ---------------------------------------------------------------

recordIO :: (Terminal.C term,
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
recordIO ti term showKey opts x =
   plotOne term (recordFrame ti) (record showKey opts x)



recordIOList ::
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
recordIOList ti term showKey opts x =
   plotOne term (recordFrame ti) (recordList showKey opts varOpts x)
   where
     varOpts n = LineSpec.lineStyle n

{-# WARNING  recordIOList_extractWithLeadSignal "pg: Lead signals still need to be highlighted or scale to be displayed " #-}
recordIOList_extractWithLeadSignal :: (Terminal.C term,
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
                                       V.Singleton v) =>
                                      String ->
                                      term ->
                                      (id -> String) ->
                                      (LineSpec.T -> LineSpec.T) ->
                                      (Record.RangeFrom id, Record.ToModify id) ->
                                      [(Record.Name, Record s1 s2 t1 t2 id v d1 d2)] -> IO ()
recordIOList_extractWithLeadSignal ti term showKey opts (extract, leadIds) recList =
   plotOne term (recordFrame ti) (recordList showKey opts  (\n -> LineSpec.pointType n) $ finalRecs)
  where extractedRecList = case extract of
          Record.RangeFrom idList -> map (\(x,y) -> (x,Record.extract idList y)) recList
          Record.RangeFromAll -> recList

        finalRecs = map (\(x,y)->(x,Record.normSignals2Max75 (extract, leadIds) y)) extractedRecList

-------------------------------------------
-- Futher Record Plot Variants

recordIOSplit ::
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
recordIOSplit n ti term showKey opts r =
   zipWithM_
      (\k -> recordIO (ti ++ " - Part " ++ show (k::Int)) term showKey opts)
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
      (\k -> recordIO (ti ++ " - Part " ++ show (k::Int)) term opts)
      [0 ..] (map (Record.addSignals list) (Record.split n r))
-}
--------------------------------------------
-- recordIO command to plot selected Signals only
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

sequenceIO ::
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
sequenceIO ti term showKey opts =
  Fold.sequence_ .  SD.mapWithSection (\ x -> recordIO (ti ++ " - "++ show x) term showKey opts)
   -- (Plot.plotSync term) . sequenceFrame ti opts

sequenceIOSplit ::
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
sequenceIOSplit n ti term showKey opts =
   Fold.sequence_ .
   SD.mapWithSection (\ x -> recordIOSplit n (ti ++ " - "++ show x) term showKey opts)

-- | Plotting Stacks ---------------------------------------------------------------

stackIO ::
   (FormatValue term, Show term, Ord term) =>
   String -> Format.ASCII -> M.Map term Double -> IO ()
stackIO title var m =
   void .  Plot.plotSync DefaultTerm.cons . Frame.cons (stackFrame title var) . stack $ m


{- |
The length of @[var]@ must match the one of the @[Double]@ lists.
-}
stacksIO ::
   (Ord term, FormatValue term, Show term) =>
   String -> [Format.ASCII] -> M.Map term [Double] -> IO ()
stacksIO title vars xs =
   void . Plot.plotSync DefaultTerm.cons .
   Frame.cons (stacksFrame title vars) . stacks $ xs


stackIOfromEnv:: (Show node, Ord i, FormatValue i, TDNode.C node, Show i) =>
        String ->
        Idx.InSection Idx.Energy node ->
        Double ->
        (Record.DeltaName, Env.Complete
        node t (EqRecord.Absolute (Result.Result (Stack.Stack i Double)))) ->
        IO ()

stackIOfromEnv ti energyIndex eps (Record.DeltaName recName, env) = do
  stackIO ("Record " ++ recName ++ "-" ++ ti)
    (formatValue $ Idx.delta $ Var.index energyIndex)
    (AssignMap.threshold eps $ AssignMap.lookupStack energyIndex env)

recordStackRowIO:: (TDNode.C node, Ord node, Ord i, Show i, Show node, FormatValue i) =>
                            String
                            -> [Record.DeltaName]
                            -> Idx.InSection Idx.Energy node
                            -> Double
                            -> [Env.Complete node t
                                   (EqRecord.Absolute (Result.Result (Stack.Stack i Double)))]
                            -> IO ()

recordStackRowIO ti deltaSets energyIndex eps envs =
   stacksIO ti
   (map (Format.literal . (\ (Record.DeltaName x) -> x)) deltaSets)
   (AssignMap.simultaneousThreshold eps .
   AssignMap.transpose .
   map (AssignMap.lookupStack energyIndex)
    $ envs)

sectionStackRowIO:: (Ord node, TDNode.C node,Show i, Ord i, FormatValue i) =>
                  String
                  -> Idx.Energy node
                  -> Double
                  -> Env.Complete node t
                         (EqRecord.Absolute (Result.Result (Stack.Stack i Double)))
                  -> IO ()
sectionStackRowIO ti energyIndex eps env =
   stacksIO ti
   (map (Format.literal . (\(Idx.InSection sec _) -> show sec) .fst) allStacks)
   (AssignMap.simultaneousThreshold eps . AssignMap.transpose $
    map (M.mapKeys AssignMap.deltaIndexSet .
         Stack.assignDeltaMap . snd) $ allStacks)
   where allStacks = AssignMap.lookupAllStacks energyIndex env

aggregatedStackIO ::
   (TDNode.C node, Ord node, Show node,
    Ord i, Show i, FormatValue i) =>
   String ->
   Idx.Energy node ->
   Double ->
   Env.Complete node t
      (EqRecord.Absolute (Result.Result (Stack.Stack i Double))) ->
   IO ()

aggregatedStackIO ti energyIndex eps env =
  stackIO ti (formatValue $ Idx.delta energyIndex) $
  AssignMap.threshold eps $
  M.mapKeys AssignMap.deltaIndexSet $
  AssignMap.lookupAggregatedStack energyIndex env


-- | Plotting Average Efficiency Curves over Energy Flow Distribution -------------------------------

-- | Simple plot with provided data p and n time signals, ideally sorted, fDist is energy distribution over power
-- | and nDist is the averaged efficiency over power
-- | pg: currently plots over input power, one should be able to choose
etaDistr1DimIO :: (Fractional d,
                  V.Walker v,
                  V.Storage v d,
                  V.FromList v,
                  Atom.C d,
                  Tuple.C d) =>
                 String -> Sig.PFSignal v d -> Sig.NFSignal v d ->
                 Sig.PDistr v d -> Sig.FDistr v d -> Sig.NDistr v d -> IO ()
etaDistr1DimIO ti p n pDist fDist nDist =
  plotOne DefaultTerm.cons
  (xyFrameAttr ti p n) $
  xy id (\_ -> "Efficiency Operation Points over Power")  p n <>
  xy id (\_ -> "Averaged Efficiency over Power") pDist nDist <>
  xy id (\_ -> "Input Energy Distribution over Power") pDist fDist


-- | Plot efficiency distribution from List of Records
-- | pg: currently plots over input power, one should be able to choose
-- | You however can choose which power you want to plot and classify over (abszisse)
etaDistr1DimIOfromRecordList :: (Ord id,
                             Show id,
                             Show (v d),
                             Base.BProd d d,
                             Ord d,
                             V.Zipper v,
                             V.Walker v,
                             V.Storage v (d, d),
                             V.Storage v d,
                             Fractional d,
                             V.FromList v,
                             Atom.C d,
                             Tuple.C d,
                             V.SortBy v,
                             V.Unique v (Sig.Class d),
                             V.Storage v Sig.SignalIdx,
                             V.Storage v Int,
                             V.Storage v (Sig.Class d),
                             V.Storage v ([Sig.Class d], [Sig.SignalIdx]),
                             RealFrac d,
                             V.Lookup v,
                             V.Find v,
                             Base.BSum d,
                             Base.DArith0 d,
                             V.Storage v (d, (d, d)),
                             V.Singleton v) =>
                            String  -> d -> d -> [(Record.Name, Record.DTimeFlowRecord id v d)]
                            -> (String, (Idx.PPos id, Idx.PPos id, Idx.PPos id)) -> IO ()

etaDistr1DimIOfromRecordList ti  intervall offset rList  (plotTitle, (idIn,idOut,idAbszisse)) = mapM_ f rList
  where f ((Record.Name recTitle), rec) = do
          let ein = Record.getSig rec idIn
              eout = Record.getSig rec idOut
              eAbszisse = Record.getSig rec idAbszisse
              pAbszisse = eAbszisse Sig../dtime
              dtime = Record.getTime rec
              eta = Sig.calcEtaWithSign eout ein eout
              (pDist, einDist , _ , nDist) = Sig.etaDistibution1D intervall offset
                                                 dtime ein eout eout
              (x,y) = Sig.sortTwo (pAbszisse,eta)
          etaDistr1DimIO (ti ++ "_" ++ plotTitle ++ "_" ++ recTitle) x y  pDist
            (Sig.scale (Sig.norm einDist) 100) nDist
   
