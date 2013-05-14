{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Modules.Plots where

--import Modules.System as System

import qualified EFA.Example.Index as XIdx
--import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.PlotIO as PlotIO
--import qualified EFA.Signal.PlotBase as PlotBase
--import qualified EFA.Graph.Topology.Node as TDNode
import qualified EFA.Signal.Vector as V

-- import qualified Graphics.Gnuplot.Advanced as Plot
-- import qualified Graphics.Gnuplot.Advanced as AGP

-- import qualified Graphics.Gnuplot.Terminal.X11 as X11
-- import qualified Graphics.Gnuplot.Terminal.WXT as WXT

import qualified Graphics.Gnuplot.Terminal as Terminal
-- import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm
-- import qualified Graphics.Gnuplot.Plot as Plt
--import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
-- import qualified Graphics.Gnuplot.Plot.ThreeDimensional as Plot3D
--import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D

-- import qualified Graphics.Gnuplot.Graph as Graph
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple

import qualified Graphics.Gnuplot.LineSpecification as LineSpec
-- import qualified Graphics.Gnuplot.ColorSpecification as ColourSpec

-- import qualified Graphics.Gnuplot.Frame as Frame
-- import qualified Graphics.Gnuplot.Frame.Option as Opt
-- import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
-- import qualified Graphics.Gnuplot.Frame.OptionSet.Style as OptsStyle
-- import qualified Graphics.Gnuplot.Frame.OptionSet.Histogram as Histogram

import EFA.Report.Typ (TDisp) {-
                       DisplayType(Typ_T),
                       getDisplayUnit,
                       DeltaDisp,
                       getDisplayTypName)

import EFA.Signal.Typ (Typ,
                       A,
                       T,
                       P,
                       Tt,
                       N
                      )
--import qualified EFA.Signal.Signal as Sig

import EFA.Signal.Signal(TC,(./),
                         FSignal,
                         FFSignal,
                         PFSignal,
                         DTFSignal,
                         FDistr,
                         PDistr,
                         NDistr)
-}
--import qualified EFA.Signal.Base as Base
--import EFA.Signal.Data (Data,
--                        Nil,(:>))

-- import EFA.Signal.Record (SigId(..), Record(..), PowerRecord, SignalRecord)
import EFA.Signal.Record as Record
-- import EFA.Hack.Record as HRecord
--import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Equation.Environment as Env
--import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Result as Result

--import qualified Data.NonEmpty as NonEmpty
import EFA.Report.FormatValue (FormatValue)
                               --formatValue)
--import qualified EFA.Report.Format as Format

import qualified EFA.Equation.Stack as Stack
--import qualified EFA.Equation.Variable as Var
--import qualified Data.Foldable as Fold
--import EFA.Equation.Arithmetic ((~+))

--import EFA.Report.Typ (TDisp)
--import qualified EFA.Symbolic.SumProduct as SumProduct



--import qualified Graphics.Gnuplot.Value.Atom as Atom
--import qualified Graphics.Gnuplot.Value.Tuple as Tuple
--import Control.Functor.HT (void)
--import qualified Graphics.Gnuplot.Frame as Frame
--import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
--import qualified Graphics.Gnuplot.Graph as Graph
--import EFA.Report.Typ (TDisp, DisplayType(Typ_T), getDisplayUnit, getDisplayTypName)
--import qualified Graphics.Gnuplot.Advanced as Plot

--import qualified Data.Foldable as Fold
--import qualified Data.Map as M

--import Control.Functor.HT (void)

import qualified EFA.Example.AssignMap as AssignMap

--import Debug.Trace

--import Data.Monoid ((<>))

sigsWithSpeed :: (Fractional d1,
                      Fractional d2,
                      Ord d2,
                      Show (v d2),
                      V.Walker v,
                      V.Storage v d1,
                      V.Storage v d2,
                      V.Singleton v,
                      V.FromList v,
                      TDisp t1,
                      TDisp t2,
                      Tuple.C d1,
                      Tuple.C d2,
                      Atom.C d1,
                      Atom.C d2,
                      Terminal.C term) =>
                 term ->
                 [(Record.Name,Record s1 s2 t1 t2 SigId v d1 d2)] ->
                 (String,[SigId]) -> IO()
sigsWithSpeed term recList (componentName, idList) = PlotIO.recordList_extractWithLeadSignal ("Component " ++ componentName ++ " -  Signals with Vehicle Speed") term show id (Record.RangeFrom idList, Record.ToModify $ [Record.SigId "speedsensor1.v"]) recList


operation :: (Fractional d, Ord id, Show (v d), Show id, V.Walker v,
              V.Storage v d, V.FromList v, TDisp t2, Tuple.C d, Atom.C d,
              Terminal.C term) =>
              String ->
              term ->
              (LineSpec.T -> LineSpec.T) ->
              [(Record.Name, Record s1 s2 t1 t2 id v d d)] -> ([Char], (id, id)) -> IO ()

operation ti term opts rList  (plotTitle, (idx,idy)) = mapM_ f rList
  where f ((Record.Name recTitle), rec) = do
          let x = getSig rec idx
              y = getSig rec idy
              legend n = recTitle ++ " - Torque over Speed"
          PlotIO.xy (ti ++ "_" ++ plotTitle) term opts legend x y


reportStack::(Num a, Ord node, Ord i, Ord a, Show node, FormatValue a,
                               FormatValue i) =>
                              [Char]
                              -> XIdx.Energy node
                              -> a
                              -> Env.Complete
                                   node t (EqRecord.Absolute (Result.Result (Stack.Stack i a)))
                              -> IO ()

reportStack ti energyIndex eps (env) = do
               print (ti ++ show energyIndex)
               AssignMap.print $ AssignMap.threshold eps $ AssignMap.lookupStack energyIndex env
