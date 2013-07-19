{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Modules.Plots where

import qualified EFA.Application.Index as XIdx
import qualified EFA.Application.AssignMap as AssignMap

import qualified EFA.Signal.PlotIO as PlotIO
import qualified EFA.Signal.Record as Record
import qualified EFA.Signal.Vector as V

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Stack as Stack

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Typ (TDisp)

import qualified Graphics.Gnuplot.Terminal as Terminal
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.LineSpecification as LineSpec


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


reportStack::(Num a, Node.C node, Ord i, Ord a, FormatValue a,
                               FormatValue i) =>
                              String
                              -> XIdx.Energy node
                              -> a
                              -> Env.Complete
                                   node t (EqRecord.Absolute (Result.Result (Stack.Stack i a)))
                              -> IO ()

reportStack ti energyIndex eps env = do
   print (ti ++ Format.unUnicode (formatValue energyIndex))
   AssignMap.print $ AssignMap.threshold eps $
      AssignMap.lookupStack energyIndex env
