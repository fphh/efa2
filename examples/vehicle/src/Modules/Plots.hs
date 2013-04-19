{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Modules.Plots where

--import Modules.System as System

import qualified EFA.Example.Index as XIdx
import qualified EFA.Signal.Plot as Plot
import qualified EFA.Hack.Plot as HPlot
import qualified EFA.Hack.Options as O
import qualified EFA.Graph.Topology.Node as TDNode

import qualified EFA.Signal.Vector as V

--import EFA.Signal.Typ (Typ,A,T,P,Tt)
--import EFA.Signal.Signal (Signal)
-- import EFA.Signal.Record (SigId(..), Record(..), PowerRecord, SignalRecord)
import EFA.Signal.Record as Record
-- import EFA.Hack.Record as HRecord
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as EqRecord
import qualified EFA.Equation.Result as Result

import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Variable as Var
--import qualified Data.Foldable as Fold

import EFA.Report.Typ (TDisp)
--import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
--import Control.Functor.HT (void)
--import qualified Graphics.Gnuplot.Frame as Frame
--import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
--import qualified Graphics.Gnuplot.Graph as Graph
--import EFA.Report.Typ (TDisp, DisplayType(Typ_T), getDisplayUnit, getDisplayTypName)
--import qualified Graphics.Gnuplot.Advanced as Plot

import qualified Data.Map as M
--import qualified Data.NonEmpty as NonEmpty
--import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import qualified EFA.Example.AssignMap as AssignMap

--import Debug.Trace

--import Data.Monoid ((<>))



sigsWithSpeed ::(Fractional a, Ord a, Show (v a), V.Walker v, V.Storage v a,
                                 V.Singleton v, V.FromList v, TDisp t2, TDisp t1, Atom.C a,
                                 Tuple.C a) =>
                                [Record s1 s2 t1 t2 SigId v a] -> (String, [SigId]) -> IO ()


sigsWithSpeed recList (ti, idList) =  do
  HPlot.record2 (O.title ti .
                 O.extract idList .
                 O.leadSignalsMax (Record.RangeFrom idList, Record.ToModify [Record.SigId "speedsensor1.v"]) .
                 O.pointSize 0.1) (HPlot.RecList recList)

operation :: (Fractional a, Ord id, Show (v a), Show id, V.Walker v,
              V.Storage v a, V.FromList v, TDisp t2, Tuple.C a, Atom.C a) =>
              [Char]
              -> [([Char], Record s1 s2 t1 t2 id v a)] -> ([Char], (id, id)) -> IO ()

operation ti rList  (plotTitle, (idx,idy)) = mapM_ f rList
  where f (recTitle, rec) = do
          let x = getSig rec idx
              y = getSig rec idy
          Plot.xyIO (ti ++ "_" ++ plotTitle ++ "_" ++ recTitle) x y



stack:: (Show a, Ord i, FormatValue i, TDNode.C a, Show i) =>
        String ->
        XIdx.Energy a ->
        Double ->
        (String, Env.Complete
        a t (EqRecord.Absolute (Result.Result (Stack.Stack i Double)))) ->
        IO ()
stack ti energyIndex eps (recName,env) = do
--   AssignMap.print $ lookupStack energyIndex env
   Plot.stackIO ("Record " ++ recName ++ "-" ++ ti)
      (formatValue $ Idx.delta $ Var.index energyIndex)
      (AssignMap.threshold eps $ lookupStack energyIndex env)


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
               AssignMap.print $ AssignMap.threshold eps $ lookupStack energyIndex env

recordStackRow:: (TDNode.C node, Ord node, Ord i, Show i, Show node, FormatValue i) =>
                            String
                            -> XIdx.Energy node
                            -> Double
                            -> [Env.Complete node t
                                   (EqRecord.Absolute (Result.Result (Stack.Stack i Double)))]
                            -> IO ()

recordStackRow ti energyIndex eps envs =
   Plot.stacksIO ti
      (map (const $ formatValue $ Idx.delta $ Var.index energyIndex) envs) .
   AssignMap.simultaneousThreshold eps .
   AssignMap.transpose .
   map (lookupStack energyIndex)
    $ envs


lookupStack:: (Ord i, Ord node, Show node) =>
                              XIdx.Energy node
                              -> Env.Complete
                                   node t (EqRecord.Absolute (Result.Result (Stack.Stack i a)))
                              -> M.Map (AssignMap.IndexSet i) a

lookupStack energyIndex env =  case M.lookup energyIndex (Env.energyMap signalEnv) of
    Nothing -> error (show energyIndex ++ "undefined")
    Just d ->
      case EqRecord.unAbsolute d of
        Result.Undetermined -> error (show energyIndex ++ "undetermined")
        Result.Determined xs -> M.mapKeys AssignMap.deltaIndexSet $
                             Stack.assignDeltaMap xs

   where
        Env.Complete _scalarEnv signalEnv = env




