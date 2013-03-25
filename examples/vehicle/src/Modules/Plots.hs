{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Modules.Plots where

--import Modules.System as System

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
import qualified Data.Foldable as Fold

import EFA.Report.Typ (TDisp)
import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
--import Control.Functor.HT (void)
--import qualified Graphics.Gnuplot.Frame as Frame
--import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
--import qualified Graphics.Gnuplot.Graph as Graph
--import EFA.Report.Typ (TDisp, DisplayType(Typ_T), getDisplayUnit, getDisplayTypName)
--import qualified Graphics.Gnuplot.Advanced as Plot

import qualified Data.Map as M
--import EFA.Utility(checkedLookup)
import qualified Data.NonEmpty as NonEmpty
import qualified EFA.Report.Format as Format

import EFA.Report.FormatValue (formatValue, FormatValue)
import Data.Tuple.HT (mapFst)

--import Debug.Trace

--import Data.Monoid ((<>))



sigsWithSpeed ::(Fractional a, Ord a, Show (v a), V.Walker v, V.Storage v a,
                                 V.Singleton v, V.FromList v, TDisp t2, TDisp t1, Atom.C a,
                                 Tuple.C a) =>
                                [Record s t1 t2 SigId v a] -> (String, [SigId]) -> IO ()

sigsWithSpeed recList (ti, idList) =  do
  HPlot.record2 (O.title ti .
                 O.extract idList .
                 O.leadSignalsMax (Record.RangeFrom idList, Record.ToModify [Record.SigId "speedsensor1.v"]) .
                 O.pointSize 0.1) (HPlot.RecList recList)



stack:: (Ord i, FormatValue i, TDNode.C a) =>
        String ->
        Idx.Energy a ->
        (String, Env.Complete
        a t (EqRecord.Absolute (Result.Result (Stack.Stack i Double)))) ->
        IO ()
stack ti energyIndex (recName,env) = do

  let (Env.Complete _ sigEnv) = env

  case M.lookup energyIndex (Env.energyMap sigEnv) of
    Nothing -> error "undefined E"
    Just d ->
      case EqRecord.unAbsolute d of
        Result.Undetermined -> error "undetermined E"
        Result.Determined xs -> do
          let assigns =
                fmap (mapFst (foldl (\p i -> p * SumProduct.Atom i) 1)) $
                NonEmpty.tail $
                Stack.assigns xs

          let  assignsFilt = filter (\(_,x)-> (abs x) >= 1) assigns

          Fold.forM_ assignsFilt $ \(term,val) -> do
            putStrLn $ Format.unUnicode $
              Format.assign (formatValue term) (formatValue val)

          Plot.stackIO ("Record " ++ recName ++ "-" ++ ti)
              (Idx.delta $ Var.index energyIndex) $ assignsFilt


-- operation ti rec 