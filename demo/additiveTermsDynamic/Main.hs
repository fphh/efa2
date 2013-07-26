{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Application.AssignMap as AssignMap
import qualified EFA.Application.Absolute as EqGen
import qualified EFA.Application.Index as XIdx
import qualified EFA.Application.Utility as Utility
import EFA.Application.Utility (makeEdges, constructSeqTopo)
import EFA.Application.Absolute ((.=))

import qualified EFA.Symbolic.Variable as SymVar
import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Stack (Stack)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Signal.PlotIO as PlotIO

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatSignalIndex, FormatValue, formatValue)

import qualified Data.Map as Map
import Data.Monoid (mempty, (<>))

import qualified System.IO as IO


sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1, node2 :: Node.Int
node0 :~ node1 :~ node2 :~ _ = Stream.enumFrom minBound


topoLinear :: TD.Topology Node.Int
topoLinear = Gr.fromList ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Crossing),
              (node2, TD.Sink)]
        es = [(node0, node1), (node1, node2)]


type SignalTerm = Utility.SignalTerm Idx.Delta SumProduct.Term Node.Int
type ScalarTerm = Utility.ScalarTerm Idx.Delta SumProduct.Term Node.Int


type
   EquationSystemSymbolic s =
      EqGen.EquationSystem Node.Int s
         (Stack (Var.SectionAny Node.Int) ScalarTerm)
         (Stack (Var.SectionAny Node.Int) SignalTerm)

infixr 6 *=<>, -=<>

(*=<>) ::
   (Ord (idx Node.Int), FormatSignalIndex idx, Env.AccessSignalMap idx) =>
   Idx.InSection idx Node.Int ->
   EquationSystemSymbolic s -> EquationSystemSymbolic s
idx *=<> eqsys =
   (idx .= (Stack.singleton $ SymVar.varSymbol $ Idx.before idx))
   <>
   eqsys

(-=<>) ::
   (Ord (idx Node.Int), FormatSignalIndex idx, Env.AccessSignalMap idx) =>
   Idx.InSection idx Node.Int -> EquationSystemSymbolic s -> EquationSystemSymbolic s
idx -=<> eqsys =
   (idx .=
      let var = Var.index idx
      in  Stack.deltaPair
             (Var.Signal var)
             (SymVar.symbol (Idx.before var))
             (SymVar.symbol (Idx.delta  var)))
   <>
   eqsys


givenSymbolic :: EquationSystemSymbolic s
givenSymbolic =
   (XIdx.dTime sec0 .= Arith.fromInteger 1) <>

   XIdx.energy sec0 node0 node1 -=<>
   XIdx.eta sec0 node0 node1 -=<>
   XIdx.eta sec0 node1 node2 -=<>

   mempty


mainSymbolic :: IO ()
mainSymbolic = do

   let seqTopo = constructSeqTopo topoLinear [0]
   let env = EqGen.solve seqTopo givenSymbolic

   putStrLn $ Format.unUnicode $ formatValue env

   Draw.xterm $
     Draw.sequFlowGraphAbsWithEnv seqTopo env



type
   EquationSystemNumeric s =
      EqGen.EquationSystem Node.Int s
         (Stack (Var.SectionAny Node.Int) Double)
         (Stack (Var.SectionAny Node.Int) Double)

deltaPair ::
   (Ord (idx Node.Int), FormatSignalIndex idx, Env.AccessSignalMap idx) =>
   Idx.InSection idx Node.Int -> Double -> Double -> EquationSystemNumeric s
deltaPair idx before delta =
   idx .= Stack.deltaPair (Var.Signal $ Var.index idx) before delta


givenNumeric :: EquationSystemNumeric s
givenNumeric =
   (XIdx.dTime sec0 .= 1) <>

   deltaPair (XIdx.energy sec0 node0 node1) 4 (-0.6) <>
   deltaPair (XIdx.eta sec0 node0 node1) 0.25 0.1 <>
   deltaPair (XIdx.eta sec0 node1 node2) 0.85 0.05 <>

   mempty


eout :: XIdx.Energy Node.Int
eout = XIdx.energy sec0 node2 node1



mainNumeric :: IO ()
mainNumeric = do

   let seqTopo = constructSeqTopo topoLinear [0]
       Env.Complete _scalarEnv signalEnv = EqGen.solve seqTopo givenNumeric

   case Map.lookup eout (Env.energyMap signalEnv) of
      Nothing -> error "undefined E_2_1"
      Just d ->
         case d of
            Result.Undetermined -> error "undetermined E_2_1"
            Result.Determined x -> do
               let assigns =
                      Map.mapKeys AssignMap.indexSet $
                      Stack.assignDeltaMap x
               AssignMap.print assigns
               PlotIO.stack "Decomposition of total output energy"
                  (formatValue $ Idx.delta $ Var.index eout) assigns


main :: IO ()
main = IO.hSetEncoding IO.stdout IO.utf8 >> mainNumeric >> mainSymbolic
