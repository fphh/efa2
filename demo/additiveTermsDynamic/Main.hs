{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Example.Topology.LinearTwo as LinearTwo
import EFA.Example.Topology.LinearTwo (Node, node0, node1, node2)

import qualified EFA.Application.AssignMap as AssignMap
import qualified EFA.Application.Symbolic as Symbolic
import qualified EFA.Application.Plot as PlotIO
import EFA.Application.Utility (seqFlowGraphFromTopology)

import qualified EFA.Flow.Sequence.AssignMap as SeqFlowAssignMap
import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Sequence.Absolute ((.=))

import qualified EFA.Symbolic.Variable as SymVar
import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Stack (Stack)

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Data.Map as Map
import Data.Monoid (mempty, (<>))

import qualified System.IO as IO


sec0 :: Idx.Section
sec0 = Idx.Section 0


type SignalTerm = Symbolic.SignalTerm Idx.Delta SumProduct.Term Node
type ScalarTerm = Symbolic.ScalarTerm Idx.Delta SumProduct.Term Node


type
   EquationSystemSymbolic s =
      EqSys.EquationSystemIgnore Node s
         (Stack (Var.SectionAny Node) ScalarTerm)
         (Stack (Var.SectionAny Node) SignalTerm)

infixr 6 *=<>, -=<>

(*=<>) ::
   (SeqFlow.LookupSignal idx) =>
   Idx.InSection idx Node ->
   EquationSystemSymbolic s -> EquationSystemSymbolic s
idx *=<> eqsys =
   (idx .= (Stack.singleton $ SymVar.varSymbol $ Idx.before idx))
   <>
   eqsys

(-=<>) ::
   (SeqFlow.LookupSignal idx) =>
   Idx.InSection idx Node -> EquationSystemSymbolic s -> EquationSystemSymbolic s
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

   let solved =
          EqSys.solve
             (seqFlowGraphFromTopology LinearTwo.topology)
             givenSymbolic

   putStrLn $ Format.unUnicode $ Format.lines $
      SeqFlowAssignMap.format $ SeqFlow.toAssignMap solved

   Draw.xterm $ Draw.seqFlowGraph Draw.optionsDefault solved


type
   EquationSystemNumeric s =
      EqSys.EquationSystemIgnore Node s
         (Stack (Var.SectionAny Node) Double)
         (Stack (Var.SectionAny Node) Double)

deltaPair ::
   (SeqFlow.LookupSignal idx) =>
   Idx.InSection idx Node -> Double -> Double -> EquationSystemNumeric s
deltaPair idx before delta =
   idx .= Stack.deltaPair (Var.Signal $ Var.index idx) before delta


givenNumeric :: EquationSystemNumeric s
givenNumeric =
   (XIdx.dTime sec0 .= 1) <>

   deltaPair (XIdx.energy sec0 node0 node1) 4 (-0.6) <>
   deltaPair (XIdx.eta sec0 node0 node1) 0.25 0.1 <>
   deltaPair (XIdx.eta sec0 node1 node2) 0.85 0.05 <>

   mempty


eout :: XIdx.Energy Node
eout = XIdx.energy sec0 node2 node1



mainNumeric :: IO ()
mainNumeric = do

   let solved =
          EqSys.solve (seqFlowGraphFromTopology LinearTwo.topology) givenNumeric

   case SeqFlow.lookupEnergy eout solved of
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
