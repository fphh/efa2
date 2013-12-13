{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Example.Topology.LinearTwo as LinearTwo
import EFA.Example.Topology.LinearTwo (Node, node0, node1, node2)

import qualified EFA.Application.AssignMap as AssignMap
import qualified EFA.Application.Plot as PlotIO
import EFA.Application.Utility (quantityTopology)

import qualified EFA.Flow.Topology.Symbolic as Symbolic
import qualified EFA.Flow.Topology.AssignMap as FlowTopoAssignMap
import qualified EFA.Flow.Topology.Absolute as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Variable as Var
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Topology.Absolute ((.=))

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Stack (Stack)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Data.Map as Map
import Data.Monoid (mempty, (<>))

import qualified System.IO as IO



type Term = Symbolic.Term SumProduct.Term RecIdx.Delta Node


type
   EquationSystemSymbolic s =
      EqSys.EquationSystemIgnore Node s
         (Stack (Var.Signal Node) Term)

infixr 6 *=<>, -=<>

(*=<>) ::
   (FlowTopo.Lookup idx) =>
   idx Node ->
   EquationSystemSymbolic s -> EquationSystemSymbolic s
idx *=<> eqsys =
   (idx .= (Stack.singleton $ Symbolic.varSymbol $ RecIdx.before idx))
   <>
   eqsys

(-=<>) ::
   (FlowTopo.Lookup idx) =>
   idx Node ->
   EquationSystemSymbolic s -> EquationSystemSymbolic s
idx -=<> eqsys =
   (idx .=
      let var = Var.index idx
      in  Stack.deltaPair var
             (Symbolic.symbol (RecIdx.before var))
             (Symbolic.symbol (RecIdx.delta  var)))
   <>
   eqsys


givenSymbolic :: EquationSystemSymbolic s
givenSymbolic =
   (XIdx.dTime .= Arith.fromInteger 1) <>

   XIdx.energy node0 node1 -=<>
   XIdx.eta node0 node1 -=<>
   XIdx.eta node1 node2 -=<>

   mempty


mainSymbolic :: IO ()
mainSymbolic = do

   let solved =
          EqSys.solve
             (quantityTopology LinearTwo.topology)
             givenSymbolic

   putStrLn $ Format.unUnicode $ Format.lines $
      FlowTopoAssignMap.format $ FlowTopo.toAssignMap solved

   Draw.xterm $ Draw.flowSection Draw.optionsDefault solved


type
   EquationSystemNumeric s =
      EqSys.EquationSystemIgnore Node s
         (Stack (Var.Signal Node) Double)

deltaPair ::
   (FlowTopo.Lookup idx) =>
   idx Node -> Double -> Double -> EquationSystemNumeric s
deltaPair idx before delta =
   idx .= Stack.deltaPair (Var.index idx) before delta


givenNumeric :: EquationSystemNumeric s
givenNumeric =
   (XIdx.dTime .= 1) <>

   deltaPair (XIdx.energy node0 node1) 4 (-0.6) <>
   deltaPair (XIdx.eta node0 node1) 0.25 0.1 <>
   deltaPair (XIdx.eta node1 node2) 0.85 0.05 <>

   mempty


eout :: XIdx.Energy Node
eout = XIdx.energy node2 node1



mainNumeric :: IO ()
mainNumeric = do

   let solved =
          EqSys.solve (quantityTopology LinearTwo.topology) givenNumeric

   case FlowTopo.lookupEnergy eout solved of
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
                  (formatValue $ RecIdx.delta $ Var.index eout) assigns


main :: IO ()
main = IO.hSetEncoding IO.stdout IO.utf8 >> mainNumeric >> mainSymbolic
