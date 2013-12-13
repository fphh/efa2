{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Example.Topology.LinearTwo as LinearTwo
import EFA.Example.Topology.LinearTwo (Node, node0, node1, node2)

import qualified EFA.Application.NestedDelta as NestedDelta
import qualified EFA.Application.AssignMap as AssignMap
import qualified EFA.Application.Plot as PlotIO
import EFA.Application.NestedDelta
          (ParameterRecord,
           beforeDelta, extrudeStart,
           (<&), (<&>), (&>), (&&>))
import EFA.Application.Utility (quantityTopology)

import qualified EFA.Flow.Topology.Symbolic as Symbolic
import qualified EFA.Flow.Topology.AssignMap as FlowTopoAssignMap
import qualified EFA.Flow.Topology.EquationSystem as EqSys
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Topology.Variable as Var
import qualified EFA.Flow.Topology.Index as XIdx
import qualified EFA.Flow.Draw as Draw
import EFA.Flow.Topology.NestedDelta
          (givenParameterSymbol, givenParameterNumber, (?=))

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Symbolic.OperatorTree as Op

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified EFA.Utility.FixedLength as FL

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import Data.NonEmpty ((!:))
import Data.Monoid (mempty, (<>))



type Term = Symbolic.Term SumProduct.Term RecIdx.Delta Node

type IdxMultiDelta = RecIdx.ExtDelta (RecIdx.ExtDelta (RecIdx.ExtDelta RecIdx.Absolute))
type RecMultiDelta = Record.ExtDelta (Record.ExtDelta (Record.ExtDelta Record.Absolute))

type
   EquationSystemSymbolic s =
      EqSys.EquationSystem Symbolic.Ignore
         RecMultiDelta Node s Term



_absolute, absolute ::
   (Arith.Sum a) => NestedDelta.InnerExtrusion RecMultiDelta a
_absolute = beforeDelta  &> beforeDelta  &> beforeDelta  &> extrudeStart

param0, param1, param2 ::
   (Arith.Sum a) => NestedDelta.OuterExtrusion RecMultiDelta a
param0 = beforeDelta <&  beforeDelta <&  beforeDelta <&> extrudeStart
param1 = beforeDelta <&  beforeDelta <&> beforeDelta  &> extrudeStart
param2 = beforeDelta <&> beforeDelta  &> beforeDelta  &> extrudeStart


absolute = NestedDelta.getAbsoluteRecord params

params ::
   (Arith.Sum a) =>
   ParameterRecord FL.N3 RecMultiDelta a
params =
   beforeDelta &&> beforeDelta &&> beforeDelta &&> NestedDelta.parameterStart

absoluteRecord ::
   (Arith.Sum x) =>
   x -> RecMultiDelta (Result x)
absoluteRecord = NestedDelta.absoluteRecord absolute


eout, ein :: XIdx.Energy Node
ein  = XIdx.energy node0 node1
eout = XIdx.energy node2 node1

eta0, eta1 :: XIdx.Eta Node
eta0 = XIdx.eta node0 node1
eta1 = XIdx.eta node1 node2


termFromIndex ::
   IdxMultiDelta ->
   [RecIdx.Record RecIdx.Delta (Var.Signal Node)]
termFromIndex
      (RecIdx.ExtDelta r2 (RecIdx.ExtDelta r1 (RecIdx.ExtDelta r0 RecIdx.Absolute))) =
   RecIdx.Record r2 (Var.index ein) :
   RecIdx.Record r1 (Var.index eta0) :
   RecIdx.Record r0 (Var.index eta1) :
   []



_givenSymbolic :: EquationSystemSymbolic s
_givenSymbolic =
   (XIdx.dTime ?= absoluteRecord (Arith.fromInteger 1)) <>

   givenParameterSymbol ein  param2 <>
   givenParameterSymbol eta0 param1 <>
   givenParameterSymbol eta1 param0 <>

   mempty

givenSymbolic :: EquationSystemSymbolic s
givenSymbolic =
   (XIdx.dTime ?= absoluteRecord (Arith.fromInteger 1)) <>

   Fold.fold
      (NonEmpty.zipWith ($)
         (givenParameterSymbol ein  !:
          givenParameterSymbol eta0 !:
          givenParameterSymbol eta1 !:
          Empty.Cons)
         (NestedDelta.getParameterRecord params)) <>

   mempty


simplify :: Term -> Term
simplify =
   Op.toNormalTerm .
   NonEmpty.sum . Op.expand .
   Op.fromNormalTerm

simplifiedSummands ::
   RecMultiDelta (Result Term) -> NonEmpty.T [] (Result Term)
simplifiedSummands =
   fmap (fmap simplify) . Record.summands

mainSymbolic :: IO ()
mainSymbolic = do

   let solved =
          EqSys.solve
             (quantityTopology LinearTwo.topology)
             givenSymbolic

   putStrLn $ Format.unUnicode $ Format.lines $
      FlowTopoAssignMap.format $ FlowTopo.toAssignMap $
      FlowTopo.mapSection simplifiedSummands solved

   Draw.xterm $ Draw.flowSection Draw.optionsDefault $
      FlowTopo.mapSection simplifiedSummands solved


type
   EquationSystemNumeric s =
      EqSys.EquationSystem Symbolic.Ignore
         RecMultiDelta Node s Double


_givenNumeric :: EquationSystemNumeric s
_givenNumeric =
   (XIdx.dTime ?= absoluteRecord 1) <>

   givenParameterNumber ein  4.00 (-0.6) param2 <>
   givenParameterNumber eta0 0.25   0.1  param1 <>
   givenParameterNumber eta1 0.85   0.05 param0 <>

   mempty

givenNumeric :: EquationSystemNumeric s
givenNumeric =
   (XIdx.dTime ?= absoluteRecord 1) <>

   Fold.fold
      (NonEmpty.zipWith ($)
         (givenParameterNumber ein  4.00 (-0.6) !:
          givenParameterNumber eta0 0.25   0.1  !:
          givenParameterNumber eta1 0.85   0.05 !:
          Empty.Cons)
         (NestedDelta.getParameterRecord params)) <>

   mempty



mainNumeric :: IO ()
mainNumeric = do

   let solved =
          EqSys.solve
             (quantityTopology LinearTwo.topology)
             givenNumeric

   case FlowTopo.lookupEnergy eout solved of
      Nothing -> error "undefined E_2_1"
      Just x -> do
         let assigns =
                Map.mapKeys termFromIndex $
                Record.assignDeltaMap x
         AssignMap.print assigns
         PlotIO.stack "Decomposition of total output energy"
            (formatValue $ RecIdx.delta $ Var.index eout)
            (AssignMap.ignoreUndetermined assigns)


main :: IO ()
main = mainNumeric >> mainSymbolic
