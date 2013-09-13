{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Application.NestedDelta as NestedDelta
import qualified EFA.Application.AssignMap as AssignMap
import qualified EFA.Application.Symbolic as Symbolic
import qualified EFA.Application.Topology.LinearTwo as LinearTwo
import EFA.Application.Topology.LinearTwo (Node, node0, node1, node2)
import EFA.Application.NestedDelta
          (ParameterRecord,
           givenParameterSymbol, givenParameterNumber,
           beforeDelta, extrudeStart,
           (<&), (<&>), (&>), (&&>), (?=))
import EFA.Application.Utility (seqFlowGraphRecordFromStates)

import qualified EFA.Flow.Sequence.AssignMap as SeqFlowAssignMap
import qualified EFA.Flow.Sequence.EquationSystem as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Symbolic.OperatorTree as Op
import qualified EFA.Symbolic.Mixed as Term

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Signal.PlotIO as PlotIO

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import Data.NonEmpty ((!:))
import Data.Monoid (mempty, (<>))



sec0 :: Idx.Section
sec0 = Idx.Section 0


type SignalTerm = Symbolic.SignalTerm Idx.Delta SumProduct.Term Node
type ScalarTerm = Symbolic.ScalarTerm Idx.Delta SumProduct.Term Node

type IdxMultiDelta = Idx.ExtDelta (Idx.ExtDelta (Idx.ExtDelta Idx.Absolute))
type RecMultiDelta = Record.ExtDelta (Record.ExtDelta (Record.ExtDelta Record.Absolute))

type
   EquationSystemSymbolic s =
      EqSys.EquationSystem Symbolic.Ignore
         RecMultiDelta Node s ScalarTerm SignalTerm



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
   ParameterRecord
      (NonEmpty.T (NonEmpty.T (NonEmpty.T Empty.T)))
      RecMultiDelta a
params =
   beforeDelta &&> beforeDelta &&> beforeDelta &&> NestedDelta.parameterStart

absoluteRecord ::
   (Arith.Sum x) =>
   x -> RecMultiDelta (Result x)
absoluteRecord = NestedDelta.absoluteRecord absolute


eout, ein :: XIdx.Energy Node
ein  = XIdx.energy sec0 node0 node1
eout = XIdx.energy sec0 node2 node1

eta0, eta1 :: XIdx.Eta Node
eta0 = XIdx.eta sec0 node0 node1
eta1 = XIdx.eta sec0 node1 node2


termFromIndex ::
   IdxMultiDelta ->
   [Idx.Record Idx.Delta (Var.InSectionSignal Node)]
termFromIndex
      (Idx.ExtDelta r2 (Idx.ExtDelta r1 (Idx.ExtDelta r0 Idx.Absolute))) =
   Idx.Record r2 (Var.index ein) :
   Idx.Record r1 (Var.index eta0) :
   Idx.Record r0 (Var.index eta1) :
   []



_givenSymbolic :: EquationSystemSymbolic s
_givenSymbolic =
   (XIdx.dTime sec0 ?= absoluteRecord (Arith.fromInteger 1)) <>

   givenParameterSymbol ein  param2 <>
   givenParameterSymbol eta0 param1 <>
   givenParameterSymbol eta1 param0 <>

   mempty

givenSymbolic :: EquationSystemSymbolic s
givenSymbolic =
   (XIdx.dTime sec0 ?= absoluteRecord (Arith.fromInteger 1)) <>

   Fold.fold
      (NonEmpty.zipWith ($)
         (givenParameterSymbol ein  !:
          givenParameterSymbol eta0 !:
          givenParameterSymbol eta1 !:
          Empty.Cons)
         (NestedDelta.getParameterRecord params)) <>

   mempty


simplify ::SignalTerm -> SignalTerm
simplify =
   Term.Signal . Op.toNormalTerm .
   NonEmpty.sum . Op.expand .
   Op.fromNormalTerm . Term.getSignal

simplifiedSummands ::
   RecMultiDelta (Result SignalTerm) -> NonEmpty.T [] (Result SignalTerm)
simplifiedSummands =
   fmap (fmap simplify) . Record.summands

mainSymbolic :: IO ()
mainSymbolic = do

   let solved =
          EqSys.solve
             (seqFlowGraphRecordFromStates LinearTwo.topology [0])
             givenSymbolic

   putStrLn $ Format.unUnicode $ Format.lines $
      SeqFlowAssignMap.format $ SeqFlow.toAssignMap $
      SeqFlow.mapGraph Record.summands simplifiedSummands solved

   Draw.xterm $ Draw.sequFlowGraph Draw.optionsDefault $
      SeqFlow.mapGraph
         (Record.Absolute . Record.summands)
         (Record.Absolute . simplifiedSummands)
         solved


type
   EquationSystemNumeric s =
      EqSys.EquationSystem Symbolic.Ignore
         RecMultiDelta Node s Double Double


_givenNumeric :: EquationSystemNumeric s
_givenNumeric =
   (XIdx.dTime sec0 ?= absoluteRecord 1) <>

   givenParameterNumber ein  4.00 (-0.6) param2 <>
   givenParameterNumber eta0 0.25   0.1  param1 <>
   givenParameterNumber eta1 0.85   0.05 param0 <>

   mempty

givenNumeric :: EquationSystemNumeric s
givenNumeric =
   (XIdx.dTime sec0 ?= absoluteRecord 1) <>

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
             (seqFlowGraphRecordFromStates LinearTwo.topology [0])
             givenNumeric

   case SeqFlow.lookupEnergy eout solved of
      Nothing -> error "undefined E_2_1"
      Just x -> do
         let assigns =
                Map.mapKeys termFromIndex $
                Record.assignDeltaMap x
         AssignMap.print assigns
         PlotIO.stack "Decomposition of total output energy"
            (formatValue $ Idx.delta $ Var.index eout)
            (AssignMap.ignoreUndetermined assigns)


main :: IO ()
main = mainNumeric >> mainSymbolic
