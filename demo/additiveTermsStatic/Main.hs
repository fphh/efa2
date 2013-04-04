{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Example.NestedDelta as NestedDelta
import qualified EFA.Example.AssignMap as AssignMap
import qualified EFA.Example.Utility as Utility
import qualified EFA.Example.Index as XIdx
import EFA.Example.NestedDelta
          (ParameterRecord,
           givenParameterSymbol, givenParameterNumber,
           beforeDelta, extrudeStart,
           (<&), (<&>), (&>), (&&>), (?=))
import EFA.Example.Utility (makeEdges, constructSeqTopo)
import EFA.Equation.Result (Result)

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Symbolic.SumProduct as SumProduct
import qualified EFA.Symbolic.OperatorTree as Op
import qualified EFA.Symbolic.Mixed as Term

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Signal.Plot as Plot

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.NonEmpty as NonEmpty
import Data.NonEmpty ((!:))
import Data.Monoid (mempty, (<>))



sec0 :: Idx.Section
sec0 = Idx.Section 0

node0, node1, node2 :: Node.Int
node0 :~ node1 :~ node2 :~ _ = Stream.enumFrom minBound


topoLinear :: TD.Topology Node.Int
topoLinear = Gr.mkGraph ns (makeEdges es)
  where ns = [(node0, TD.Source),
              (node1, TD.Crossing),
              (node2, TD.Sink)]
        es = [(node0, node1), (node1, node2)]


type SignalTerm = Utility.SignalTerm Record.Delta SumProduct.Term Node.Int
type ScalarTerm = Utility.ScalarTerm Record.Delta SumProduct.Term Node.Int

type IdxMultiDelta = Idx.ExtDelta (Idx.ExtDelta (Idx.ExtDelta Idx.Absolute))
type RecMultiDelta = Record.ExtDelta (Record.ExtDelta (Record.ExtDelta Record.Absolute))

type
   EquationSystemSymbolic s =
      EqGen.EquationSystem RecMultiDelta Node.Int s ScalarTerm SignalTerm



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
      (NonEmpty.T (NonEmpty.T (NonEmpty.T NonEmpty.Empty)))
      RecMultiDelta a
params =
   beforeDelta &&> beforeDelta &&> beforeDelta &&> NestedDelta.parameterStart

absoluteRecord ::
   (Arith.Sum x) =>
   x -> RecMultiDelta (Result x)
absoluteRecord = NestedDelta.absoluteRecord absolute


eout, ein :: XIdx.Energy Node.Int
ein  = XIdx.energy sec0 node0 node1
eout = XIdx.energy sec0 node2 node1

eta0, eta1 :: XIdx.Eta Node.Int
eta0 = XIdx.eta sec0 node0 node1
eta1 = XIdx.eta sec0 node1 node2


termFromIndex :: IdxMultiDelta -> [Idx.Record Idx.Delta (Var.Signal Node.Int)]
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
          NonEmpty.Empty)
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

   let seqTopo = constructSeqTopo topoLinear [0]
   let (Env.Complete scalarEnv signalEnv) =
          EqGen.solve seqTopo givenSymbolic

   putStrLn $ Format.unUnicode $ formatValue $
      Env.Complete
         (fmap Record.summands scalarEnv)
         (fmap simplifiedSummands signalEnv)

   Draw.sequFlowGraphAbsWithEnv "" seqTopo $
      Env.Complete
         (fmap (Record.Absolute . Record.summands) scalarEnv)
         (fmap (Record.Absolute . simplifiedSummands) signalEnv)


type
   EquationSystemNumeric s =
      EqGen.EquationSystem RecMultiDelta Node.Int s Double Double


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
          NonEmpty.Empty)
         (NestedDelta.getParameterRecord params)) <>

   mempty



mainNumeric :: IO ()
mainNumeric = do

   let seqTopo = constructSeqTopo topoLinear [0]
       Env.Complete _scalarEnv signalEnv = EqGen.solve seqTopo givenNumeric

   case Map.lookup eout (Env.energyMap signalEnv) of
      Nothing -> error "undefined E_2_1"
      Just x -> do
         let assigns =
                Map.mapKeys termFromIndex $
                Record.assignDeltaMap x
         AssignMap.print assigns
         Plot.stackIO "Decomposition of total output energy"
            (Idx.delta $ Var.index eout)
            (AssignMap.ignoreUndetermined assigns)


main :: IO ()
main = mainNumeric >> mainSymbolic
