{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Example.Utility as Utility
import EFA.Example.Utility
          (symbol, edgeVar, makeEdges, constructSeqTopo)
import EFA.Equation.System ((=.=))

import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Variable as Var
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph as Gr

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Data.Accessor.Basic as Accessor

import qualified UniqueLogic.ST.System as Sys


import Control.Applicative (pure)
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

type IdxMultiDelta = Idx.ExtDelta (Idx.ExtDelta Idx.Delta)
type RecMultiDelta = Record.ExtDelta (Record.ExtDelta Record.Delta)

type
   EquationSystemSymbolic s =
      EqGen.EquationSystem RecMultiDelta Node.Int s ScalarTerm SignalTerm


infixr 9 &

(&) :: Idx.Delta -> a -> Idx.ExtDelta a
(&) = Idx.ExtDelta

absolute_ :: idx -> Idx.Record IdxMultiDelta idx
absolute_ = Idx.Record absolute

absolute, param0, param1, param2, del :: IdxMultiDelta
absolute = Idx.Before & Idx.Before & Idx.Before
param0 =   Idx.Before & Idx.Before & Idx.Delta
param1 =   Idx.Before & Idx.Delta  & Idx.Before
param2 =   Idx.Delta  & Idx.Before & Idx.Before
del    =   Idx.Delta  & Idx.Delta  & Idx.Delta


{- |
Caution:
This function creates an inconsistent nested Delta record.
The equations before+delta=after are not (always) satisfied.
This is ok for application since we only use the before and delta values
to initialize the equation system.
-}
parameterSymbol ::
   (t ~ Utility.VarTerm var Idx.Delta SumProduct.Term Node.Int,
    Eq t, Arith.Sum t, Arith.Constant t,
    Ord (idx Node.Int),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   IdxMultiDelta -> idx Node.Int -> RecMultiDelta t

parameterSymbol param idx =
   Accessor.set (Record.access param) (symbol (Idx.delta $ Var.index idx)) $
   absoluteSymbol idx

{- | Caution: See 'parameterSymbol' -}
absoluteSymbol ::
   (t ~ Utility.VarTerm var Idx.Delta SumProduct.Term Node.Int,
    Eq t, Arith.Sum t, Arith.Constant t,
    Ord (idx Node.Int),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   idx Node.Int -> RecMultiDelta t

absoluteSymbol idx =
   absoluteRecord (symbol (Idx.before $ Var.index idx))

{- | Caution: See 'parameterSymbol' -}
absoluteRecord ::
   (Arith.Constant x) =>
   x -> RecMultiDelta x
absoluteRecord x =
   Accessor.set (Record.access absolute) x $
   pure Arith.zero


equalDelta ::
   Eq x =>
   Record.Delta (EqGen.Expression rec node s a v x) ->
   Record.Delta (EqGen.Expression rec node s a v x) ->
   EqGen.EquationSystem rec node s a v
equalDelta x y =
   (Record.before x =.= Record.before y) <>
   (Record.delta  x =.= Record.delta  y)

equalExtDelta ::
   (f x -> f x -> EqGen.EquationSystem rec node s a v) ->
   Record.ExtDelta f x ->
   Record.ExtDelta f x ->
   EqGen.EquationSystem rec node s a v
equalExtDelta eq x y =
   eq (Record.extBefore x) (Record.extBefore y) <>
   eq (Record.extDelta  x) (Record.extDelta  y)


infix 0 =%=, %=

(=%=) ::
   Eq x =>
   RecMultiDelta (EqGen.Expression rec node s a v x) ->
   RecMultiDelta (EqGen.Expression rec node s a v x) ->
   EqGen.EquationSystem rec node s a v
(=%=) = equalExtDelta (equalExtDelta equalDelta)

(%=) ::
   (Eq x, Arith.Sum x,
    EqGen.Element idx RecMultiDelta s a v ~ RecMultiDelta (Sys.Variable s x),
    Env.AccessMap idx, Ord (idx node), Var.Type idx ~ var) =>
   idx node -> RecMultiDelta x ->
   EqGen.EquationSystem RecMultiDelta node s a v
evar %= val  =
   fmap (EqGen.variable . flip Idx.Record evar) Record.indices
   =%=
   fmap EqGen.constant val


givenParameterSymbol ::
   (t ~ Utility.VarTerm var Idx.Delta SumProduct.Term Node.Int,
    Eq t, Arith.Sum t, Arith.Constant t,
    EqGen.Element idx RecMultiDelta s ScalarTerm SignalTerm
      ~ RecMultiDelta (Sys.Variable s t),
    Ord (idx Node.Int),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   IdxMultiDelta -> idx Node.Int ->
   EquationSystemSymbolic s
givenParameterSymbol param idx =
   idx %= parameterSymbol param idx

givenSymbolic :: EquationSystemSymbolic s
givenSymbolic =
   (Idx.DTime Idx.initSection %= absoluteRecord (Arith.fromInteger 1)) <>
   (Idx.DTime sec0 %= absoluteRecord (Arith.fromInteger 1)) <>

   givenParameterSymbol param0 (edgeVar Idx.Energy sec0 node0 node1) <>
   givenParameterSymbol param1 (edgeVar Idx.Eta sec0 node0 node1) <>
   givenParameterSymbol param2 (edgeVar Idx.Eta sec0 node1 node2) <>

   mempty


main :: IO ()
main = do

   let seqTopo = constructSeqTopo topoLinear [0]
   let (Env.Complete scalarEnv signalEnv) =
          EqGen.solve seqTopo givenSymbolic

   putStrLn $ Format.unUnicode $ formatValue $
      Env.Complete
         (fmap Record.summands scalarEnv)
         (fmap Record.summands signalEnv)

   Draw.sequFlowGraphAbsWithEnv seqTopo $
      Env.Complete
         (fmap (Record.Absolute . Record.summands) scalarEnv)
         (fmap (Record.Absolute . Record.summands) signalEnv)
