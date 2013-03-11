{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified EFA.Example.Utility as Utility
import EFA.Example.Utility
          (symbol, edgeVar, makeEdges, constructSeqTopo, (.=))

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

import qualified UniqueLogic.ST.System as Sys


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


absolute, param0, param1, param2 :: idx -> Idx.Record IdxMultiDelta idx
absolute = Idx.Record (Idx.Before & Idx.Before & Idx.Before)
param0 =   Idx.Record (Idx.Before & Idx.Before & Idx.Delta )
param1 =   Idx.Record (Idx.Before & Idx.Delta  & Idx.Before)
param2 =   Idx.Record (Idx.Delta  & Idx.Before & Idx.Before)

givenParameterSymbol ::
   (t ~ Utility.VarTerm var Idx.Delta SumProduct.Term Node.Int,
    Eq t, Arith.Sum t,
    EqGen.Element idx RecMultiDelta s ScalarTerm SignalTerm
      ~ RecMultiDelta (Sys.Variable s t),
    Ord (idx Node.Int),
    Var.Type idx ~ var, Utility.Symbol var, Env.AccessMap idx) =>

   (idx Node.Int -> Idx.Record IdxMultiDelta (idx Node.Int)) ->
   idx Node.Int ->
   EquationSystemSymbolic s
givenParameterSymbol param idx =
   (absolute idx .= symbol (Idx.before $ Var.index idx))
   <>
   (param idx .= symbol (Idx.delta $ Var.index idx))

givenSymbolic :: EquationSystemSymbolic s
givenSymbolic =
   (absolute (Idx.DTime Idx.initSection) .= Arith.fromInteger 1) <>
   (absolute (Idx.DTime sec0) .= Arith.fromInteger 1) <>

   givenParameterSymbol param0 (edgeVar Idx.Energy sec0 node0 node1) <>
   givenParameterSymbol param1 (edgeVar Idx.Eta sec0 node0 node1) <>
   givenParameterSymbol param2 (edgeVar Idx.Eta sec0 node1 node2) <>

   mempty


{- |
These function only fetch the before and delta components,
in contrast to Fold.toList.
-}
flatten :: Record.Delta a -> [a]
flatten r = [Record.before r, Record.delta r]

extFlatten :: (rec a -> [a]) -> Record.ExtDelta rec a -> [a]
extFlatten f r = f (Record.extBefore r) ++ f (Record.extDelta r)

multiFlatten :: RecMultiDelta a -> [a]
multiFlatten = extFlatten (extFlatten flatten)


main :: IO ()
main = do

   let seqTopo = constructSeqTopo topoLinear [0]
   let (Env.Complete scalarEnv signalEnv) =
          EqGen.solve seqTopo givenSymbolic

   putStrLn $ Format.unUnicode $ formatValue $
      Env.Complete
         (fmap multiFlatten scalarEnv)
         (fmap multiFlatten signalEnv)

   Draw.sequFlowGraphAbsWithEnv seqTopo $
      Env.Complete
         (fmap (Record.Absolute . multiFlatten) scalarEnv)
         (fmap (Record.Absolute . multiFlatten) signalEnv)
