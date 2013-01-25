{-# LANGUAGE FlexibleInstances #-}

-- | Demonstriert, wie man ein eta als Funktion definiert.

module Main where

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.System as EqGen
import EFA.Equation.System ((=.=))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility (checkedLookup)
import EFA.Graph (mkGraph)
import EFA.Example.Utility ((.=), constructSeqTopo, edgeVar, makeEdges, recAbs)

import Data.Ratio ((%))

import Data.Monoid ((<>))
import Data.Foldable (foldMap)


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0

sink, source :: Idx.Node
sink :~ (source :~ _) = Stream.enumFrom $ Idx.Node 0

linearOne :: TD.Topology
linearOne = mkGraph nodes (makeEdges edges)
  where nodes = [(sink, TD.AlwaysSink), (source, TD.AlwaysSource)]
        edges = [(source, sink)]

seqTopo :: TD.SequFlowGraph
seqTopo = constructSeqTopo linearOne [0]

enRange :: [Rational]
enRange = (1%100):[1%2, 1 .. 9]

c :: EqGen.ExprWithVars s a
c = edgeVar EqGen.power sec0 source sink

n :: EqGen.ExprWithVars s a
n = edgeVar EqGen.eta sec0 source sink

eta :: Idx.Eta
eta = edgeVar (Idx.Eta recAbs) sec0 source sink


functionEta :: EqGen.ExprWithVars s Rational -> EqGen.ExprWithVars s Rational
functionEta p = 0.2 * p

given :: Rational -> EqGen.EquationSystem s Rational
given p =
   foldMap (uncurry (.=)) $
   (EqGen.dtime sec0, 1) :
   (edgeVar EqGen.power sec0 source sink, p) : []


solve :: Rational -> String
solve p =
  let env = EqGen.solve ((n =.= functionEta c) <> given p) seqTopo
  in  show p ++ "\t" 
        ++ case checkedLookup (Env.etaMap env) eta of
                EqGen.Undetermined -> "undetermined"
                EqGen.Determined x -> show x

main :: IO ()
main =
  putStrLn $ unlines $ map solve enRange
