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

data Nodes = Sink | Source deriving (Ord, Eq, Show)

linearOne :: TD.Topology Nodes
linearOne = mkGraph nodes (makeEdges edges)
  where nodes = [(Source, TD.AlwaysSink), (Sink, TD.AlwaysSource)]
        edges = [(Source, Sink)]

seqTopo :: TD.SequFlowGraph Nodes
seqTopo = constructSeqTopo linearOne [0]

enRange :: [Rational]
enRange = (1%100):[1%2, 1 .. 9]

c :: EqGen.ExprWithVars Nodes s a
c = edgeVar EqGen.power sec0 Source Sink

n :: EqGen.ExprWithVars Nodes s a
n = edgeVar EqGen.eta sec0 Source Sink

eta :: Idx.Eta Nodes
eta = edgeVar (Idx.Eta recAbs) sec0 Source Sink


functionEta :: EqGen.ExprWithVars Nodes s Rational -> EqGen.ExprWithVars Nodes s Rational
functionEta p = 0.2 * p

given :: Rational -> EqGen.EquationSystem Nodes s Rational
given p =
   foldMap (uncurry (.=)) $
   (EqGen.dtime sec0, 1) :
   (edgeVar EqGen.power sec0 Source Sink, p) : []


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
