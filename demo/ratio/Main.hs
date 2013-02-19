{-# LANGUAGE FlexibleInstances #-}

-- | Demonstriert, wie man ein eta als Funktion definiert.

module Main where

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.System as EqGen
import EFA.Equation.System ((=.=))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility (checkedLookup)
import EFA.Graph (mkGraph)
import EFA.Example.Utility ((.=), constructSeqTopo, edgeVar, makeEdges, recAbs)

import Data.Ratio ((%), Ratio)

import Data.Monoid ((<>))
import Data.Foldable (foldMap)
import qualified EFA.Graph.Draw as Draw

sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0

data Node = Sink | Source deriving (Ord, Eq, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


linearOne :: TD.Topology Node
linearOne = mkGraph nodes (makeEdges edges)
  where nodes = [(Sink, TD.AlwaysSink), (Source, TD.AlwaysSource)]
        edges = [(Sink, Source)]

seqTopo :: TD.SequFlowGraph Node
seqTopo = constructSeqTopo linearOne [0]

enRange :: [Rational]
enRange = (1%100):[1%2, 1 .. 9]

c :: EqGen.ExprWithVars Node s a
c = edgeVar EqGen.power sec0 Source Sink

n :: EqGen.ExprWithVars Node s a
n = edgeVar EqGen.eta sec0 Source Sink

eta :: Idx.Eta Node
eta = edgeVar (Idx.Eta recAbs) sec0 Source Sink


functionEta :: EqGen.ExprWithVars Node s Rational -> EqGen.ExprWithVars Node s Rational
functionEta p = 0.2 * p

given :: Rational -> EqGen.EquationSystem Node s Rational
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

solveEnv ::
  Rational -> Env.Env Node Env.SingleRecord (EqGen.Result (Ratio Integer))
solveEnv p = EqGen.solve ((n =.= functionEta c) <> given p) seqTopo

main :: IO ()
main = do
  putStrLn $ unlines $ map solve enRange
  

  let env = solveEnv 0.5

  Draw.sequFlowGraphAbsWithEnv seqTopo env