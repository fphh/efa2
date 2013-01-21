
module Main where

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.System as EqSys
import EFA.Equation.System ((=.=))

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility (checkedLookup)
import EFA.Graph (mkGraph)
import EFA.Example.Utility ((.=), constructSeqTopo, edgeVar, makeEdges, recAbs)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

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

enRange :: [Double]
enRange = 0.01:[0.5, 1 .. 9]

c :: EqSys.ExprWithVars s a
c = edgeVar EqSys.power sec0 source sink

n :: EqSys.ExprWithVars s a
n = edgeVar EqSys.eta sec0 source sink

eval :: [(Double, Double)] -> Double -> Double
eval lt pin =
  case dropWhile ((< pin) . fst) lt of
       [] -> 0
       (_, v):_ -> v


lookupEta :: EqSys.ExprWithVars s Double -> EqSys.ExprWithVars s Double
lookupEta = EqSys.makeFunc $ eval table
  where table = zip [0..9] [0, 0.1, 0.3, 0.6, 0.7, 0.65, 0.6, 0.4, 0.35, 0.1]

given :: Double -> EqSys.EquationSystem s Double
given p =
   foldMap (uncurry (.=)) $
   (EqSys.dtime sec0, 1) :
   (edgeVar EqSys.power sec0 source sink, p) : []

eta :: Idx.Eta
eta = edgeVar (Idx.Eta recAbs) sec0 source sink


solve :: Double -> String
solve p =
  let env = EqSys.solve ((n =.= lookupEta c) <> given p) seqTopo
  in  show p ++ " " ++
      Format.unUnicode (formatValue (checkedLookup (Env.etaMap env) eta))

main :: IO ()
main =
  putStrLn $ unlines $ map solve enRange
