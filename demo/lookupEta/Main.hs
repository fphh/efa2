
-- | Demonstriert, wie man ein eta mit Hilfe von Lookup-Tables definiert.

module Main where

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Environment as Env
import qualified EFA.Example.Absolute as EqSys
import EFA.Example.Absolute ((.=), (=.=))
import EFA.Example.Utility (constructSeqTopo, edgeVar, makeEdges)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility (checkedLookup)
import EFA.Graph (mkGraph)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

import Data.Monoid (mconcat, (<>))


sec0 :: Idx.Section
sec0 :~ _ = Stream.enumFrom $ Idx.Section 0

sink, source :: Node.Int
sink :~ (source :~ _) = Stream.enumFrom $ Node.Int 0

linearOne :: TD.Topology Node.Int
linearOne = mkGraph nodes (makeEdges edges)
  where nodes = [(sink, TD.AlwaysSink), (source, TD.AlwaysSource)]
        edges = [(source, sink)]

seqTopo :: TD.SequFlowGraph Node.Int
seqTopo = constructSeqTopo linearOne [0]

enRange :: [Double]
enRange = 0.01:[0.5, 1 .. 9]

c :: Idx.Power Node.Int
c = edgeVar Idx.Power sec0 source sink

eta :: Idx.Eta Node.Int
eta = edgeVar Idx.Eta sec0 source sink

eval :: [(Double, Double)] -> Double -> Double
eval lt pin =
  case dropWhile ((< pin) . fst) lt of
       [] -> 0
       (_, v):_ -> v


lookupEta ::
   EqSys.Expression Node.Int s a v Double ->
   EqSys.Expression Node.Int s a v Double
lookupEta = EqSys.liftF $ eval table
  where table = zip [0..9] [0, 0.1, 0.3, 0.6, 0.7, 0.65, 0.6, 0.4, 0.35, 0.1]

given :: Double -> EqSys.EquationSystem Node.Int s Double Double
given p =
   mconcat $

   (Idx.DTime sec0 .= 1) :
   (c .= p) :
   []


solve :: Double -> String
solve p =
  let env =
         EqSys.solve seqTopo
            ((EqSys.variable eta =.= lookupEta (EqSys.variable c))
               <> given p)
  in  show p ++ " " ++
      Format.unUnicode (formatValue (Record.unAbsolute
         (checkedLookup (Env.etaMap $ Env.signal env) eta)))

main :: IO ()
main =
  putStrLn $ unlines $ map solve enRange
