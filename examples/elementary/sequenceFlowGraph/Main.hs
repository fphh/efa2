module Main where

import EFA.Application.Utility (seqFlowGraphFromFlowTopos, topologyFromEdges)

import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import EFA.Equation.Result (Result)

import EFA.Utility.Async (concurrentlyMany_)

import Data.List.HT (chop)
import Data.Char (isSpace)


node0, node1, node2, node3 :: Node.Int
node0 = Node.intNoRestriction 0
node1 = Node.intNoRestriction 1
node2 = Node.intCrossing 0
node3 = Node.intStorage 0


topoTripod :: Topo.Topology Node.Int
topoTripod =
   topologyFromEdges
      [ (node0, node2), (node1, node2), (node2, node3) ]


interactIO :: String -> (String -> IO a) -> IO a
interactIO qstr f = do
   putStrLn qstr
   f =<< getLine

prompt :: String -> IO String
prompt qstr = putStrLn qstr >> getLine

parse :: String -> [Int]
parse = map readNum . chop (','==)

readNum :: (Read a, Num a) => String -> a
readNum s =
   case reads s of
      [(n, trailer)] ->
         if all isSpace trailer
           then n
           else error $ "cannot handle characters \"" ++ trailer ++ "\""
      _ -> error "parse error: not a number!"

drawSeqGraph :: [Topo.FlowTopology Node.Int] ->  IO ()
drawSeqGraph sol = do
   xs <- parse `fmap`
           prompt "Gib kommagetrennt die gewuenschten Sektionsindices ein: "
   Draw.xterm $
      Draw.seqFlowGraph Draw.optionsDefault $
      SeqFlow.mapGraph
         (\a -> a :: Result Double)
         (\v -> v :: Result Double) $
      seqFlowGraphFromFlowTopos $ map (sol !!) xs


main :: IO ()
main = do
   let sol = StateAnalysis.advanced topoTripod

   concurrentlyMany_ $
      (Draw.xterm $ Draw.flowTopologies sol) :
      drawSeqGraph sol :
      []
