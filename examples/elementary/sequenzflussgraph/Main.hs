module Main where

import EFA.Application.Utility (makeEdges)

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Draw as Draw
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr

import qualified EFA.Signal.SequenceData as SD

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility.Async (concurrentlyMany_)

import Data.List.HT (chop)
import Data.Char (isSpace)

node0, node1, node2, node3 :: Node.Int
node0 :~ node1 :~ node2 :~ node3 :~ _ = Stream.enumFrom minBound


topoDreibein :: TD.Topology Node.Int
topoDreibein = Gr.fromList ns (makeEdges es)
  where ns = [ (node0, TD.NoRestriction),
               (node1, TD.NoRestriction),
               (node2, TD.Crossing),
               (node3, TD.storage) ]
        es = [ (node0, node2), (node1, node2), (node2, node3) ]


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

select :: [topo] -> [Int] -> [topo]
select ts = map (ts!!)

drawSeqGraph :: [TD.FlowTopology Node.Int] ->  IO ()
drawSeqGraph sol = do
   xs <- parse `fmap`
           prompt "Gib kommagetrennt die gewuenschten Sektionsindices ein: "
   Draw.xterm $
     Draw.sequFlowGraph $
       (Flow.sequenceGraph (SD.fromList $ select sol xs))


main :: IO ()
main = do
  let sol = StateAnalysis.advanced topoDreibein

  concurrentlyMany_ [
    Draw.xterm $ Draw.flowTopologies sol,
    drawSeqGraph sol ]
