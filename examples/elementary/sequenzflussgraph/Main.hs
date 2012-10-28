module Main where

import EFA2.Example.Examples
import EFA2.StateAnalysis.StateAnalysis
import EFA2.Topology.Draw

import EFA2.Topology.TopologyData
import qualified EFA2.Topology.Flow as Flow
import EFA2.Signal.SequenceData

import Data.List.HT (chop)


interactIO :: String -> (String -> IO a) -> IO a
interactIO qstr f = do
  putStrLn qstr
  f =<< getLine

interactA :: String -> (String -> a) -> IO a
interactA qstr f = do
  putStrLn qstr
  fmap f getLine

parse :: String -> [Int]
parse str = map readNum $ chop (','==) $ filter (' '/=) str
  where readNum s =
           case reads s of
              [(n, "")] -> n
              _ -> error "parse error: not a number!"

select :: [Topology] -> [Int] -> [Topology]
select ts = map (ts!!)

drawSeqGraph :: [Topology] ->  IO ()
drawSeqGraph sol =
   drawTopologySimple .
   Flow.mkSequenceTopology .
   Flow.genSectionTopology . SequData =<<
   interactA "Gib kommagetrennt die gewuenschten Sektionsindices ein: "
      (map topoToFlowTopo . select sol . parse)


main :: IO ()
main = do
  let sol = map reorderEdges $ stateAnalysis topoDreibein

  drawAll $
    drawTopologyXs' sol :
    drawSeqGraph sol :
    []
