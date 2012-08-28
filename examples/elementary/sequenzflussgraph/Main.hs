module Main where

import EFA2.Example.Examples
import EFA2.StateAnalysis.StateAnalysis
import EFA2.Display.DrawGraph

import EFA2.Topology.TopologyData
import EFA2.Topology.Flow
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

idx :: [SecIdx]
idx = map SecIdx [0..]

drawSeqGraph :: [Topology] ->  IO ()
drawSeqGraph sol = do
  let f = map topoToFlowTopo . select sol . parse
  fts <- interactA "Gib kommagetrennt die gewuenschten Sektionsindices ein: " f
  let sd = SequData (zipWith mkSectionTopology idx fts)
      seqGraph = mkSequenceTopology sd
  drawTopologySimple seqGraph


main :: IO ()
main = do
  let sol = map reorderEdges $ stateAnalysis topoDreibein

  drawAll $
    drawTopologyXs' sol :
    drawSeqGraph sol :
    []
