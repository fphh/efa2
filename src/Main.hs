module Main where

import EFA2.Example.Examples
import EFA2.StateAnalysis.StateAnalysis
import EFA2.Display.DrawGraph

import EFA2.Topology.TopologyData
import EFA2.Topology.Flow
import EFA2.Signal.SequenceData
import EFA2.Signal.Base

import Debug.Trace
import Data.Char

interactIO :: String -> (String -> IO a) -> IO a
interactIO qstr f = do
  putStrLn qstr
  str <- getLine
  f str

interactA :: String -> (String -> a) -> IO a
interactA qstr f = do
  putStrLn qstr
  str <- getLine
  return (f str)

parse :: String -> [Int]
parse str = map (read . check) $ foldr f [[]] str
  where f ' ' acc = acc
        f ',' acc = []:acc
        f c (a:acc) = (c:a):acc
        check xs | all isDigit xs = xs
                 | otherwise = error "parse error: not a number!"

select :: [Topology] -> [Int] -> [Topology]
select _ [] = []
select ts (x:xs) = (ts !! x):(select ts xs)

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

  drawAll [
    drawTopologyXs' sol,
    drawSeqGraph sol ]

