module Main where

import EFA2.Example.Examples
import qualified EFA2.StateAnalysis.StateAnalysis as StateAnalysis
import EFA2.Topology.Draw

import EFA2.Topology.TopologyData
import qualified EFA2.Topology.Flow as Flow
import EFA2.Signal.SequenceData

import Data.List.HT (chop)
import Data.Char (isSpace)


interactIO :: String -> (String -> IO a) -> IO a
interactIO qstr f = do
  putStrLn qstr
  f =<< getLine

interactA :: String -> (String -> a) -> IO a
interactA qstr f = do
  putStrLn qstr
  fmap f getLine

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

drawSeqGraph :: [FlowTopology] ->  IO ()
drawSeqGraph sol =
   drawTopologySimple .
   Flow.mkSequenceTopology .
   Flow.genSectionTopology . SequData =<<
   interactA "Gib kommagetrennt die gewuenschten Sektionsindices ein: "
      (select sol . parse)


main :: IO ()
main = do
  let sol = StateAnalysis.advanced topoDreibein

  drawAll $
    drawTopologyXs' sol :
    drawSeqGraph sol :
    []
