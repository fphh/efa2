module Main where

import qualified EFA2.Example.Examples as Example
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

drawSeqGraph :: [FlowTopology] ->  IO ()
drawSeqGraph sol =
   drawTopologySimple .
   Flow.mkSequenceTopology .
   Flow.genSectionTopology .
   SequData . select sol . parse =<<
   prompt "Gib kommagetrennt die gewuenschten Sektionsindices ein: "


main :: IO ()
main = do
  let sol = StateAnalysis.advanced Example.topoDreibein

  drawAll $
    drawTopologyXs' sol :
    drawSeqGraph sol :
    []
