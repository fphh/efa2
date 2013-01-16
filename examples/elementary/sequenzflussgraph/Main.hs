module Main where

import EFA2.Example.Utility (makeNodes, makeSimpleEdges)

import qualified EFA2.Graph.Topology.StateAnalysis as StateAnalysis
import EFA2.Graph.Draw (drawTopologyXs', drawTopologySimple, drawAll)

import qualified EFA2.Graph.Topology as TD
import qualified EFA2.Graph.Flow as Flow
import EFA2.Graph (mkGraph)
import EFA2.Signal.SequenceData

import Data.List.HT (chop)
import Data.Char (isSpace)


topoDreibein :: TD.Topology
topoDreibein = mkGraph (makeNodes ns) (makeSimpleEdges es)
  where ns = [ (0, TD.NoRestriction),
               (1, TD.NoRestriction),
               (2, TD.Crossing),
               (3, TD.NoRestriction) ]
        es = [ (0, 2), (1, 2), (2, 3) ]


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

drawSeqGraph :: [TD.FlowTopology] ->  IO ()
drawSeqGraph sol =
   drawTopologySimple .
   Flow.mkSequenceTopology .
   Flow.genSectionTopology .
   SequData . select sol . parse =<<
   prompt "Gib kommagetrennt die gewuenschten Sektionsindices ein: "


main :: IO ()
main = do
  let sol = StateAnalysis.advanced topoDreibein

  drawAll $
    drawTopologyXs' sol :
    drawSeqGraph sol :
    []
