-- | This example demonstrates the functionality of StateAnalysis

module Main where

import qualified EFA.Example.Topology.Tripod as Tripod

import qualified EFA.Flow.Draw as Draw

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph

import qualified EFA.Report.Format as Format

import qualified Data.List.Match as Match
import Data.Foldable (forM_)
import Data.List (sortBy, intercalate)


formatEdge ::
   (Node.C node) =>
   Graph.EitherEdge node -> String
formatEdge ee =
   case ee of
      Graph.EDirEdge (Graph.DirEdge x y) ->
         Format.unUnicode (Node.display x) ++
         "->" ++
         Format.unUnicode (Node.display y)
      Graph.EUnDirEdge (Graph.UnDirEdge x y) ->
         Format.unUnicode (Node.display x) ++
         "--" ++
         Format.unUnicode (Node.display y)

formatEdges ::
   (Node.C node) =>
   [[Graph.EitherEdge node]] -> String
formatEdges =
   intercalate " | " . map (intercalate ", " . map formatEdge)

main :: IO ()
main = do
   let sol = StateAnalysis.advanced Tripod.topology
   forM_ (zip [(0::Integer) ..] sol) $ \(n, topo) ->
      putStrLn $ ((show n ++ ": ") ++) $
         formatEdges $ sortBy Match.compareLength $
         StateAnalysis.minimalGiven topo
   Draw.xterm $ Draw.flowTopologies sol
