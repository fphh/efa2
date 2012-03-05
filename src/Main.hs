
module Main where


import Data.Graph.Inductive

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import EFA2.Term.Term
import EFA2.Term.EquationOrder

import EFA2.Graph.Graph
import EFA2.Utils.Utils


writeTopology :: (Show b, Show a) => Gr a b -> IO ()
writeTopology g = writeFile "results/topograph.dot" (graphviz' g)

writeDependencyGraph :: Gr NLabel ELabel -> IO ()
writeDependencyGraph g = writeFile "results/depgraph.dot" (graphviz' (nmap toString deq))
  where ts = mkEdgeEq g ++ mkNodeEq g
        vsets = map mkVarSet ts
        mt = M.fromList (zip vsets ts)
        dg = dependencyGraph vsets
        deq = nmap (mt M.!) dg 

main :: IO ()
main = do
  let input = S.fromList [Energy 4 3, Energy 0 1]
  writeTopology g
  writeDependencyGraph g


  putStrLn (termsStr $ makeEquations g input (Energy 6 5))
