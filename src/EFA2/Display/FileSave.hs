

module EFA2.Display.FileSave where

import qualified Data.Map as M
import Data.GraphViz
import Data.Graph.Inductive

import EFA2.Graph.GraphData
import EFA2.Graph.DependencyGraph
import EFA2.Term.Equation

-- dot -Tpdf topograph.dot -o topograph.pdf 


writeTopology :: (Show b, Show a) => Gr a b -> IO ()
writeTopology g = writeFile "results/topograph.dot" (graphviz' g)

writeDependencyGraph :: Gr NLabel ELabel -> [EqTerm NLabel] -> IO ()
writeDependencyGraph g given = writeFile "results/depgraph.dot" (graphviz' g')
  where g' = gmap f $ makeDependencyGraph g given
        f (ins, n, l, outs) = (ins, n, show n ++ ": " ++ showEqTerm l, outs)
