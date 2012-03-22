

module EFA2.Display.FileSave where

import qualified Data.Map as M
import Data.GraphViz
import Data.Graph.Inductive

import EFA2.Graph.Graph
import EFA2.Term.Term
import EFA2.Term.EquationOrder

-- dot -Tpdf topograph.dot -o topograph.pdf 


writeTopology :: (Show b, Show a) => Gr a b -> IO ()
writeTopology g = writeFile "results/topograph.dot" (graphviz' g)

writeDependencyGraph :: Gr NLabel ELabel -> IO ()
writeDependencyGraph g = writeFile "results/depgraph.dot" (graphviz' g')
  where g' = gmap f $ makeDependencyGraph g
        f (ins, n, l, outs) = (ins, n, show n ++ ": " ++ toString l, outs)
