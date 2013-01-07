
module EFA2.IO.FileSave where

import EFA2.Solver.Equation (EqTerm, formatTerm)
import qualified EFA2.Report.Format as Format

import Data.Graph.Inductive (Gr, nmap, graphviz')

-- dot -Tpdf topograph.dot -o topograph.pdf 
{-

writeTopology :: (Show b, Show a) => Gr a b -> IO ()
writeTopology g = writeFile "results/topograph.dot" (graphviz' g)
-}


writeDependencyGraph :: Gr EqTerm () -> IO ()
writeDependencyGraph g = writeFile "results/depgraph.dot" (graphviz' (nmap (Format.unASCII . formatTerm) g))
