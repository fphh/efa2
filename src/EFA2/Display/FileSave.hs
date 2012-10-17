
module EFA2.Display.FileSave where

import qualified Data.Map as M
import Data.Graph.Inductive (Gr, labNodes, nmap, graphviz')

import EFA2.Solver.Equation (EqTerm, showEqTerm)

-- dot -Tpdf topograph.dot -o topograph.pdf 
{-

writeTopology :: (Show b, Show a) => Gr a b -> IO ()
writeTopology g = writeFile "results/topograph.dot" (graphviz' g)
-}


writeDependencyGraph :: Gr EqTerm () -> IO ()
writeDependencyGraph g = writeFile "results/depgraph.dot" (graphviz' (nmap showEqTerm g))
  where m = M.fromList $ labNodes g
        _nshow x = show x ++ ": " ++ showEqTerm (m M.! x)
