module EFA2.Solver.DependencyGraph where

import EFA2.Solver.Equation (EqTerm, mkVarSet)
import EFA2.Utils.Utils (diffByAtMostOne, hasSameVariable)

import Data.Graph.Inductive (Graph(..), DynGraph(..), Gr, nmap, newNodes, insNode)
import Data.Maybe (mapMaybe)
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (swap)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.HT as HTL


dependencyGraph :: (S.Set EqTerm -> S.Set EqTerm -> Bool) -> [S.Set EqTerm] -> Gr (S.Set EqTerm) ()
dependencyGraph p vsets = g
  where xs = HTL.removeEach vsets
        ys = concatMap (uncurry (mkArcs p)) xs
        m = M.fromList (zip vsets [0..])
        es = map (\(x, y) -> (m M.! x, m M.! y, ())) ys
        g = mkGraph (map swap $ M.toList m) es

mkArcs :: (Ord a, Show a) => (S.Set a -> S.Set a -> Bool) -> S.Set a -> [S.Set a] -> [(S.Set a, S.Set a)]
mkArcs p s = mapMaybe (\t -> toMaybe (p s t) (s, t))

makeDependencyGraph :: (EqTerm -> Bool) -> (S.Set EqTerm -> S.Set EqTerm -> Bool) -> [EqTerm] -> Gr EqTerm ()
makeDependencyGraph isVar p ts = deq
  where vsets = map (mkVarSet isVar) ts
        mt = M.fromList (zip vsets ts)
        dg = dependencyGraph p vsets
        deq = nmap (mt M.!) dg

addToDependencyGraph :: [EqTerm] -> Gr EqTerm () -> Gr EqTerm ()
addToDependencyGraph ts dpg = L.foldl' f dpg ns
  where xs = newNodes (length ts) dpg
        ns = zip xs ts
        f g n = insNode n g

-- | The produced graph has an edge, iff the solution of one node allows for computing the solution 
--   of the other node and the other node has exactly one unknown variable.
dpgDiffByAtMostOne :: (EqTerm -> Bool) -> [EqTerm] -> Gr EqTerm ()
dpgDiffByAtMostOne isVar = makeDependencyGraph isVar diffByAtMostOne


-- | The resulting graph has an edge iff two nodes have one or more variables in common.
dpgHasSameVariable :: (EqTerm -> Bool) -> [EqTerm] -> Gr EqTerm ()
dpgHasSameVariable isVar = makeDependencyGraph isVar hasSameVariable

{-
-- | Produces a graph that has an edge iff there is a variable intersection between two nodes
--   and there is no path in the 'dpgDiffByAtMostOne'-graph beween these two nodes.
dpg :: (EqTerm -> Bool) -> [EqTerm] -> Gr EqTerm ()
dpg isVar ts = L.foldl' (flip delEdge) dpg2 (edges dpg1)
  where dpg1 = dpgDiffByAtMostOne isVar ts
        dpg2 = dpgHasSameVariable isVar ts
-}
