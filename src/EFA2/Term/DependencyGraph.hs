{-# LANGUAGE ScopedTypeVariables #-}


module EFA2.Term.DependencyGraph where

import Data.Graph.Inductive
import Data.Maybe

import qualified Data.Set as S
import qualified Data.Map as M
--import qualified Data.List as L
import qualified Data.List.HT as HTL

import Debug.Trace

import EFA2.Utils.Utils
import EFA2.Term.Equation


dependencyGraph :: [S.Set EqTerm] -> Gr (S.Set EqTerm) ()
dependencyGraph ss = g
  where xs = HTL.removeEach ss
        ys = concatMap (uncurry mkArcs) xs
        m = M.fromList (zip ss [0..])
        es = unique $ map (\(x, y) -> (m M.! x, m M.! y, ())) ys
        g = mkGraph (map flipPair $ M.toList m) es

mkArcs :: (Ord a, Show a) => S.Set a -> [S.Set a] -> [(S.Set a, S.Set a)]
mkArcs s ss = catMaybes $ map g ss
  where g t | s `diffByAtMostOne` t = Just (s, t)
        g _ = Nothing

-- If true, then we have an edge from s to t.
diffByAtMostOne :: (Eq a, Ord a, Show a) => S.Set a -> S.Set a -> Bool
diffByAtMostOne s t = (S.size t > 1) && (S.size (t S.\\ s) == 1)

makeDependencyGraph :: [EqTerm] -> Gr EqTerm ()
makeDependencyGraph ts = deq
  where vsets = map (mkVarSet isVar) ts
        mt = M.fromList (zip vsets ts)
        dg = dependencyGraph vsets
        deq = nmap (mt M.!) dg
{-
makeDependencyGraph :: TheGraph b -> [EqTerm] -> Gr EqTerm ()
makeDependencyGraph (TheGraph g _) given = deq
  where ts = mkEdgeEq g ++ mkNodeEq g ++ given
        vsets = map (mkVarSet isVar) ts
        mt = M.fromList (zip vsets ts)
        dg = dependencyGraph vsets
        deq = nmap (mt M.!) dg
-}