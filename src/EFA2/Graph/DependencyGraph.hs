{-# LANGUAGE ScopedTypeVariables #-}


module EFA2.Graph.DependencyGraph where

import Data.Graph.Inductive
import Data.Maybe

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.List.HT as HTL

import Debug.Trace

import EFA2.Utils.Utils
import EFA2.Term.Equation
import EFA2.Graph.GraphData

dependencyGraph :: forall a. (Ord a) => [S.Set a] -> Gr (S.Set a) ()
dependencyGraph ss = g
  where --xs = eachWithEvery ss
        xs = HTL.removeEach ss
        ys = concatMap (uncurry mkArcs) xs
        m = M.fromList (zip ss [0..])
        es = unique $ map (\(x, y) -> (m M.! x, m M.! y, ())) ys
        g :: Gr (S.Set a) ()
        g = mkGraph (map flipPair $ M.toList m) es

mkArcs :: (Ord a) => S.Set a -> [S.Set a] -> [(S.Set a, S.Set a)]
mkArcs s ss = catMaybes $ map g ss
  where g t | s `diffByAtMostOne` t = Just (s, t)
        g _ = Nothing

-- If true, then we have an edge from s to t.
diffByAtMostOne :: (Eq a, Ord a) => S.Set a -> S.Set a -> Bool
--diffByAtMostOne s t = (S.size t > 1) && S.size (S.difference t s) == 1
diffByAtMostOne s t = (S.size t > 1) && (S.isSubsetOf t s || S.size (S.difference t s) == 1)


makeDependencyGraph :: Gr NLabel ELabel -> [EqTerm] -> Gr EqTerm ()
makeDependencyGraph g given = deq
  where ts = mkEdgeEq g ++ mkNodeEq g ++ given
        vsets = map mkVarSet ts
        mt = M.fromList (zip vsets ts)
        dg = dependencyGraph vsets
        deq = nmap (mt M.!) dg 
