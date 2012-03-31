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

dependencyGraph :: [S.Set (EqTerm a)] -> Gr (S.Set (EqTerm a)) ()
dependencyGraph ss = g
  where xs = HTL.removeEach ss
        ys = concatMap (uncurry mkArcs) xs
        m = M.fromList (zip ss [0..])
        es = unique $ map (\(x, y) -> (m M.! x, m M.! y, ())) ys
       -- g :: Gr (S.Set a) ()
        g = mkGraph (map flipPair $ M.toList m) es

mkArcs :: (Ord a) => S.Set a -> [S.Set a] -> [(S.Set a, S.Set a)]
mkArcs s ss = catMaybes $ map g ss
  where g t | s `diffByAtMostOne` t = Just (s, t)
        g _ = Nothing

-- If true, then we have an edge from s to t.
diffByAtMostOne :: (Eq a, Ord a) => S.Set a -> S.Set a -> Bool
diffByAtMostOne s t = (S.size t > 1) && (S.isSubsetOf t s || S.size (S.difference t s) == 1)



--makeDependencyGraph :: Gr NLabel ELabel -> [EqTerm Abs] -> Gr NLabel ()

makeDependencyGraph :: Gr NLabel ELabel -> [EqTerm a] -> Gr (EqTerm a) ()
makeDependencyGraph g given = deq
  where --ts :: [EqTerm a]
        ts = mkEdgeEq g ++ mkNodeEq g ++ given
        --vsets :: [S.Set (EqTerm a)]
        vsets = map mkVarSet ts
        --mt :: M.Map (S.Set (EqTerm a)) (EqTerm a)
        mt = M.fromList (zip vsets ts)
        --dg :: Gr (S.Set (EqTerm a)) ()
        dg = dependencyGraph vsets
        --deq :: Gr (EqTerm a) ()
        deq = nmap (mt M.!) dg
        --deq = nmap (const ()) dg