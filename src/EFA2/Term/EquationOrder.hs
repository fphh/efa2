{-# LANGUAGE ScopedTypeVariables #-}


module EFA2.Term.EquationOrder where

import Data.Graph.Inductive
import Data.Maybe

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L

import Debug.Trace

import EFA2.Term.Term
import EFA2.Utils.Utils
import EFA2.Graph.Graph

dependencyGraph :: forall a. (Ord a) => [S.Set a] -> Gr (S.Set a) ()
dependencyGraph ss = g
  where xs = eachWithEvery ss
        ys = concatMap (uncurry mkArcs) xs
        m = M.fromList (zip ss [0..])
        es = unique $ map (\(x, y) -> (m M.! x, m M.! y, ())) ys
        g :: Gr (S.Set a) ()
        g = mkGraph (map flipPair $ M.toList m) es

mkArcs :: (Ord a) => S.Set a -> [S.Set a] -> [(S.Set a, S.Set a)]
mkArcs s ss = catMaybes $ map g ss
  where g t | s `diffByOne` t = Just (s, t)
        g _ = Nothing

makeOrder :: [[Node]] -> [Node]
makeOrder xs = concat res
  where f (s, acc) x = (S.union s (S.fromList x), (gunique s x):acc)
        (_, res) = L.foldl' f (S.empty, []) xs

data Equations = Equations [Term] (M.Map Term (S.Set Term)) deriving (Show)


orderEquations :: Gr NLabel ELabel -> Term -> Equations
orderEquations g target = Equations (reverse eqs) (reverseMap mt)
  where ts = mkEdgeEq g ++ mkNodeEq g
        vsets = map mkVarSet ts
        mt = M.fromList (zip vsets ts)
        dg = dependencyGraph (map mkVarSet ts)
        dgw = emap sameValue (undir dg)
        targetNode = fst $ head $ filter (S.member target . snd) (labNodes dgw)
        breadthFirst = bfs targetNode dgw
        deq = nmap (mt M.!) dg
        eqs = catMaybes $ map (lab deq) breadthFirst


directEquations :: S.Set Term -> Equations -> [Term]
directEquations s (Equations ts m) = snd $ L.foldl' (f m) (s, []) ts 

f m (set, acc) eq
  | [v] <- vdiff = (set', (transEq v):acc)
  | [] <- vdiff = (set, eq:acc)
  | otherwise = error $ "to many unknown variables in " ++ show eq ++ " (known: " ++ show set ++ ")"
  where vs = m M.! eq
        vdiff = S.toList (S.difference vs set)
        transEq v = transformEq v eq
        set' = S.union set vs

makeEquations :: Gr NLabel ELabel -> S.Set Term -> Term -> [Term]
makeEquations g given target = reverse dirEqs
  where ordEqs@(Equations ts _) = orderEquations g target
        dirEqs = directEquations given ordEqs
