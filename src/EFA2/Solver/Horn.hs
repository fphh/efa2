{-# LANGUAGE FlexibleInstances #-}


module EFA2.Solver.Horn where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Graph.Inductive

import Debug.Trace

import EFA2.Solver.DependencyGraph
import EFA2.Solver.Equation
import EFA2.Utils.Utils

data Formula = Zero
             | One
             | Atom Int
             | And Formula Formula
             | Formula :-> Formula deriving (Ord, Eq)

infix 8 :->

instance Show Formula where
         show Zero = "F"
         show One = "T"
         show (Atom x) = show x
         show (And f g) = "(" ++ show f ++ " ∧ " ++ show g ++ ")"
         show (f :-> g) = show f ++ " → " ++ show g

type Step = Int

hornsToStr :: [Formula] -> String
hornsToStr fs = L.intercalate " ∧ " $ map (("(" ++) . (++ ")") . show) fs


isAtom :: Formula -> Bool
isAtom (Atom _) = True
isAtom _ = False

fromAtom :: Formula -> Int
fromAtom (Atom x) = x
fromAtom t = error ("Wrong term " ++ show t ++ " supplied to fromAtom.")

getAtoms :: Formula -> S.Set Formula
getAtoms v@(Atom _) = S.singleton v
getAtoms (And f g) = S.union (getAtoms f) (getAtoms g)
getAtoms (f :-> g) = S.union (getAtoms f) (getAtoms g)
getAtoms _ = S.empty

leftMarked :: S.Set Formula -> Formula -> Bool
leftMarked _ (One :-> _) = True
leftMarked vs (lhs :-> _) | S.size (S.difference (getAtoms lhs) vs) == 0 = True
leftMarked _ _ = False

rightMarked :: S.Set Formula -> Formula -> Bool
rightMarked vs (_ :-> v) | S.member v vs = True
rightMarked _ _ = False

makeAnd :: [Formula] -> Formula
makeAnd = L.foldl1' And

step :: Step -> S.Set (Step, Formula) -> [Formula] -> (S.Set (Step, Formula), [Formula])
step i vs fs = (unionVs, filter (not . rightMarked onlyVars') bs)
  where (as, bs) = L.partition (leftMarked onlyVars) fs
        vs' = S.fromList $ zip (repeat i) (map (\(_ :-> v) -> v) as)
        unionVs = S.union vs' vs
        onlyVars = S.map snd vs
        onlyVars' = S.map snd unionVs

horn' :: Step -> S.Set (Step, Formula) -> [Formula] -> Maybe (S.Set (Step, Formula))
horn' i vs fs
  | noZero = if (vs == vs') then Just vs else horn' (i+1) vs' fs'
  | otherwise = Nothing             
  where (vs', fs') = step i vs fs
        noZero = all ((Zero /=) . snd) (S.toList vs)

-- | Returns a set of 'Atom's that are have to be marked True in order to fulfill the 'Formula'e.
--   To each 'Atom' is associated the 'Step' in which it was marked.
horn :: [Formula] -> Maybe (S.Set (Step, Formula))
horn fs = fmap atomsOnly res
  where atomsOnly = S.filter (isAtom . snd)
        res = horn' 0 S.empty fs

-- | Takes a dependency graph and returns Horn clauses from it, that is, every directed edge
--   is taken for an implication.
graphToHorn :: Gr EqTerm () -> [Formula]
graphToHorn g = foldGraph f [] g
  where f acc ([], _, []) = acc
        f acc (ins, x, _) = (map (:-> Atom x) (map Atom ins)) ++ acc

-- | Takes a dependency graph and returns Horn clauses from it. /Given/ 'Formula'e will
--   produce additional clauses of the form One :-> Atom x. 
--   These are the starting clauses for the Horn marking algorithm.
makeHornFormulae :: Gr EqTerm () -> [Formula]
makeHornFormulae g = given ++ graphToHorn g
  where given = L.foldl' f [] (labNodes g)
        f acc (n, t) | isGiven t = (One :-> Atom n):acc
        f acc _ = acc

-- | Takes a dependency graph and a list of 'Formula'e. With help of the horn marking algorithm
--   it produces a list of 'EqTerm' equations that is ordered such, that it can be computed
--   one by one. 
makeHornOrder :: M.Map Node EqTerm -> [Formula] -> [EqTerm]
makeHornOrder m formulae = map ((m M.!) . fromAtom) fs'
  where Just fs = horn formulae
        fs' = map snd (S.toAscList fs)

makeHornClauses :: [EqTerm] -> (M.Map Node EqTerm, [Formula])
makeHornClauses ts = (m, fsdpg1) --   ++ fsdpg2)
  where m = M.fromList (labNodes dpg1)
        dpg1 = dpgDiffByAtMostOne ts
        --dpg2 = dpgHasSameVariable ts
        --dpg3 = L.foldl' (flip delEdge) dpg2 (edges dpg1)

        fsdpg1 = makeHornFormulae dpg1
        --fsdpg2 = concat $ mapGraph g dpg3
{-
        mset = M.map (mkVarSet isVar) m
        g (_, n, _) | isGiven (m M.! n) = []
        g ([], _, _) = []
        g (ins, n, _) = map f sc
          where sc = setCoverBruteForce mset n ins
                f xs = makeAnd (map Atom xs) :-> Atom n
-}

hornOrder :: [EqTerm] -> [EqTerm]
hornOrder = uncurry makeHornOrder . makeHornClauses


allNotEmptyCombinations :: (Ord a) => [a] -> [[a]]
allNotEmptyCombinations xs = filter (not .null) $ map (map fst) zs
  where len = length xs
        bits = sequence (replicate len [False, True])
        ys = map (zip xs) bits
        zs = map (filter ((True ==) . snd)) ys


setCoverBruteForce :: M.Map Node (S.Set EqTerm) -> Node -> [Node] -> [[Node]]
--setCoverBruteForce _ _ ns | length ns > 6 = []
setCoverBruteForce m n ns = map fst $ filter p xs
  where s = m M.! n
        combs = allNotEmptyCombinations ns
        xs = zip combs (map f combs)
        f ys = S.unions $ map (m M.!) ys
        p (c, t) = S.size (s S.\\ t) < 2
