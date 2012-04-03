{-# LANGUAGE FlexibleInstances #-}


module EFA2.Term.Horn where

import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import Data.Graph.Inductive

import Debug.Trace


import EFA2.Term.Equation
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

horn :: [Formula] -> Maybe (S.Set (Step, Formula))
horn fs = fmap atomsOnly res
  where atomsOnly = S.filter (isAtom . snd)
        res = horn' 0 S.empty fs

makeAnd :: [Formula] -> Formula
makeAnd fs = L.foldl1' And fs

graphToHorn :: Gr (EqTerm a) () -> [Formula]
graphToHorn g = foldGraph foldFunc [] g

{- TODO:
For an equation e that is not implied directly or indirectly by a set of equations ES 
(e.g. there is no path in the dependency graph from ES to e) , we should
look for a variable coverage for this equation E in the set of equations ES.
The horn formula is then, for example: a and b and c -> e if the variables
from a, b, c (a subset of ES) allow to compute e.

That would be cool!
-}
foldFunc :: [Formula] -> ([Node], Node, [Node]) -> [Formula]
foldFunc acc ([], _, []) = acc
{-
foldFunc acc ([], x, outs) = map (\lits -> makeAnd lits :-> v) fs ++ acc
  where v = Atom x
        lits = map Atom outs
        fs = filter f $ unique $ (map L.sort) $ sequence [lits, lits]
        f [x, y] = x /= y
-}
foldFunc acc (ins, x, _) = insFs ++ acc
  where v = Atom x
        insFs = map (:-> v) (map Atom ins)


makeHornFormulae :: Gr (EqTerm a) () -> [EqTerm a] -> [Formula]
makeHornFormulae g given = given' ++ graphToHorn g
  where given' = L.foldl' f [] (labNodes g)
        f acc (n, Given (Energy _)) = (One :-> Atom n):acc
        f acc _ = acc

makeHornOrder :: Gr (EqTerm a) () -> [Formula] -> [EqTerm a]
makeHornOrder g formulae = catMaybes ts
  where Just fs = horn formulae
        fs' :: [Formula]
        fs' = map snd (S.toList fs)
        ts = map (lab g . fromAtom) fs'
        fromAtom (Atom x) = x

hornOrder :: Gr (EqTerm a) () -> [EqTerm a] -> [EqTerm a]
hornOrder g given = makeHornOrder g given'
  where given' = makeHornFormulae g given