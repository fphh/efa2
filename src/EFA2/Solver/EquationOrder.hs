

module EFA2.Solver.EquationOrder where

import qualified Data.List as L
import qualified Data.Set as S

import EFA2.Solver.Equation
import EFA2.Solver.IsVar

data Derived = Derived (S.Set EqTerm) EqTerm deriving (Show, Ord)

instance Eq (Derived) where
    Derived as _ == Derived bs _ = as == bs

without :: (Ord a) => S.Set a -> a -> S.Set a
without as b = S.delete b as


isSingleton :: S.Set a -> Maybe a
isSingleton xs = if S.size xs == 1 then Just (S.findMin xs) else Nothing

resolve :: Derived -> Derived -> [Derived]
resolve (Derived as aplan) (Derived bs bplan) | Just a <- isSingleton as = 
    if S.member a bs then  [Derived (bs `without` a) bplan] else []
resolve  x (Derived as ys) | Just a <- isSingleton as = resolve (Derived as ys) x
resolve _ _ = []


notin :: [Derived] -> Derived -> [Derived]
notin xs x = if x `elem` xs then [] else [x]

extend :: [Derived] -> [Derived] -> [Derived]
extend givens [] = givens
extend givens [eq] = extend (L.union givens [eq]) (givens >>= resolve eq >>= notin givens)
extend givens (eq : eqs) = extend (extend givens [eq]) eqs

consequences :: [Derived] -> [Derived]
consequences [] = []
consequences (x:xs) = extend (consequences xs) [x]

--consequences (x:y:xs) = extend (extend (consequences xs) [y]) [x]

{-
notin :: S.Set Derived -> Derived -> [Derived]
notin xs x = if S.member x xs then [] else [x]
-}
{-
extend :: S.Set Derived -> [Derived] -> S.Set Derived
extend givens [] = givens
extend givens [eq] = extend (S.insert eq givens) (S.toList givens >>= resolve eq >>= notin givens)
extend givens (eq : eqs) = extend (extend givens [eq]) eqs
-}
{-
extend :: S.Set Derived -> S.Set Derived -> S.Set Derived
extend givens eqs | S.null eqs = givens
extend givens eqs | Just eq <- isSingleton eqs = extend (S.insert eq givens) (f eq)
  where f eq = S.fromList (S.toList givens >>= resolve eq >>= notin givens)
extend givens eqs = extend (extend givens (S.singleton eq)) eqs'
  where (eq, eqs') = S.deleteFindMin eqs

consequences :: [Derived] -> S.Set Derived
consequences [] = S.empty
consequences (x:xs) = extend (consequences xs) (S.singleton x)
-}

isSingleVar :: Derived -> Bool
isSingleVar (Derived xs _ ) = S.size xs == 1

solve :: [Derived] -> [Derived]
solve eqns = filter isSingleVar (consequences eqns)

{-
solve2 :: S.Set EqTerm -> [Derived] -> [Derived]
solve2 vs eqns = snd $ L.foldr f (vs, []) (consequences eqns)
  where f d (s, l) | S.null s = (s, d:l)
        f d@(Derived v _) (s, l) | Just x <- isSingleton v = (S.delete x s, d:l)
        f _ ss = ss
-}


order :: [EqTerm] -> [EqTerm]
order ts = map g sol
  where vs = map (mkVarSet isStaticVar) ts
        sol = solve $ map (uncurry Derived) (zip vs ts)
        g (Derived v eq) = transformEq (head $ S.toList v) eq
        