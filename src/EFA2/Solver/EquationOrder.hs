

module EFA2.Solver.EquationOrder where

import qualified Data.List as L
import qualified Data.Set as S

import EFA2.Solver.Equation
import EFA2.Solver.IsVar

{-
data Plan = Given EqTerm
          | Resolve (S.Set EqTerm) Plan deriving (Show, Eq, Ord)

-}

data Derived = Derived (S.Set EqTerm) EqTerm deriving (Show, Ord)

instance Eq (Derived) where
    Derived as _ == Derived bs _ = as == bs

without :: (Ord a) => S.Set a -> a -> S.Set a
without as b = S.delete b as


isSingleton :: S.Set a -> Maybe a
isSingleton xs = if S.size xs == 1 then Just (head $ S.toList xs) else Nothing

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

isSingleVar :: Derived -> Bool
isSingleVar (Derived xs _ ) = S.size xs == 1

solve :: [Derived] -> [Derived]
solve eqns = filter isSingleVar (consequences eqns)




order :: [EqTerm] -> [EqTerm]
order ts = map g sol
  where vset t = mkVarSet isStaticVar t
        f t = Derived (mkVarSet isStaticVar t) t
        sol = solve (map f ts)
        g (Derived v eq) = transformEq (head $ S.toList v) eq
        