module EFA2.Solver.EquationOrder where

import qualified Data.List as L
import qualified Data.Set as S

import EFA2.Solver.Equation (Equation, Assign, mkVarSetEq, transformEq)
import EFA2.Solver.IsVar (maybeStaticVar)
import qualified EFA2.Interpreter.Env as Env

import Control.Monad (mplus, (<=<))
import Data.Maybe.HT (toMaybe)
import Data.Maybe (mapMaybe)


data Derived = Derived (S.Set Env.Index) Equation deriving (Show)

instance Eq (Derived) where
    Derived as _ == Derived bs _ = as == bs


isSingleton :: S.Set a -> Maybe a
isSingleton xs =
   case S.toList xs of
      [x] -> Just x
      _ -> Nothing

resolve :: Derived -> Derived -> Maybe Derived
resolve a@(Derived as _) b@(Derived bs _) =
   (isSingleton as >>= resolveTerm b)
   `mplus`
   (isSingleton bs >>= resolveTerm a)

resolveTerm :: Derived -> Env.Index -> Maybe Derived
resolveTerm (Derived bs bplan) a =
   toMaybe (S.member a bs) $ Derived (S.delete a bs) bplan


notin :: [Derived] -> Derived -> Maybe Derived
notin xs x = toMaybe (not $ elem x xs) x

extend :: [Derived] -> [Derived] -> [Derived]
extend = foldl extend'

extend' :: [Derived] -> Derived -> [Derived]
extend' givens eq =
   extend (L.union givens [eq]) $
   mapMaybe (notin givens <=< resolve eq) givens

consequences :: [Derived] -> [Derived]
consequences = foldr (flip extend') []

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


{-
solve2 :: S.Set EqTerm -> [Derived] -> [Derived]
solve2 vs eqns = snd $ L.foldr f (vs, []) (consequences eqns)
  where f d (s, l) | S.null s = (s, d:l)
        f d@(Derived v _) (s, l) | Just x <- isSingleton v = (S.delete x s, d:l)
        f _ ss = ss
-}

order :: [Equation] -> [Assign]
order =
   mapMaybe (\(Derived xs eq) -> fmap (flip transformEq eq) $ isSingleton xs) .
   consequences .
   map (\t -> Derived (mkVarSetEq maybeStaticVar t) t)
