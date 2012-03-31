

module EFA2.Term.DirEquation where

import qualified Data.Set as S
import qualified Data.List as L

import Debug.Trace

import EFA2.Term.Equation

import EFA2.Utils.Utils

-- This function takes equations already in order and
-- returns a list of variables conforming to this order that allows for calculation.
varsToCalculate :: [EqTerm a] -> [EqTerm a]
varsToCalculate ts = dropGiven $ concatMap sdiff (pairs res)
  where dropGiven = drop (length $ filter isGiven ts)
        res = reverse $ L.foldl' dirFoldFunc [S.empty] ts
        sdiff (a, b) = S.toList $ S.difference b a

dirFoldFunc :: [S.Set (EqTerm a)] -> EqTerm a -> [S.Set (EqTerm a)]
dirFoldFunc acc@(a:_) t = (S.union (mkVarSet t) a):acc


filterEquations :: [EqTerm a] -> [EqTerm a] -> [EqTerm a]
filterEquations vars ts = res
  where vsets = tail $ reverse $ L.foldl' dirFoldFunc [S.empty] ts
        notGiven = filter (not . isGiven . fst) (zip ts vsets)
        notGiven' = reverse $ L.foldl' g [] notGiven
        g [] eq = [eq]
        g acc@((_, s1):_) eq@(_, s2) | s1 == s2 = acc
        g acc eq = eq:acc
        res = map fst notGiven'

directEquations :: [EqTerm a] -> [EqTerm a]
directEquations ts = g ++ res
  where (g, ng) = L.partition isGiven ts
        unknown = varsToCalculate ts
        feqs = filterEquations unknown ts
        res = map (uncurry transformEq) (zip unknown feqs)
