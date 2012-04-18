

module EFA2.Solver.DirEquation where

import qualified Data.Set as S
import qualified Data.List as L

import Debug.Trace

import EFA2.Solver.Equation
import EFA2.Solver.IsVar

import EFA2.Utils.Utils

-- This function takes equations already in order and
-- returns a list of variables conforming to this order that allows for calculation.
varsToCalculate :: (EqTerm -> Bool) -> [EqTerm] -> [EqTerm]
varsToCalculate isVar ts = dropGiven $ concatMap sdiff (pairs res)
  where dropGiven = drop (length $ filter isGiven ts)
        res = reverse $ L.foldl' (dirFoldFunc isVar) [S.empty] ts
        sdiff (a, b) = S.toList $ S.difference b a

dirFoldFunc :: (EqTerm -> Bool) -> [S.Set EqTerm] -> EqTerm -> [S.Set (EqTerm)]
dirFoldFunc isVar acc@(a:_) t = (S.union (mkVarSet isVar t) a):acc

filterEquations :: (EqTerm -> Bool) -> [EqTerm] -> [EqTerm] -> [EqTerm]
filterEquations isVar vars ts = res
  where vsets = tail $ reverse $ L.foldl' (dirFoldFunc isVar) [S.empty] ts
        notGiven = filter (not . isGiven . fst) (zip ts vsets)
        notGiven' = reverse $ L.foldl' g [] notGiven
        g [] eq = [eq]
        g acc@((_, s1):_) eq@(_, s2) | s1 == s2 = acc
        g acc eq = eq:acc
        res = map fst notGiven'

directEquations :: (EqTerm -> Bool) -> [EqTerm] -> [EqTerm]
directEquations isVar ts = g ++ res
  where (g, ng) = L.partition isGiven ts
        unknown = varsToCalculate isVar ts
        feqs = filterEquations isVar unknown ts
        res = map (uncurry transformEq) (zip unknown feqs)
