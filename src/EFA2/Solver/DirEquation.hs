
module EFA2.Solver.DirEquation where

import EFA2.Solver.Equation (EqTerm, Equation, mkVarSetEq, transformEq)
import EFA2.Solver.IsVar (isGiven)

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.List.HT as HTL

import EFA2.Utils.Utils (pairs)
import Data.Eq.HT (equating)


-- This function takes equations already in order and
-- returns a list of variables conforming to this order that allows for calculation.
varsToCalculate :: (EqTerm -> Bool) -> [Equation] -> [EqTerm]
varsToCalculate isVar ts = dropGiven $ concatMap sdiff (pairs res)
  where dropGiven = drop (length $ filter isGiven ts)
        res = L.scanl (dirFoldFunc isVar) S.empty ts
        sdiff (a, b) = S.toList $ S.difference b a

dirFoldFunc :: (EqTerm -> Bool) -> S.Set EqTerm -> Equation -> S.Set EqTerm
dirFoldFunc isVar a t = S.union (mkVarSetEq isVar t) a

filterEquations :: (EqTerm -> Bool) -> [EqTerm] -> [Equation] -> [Equation]
filterEquations isVar _vars ts = res
  where vsets = tail $ L.scanl (dirFoldFunc isVar) S.empty ts
        notGiven = filter (not . isGiven . fst) (zip ts vsets)
        res = map (fst . head) $ HTL.groupBy (equating snd) notGiven

directEquations :: (EqTerm -> Bool) -> [Equation] -> [Equation]
directEquations isVar ts = L.filter isGiven ts ++ res
  where unknown = varsToCalculate isVar ts
        feqs = filterEquations isVar unknown ts
        res = map (uncurry transformEq) (zip unknown feqs)
