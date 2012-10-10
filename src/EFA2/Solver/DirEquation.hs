
module EFA2.Solver.DirEquation where

import EFA2.Solver.Equation
          (EqTerm, Equation(Given, (:=)), Assign(GivenIdx),
           mkVarSetEq, transformEq)
import EFA2.Solver.IsVar (isGiven)
import qualified EFA2.Interpreter.Env as Env

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.NonEmpty as NonEmpty
import qualified Data.NonEmpty.Mixed as NonEmptyM

import EFA2.Utils.Utils (pairs)
import Data.Maybe (mapMaybe)
import Data.Eq.HT (equating)


-- This function takes equations already in order and
-- returns a list of variables conforming to this order that allows for calculation.
varsToCalculate ::
   (Ord a) =>
   (EqTerm -> Maybe a) -> [Equation] -> [a]
varsToCalculate isVar ts = dropGiven $ concatMap sdiff (pairs res)
  where dropGiven = drop (length $ filter isGiven ts)
        res = L.scanl (dirFoldFunc isVar) S.empty ts
        sdiff (a, b) = S.toList $ S.difference b a

dirFoldFunc ::
   (Ord a) =>
   (EqTerm -> Maybe a) -> S.Set a -> Equation -> S.Set a
dirFoldFunc isVar a t = S.union (mkVarSetEq isVar t) a

filterEquations ::
   (Ord a) =>
   (EqTerm -> Maybe a) -> [a] -> [Equation] -> [Equation]
filterEquations isVar _vars ts = res
  where vsets = tail $ L.scanl (dirFoldFunc isVar) S.empty ts
        notGiven = filter (not . isGiven . fst) (zip ts vsets)
        res =
           map (fst . NonEmpty.head) $
           NonEmptyM.groupBy (equating snd) notGiven

directEquations ::
   (EqTerm -> Maybe Env.Index) -> [Equation] -> [Assign]
directEquations isVar ts = mapMaybe givenToAssign ts ++ res
  where unknown = varsToCalculate isVar ts
        feqs = filterEquations isVar unknown ts
        res = map (uncurry transformEq) (zip unknown feqs)
        givenToAssign equ =
           case equ of
              Given v -> Just $ GivenIdx v
              _ := _ -> Nothing
