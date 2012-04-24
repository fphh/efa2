{-# LANGUAGE TypeSynonymInstances #-}


module EFA2.Solver.Equation where

import Control.Exception

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

import Debug.Trace


import EFA2.Interpreter.Env

-- TOTHINK: Die Algorithmen aus dem Verzeichnis Solver sollten
-- über den Datentyp EqTerm parametrisierbar sein. Die Abhängigkeisanalyse
-- sollte nichts mit den konkreten Termen zu tun haben. Diese Entscheidung
-- haette wahrscheinlich auch Einfluss auf InVar...

data EqTerm = EqTerm := EqTerm
          | Const Double
          | Given
          | Power PowerIdx
          | Eta EtaIdx
          | DPower DPowerIdx
          | DEta DEtaIdx
          | X XIdx
          | Var VarIdx
 --         | FAbs { fAbsPower :: EqTerm, fAbsEta :: EqTerm }
 --         | BAbs { bAbsPower :: EqTerm, bAbsEta :: EqTerm }
          | FDiff { fDiffPower :: EqTerm, fDiffEta :: EqTerm, fDiffDPower :: EqTerm, fDiffDEta :: EqTerm }
          | BDiff { bDiffPower :: EqTerm, bDiffEta :: EqTerm, bDiffDPower :: EqTerm, bDiffDEta :: EqTerm }
          | Minus EqTerm
          | Recip EqTerm
          | EqTerm :+ EqTerm
          | EqTerm :* EqTerm deriving (Show, Eq, Ord)

infixl 1 :=

class MkVarC a where
      mkVar :: a -> EqTerm
      give :: a -> EqTerm
      give idx = mkVar idx := Given

instance MkVarC PowerIdx where
         mkVar = Power

instance MkVarC EtaIdx where
         mkVar = Eta

instance MkVarC DPowerIdx where
         mkVar = DPower

instance MkVarC DEtaIdx where
         mkVar = DEta

instance MkVarC XIdx where
         mkVar = X

instance MkVarC VarIdx where
         mkVar = Var

add :: [EqTerm] -> EqTerm
add ts = assert (length ts > 0) (L.foldl1' (:+) ts)

mult :: [EqTerm] -> EqTerm
mult = L.foldl1' (:*)

showEqTerm :: EqTerm -> String
showEqTerm (Const x) = show x
showEqTerm Given = "given"
showEqTerm (Power (PowerIdx s r x y)) = "E_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (Eta (EtaIdx s r x y)) = "n_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (DPower (DPowerIdx s r x y)) = "dE_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (DEta (DEtaIdx s r x y)) = "dn_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (X (XIdx s r x y)) =  "x_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (Var (VarIdx s r x y)) = "v_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (x :+ y) = "(" ++ showEqTerm x ++ " + " ++ showEqTerm y ++ ")"
showEqTerm (x :* y) = showEqTerm x ++ " * " ++ showEqTerm y
showEqTerm (FDiff p e dp de) = "f(" ++ showEqTerm p ++ ", " ++ showEqTerm e ++ ", " ++ showEqTerm dp ++ ", " ++ showEqTerm de ++")"
showEqTerm (BDiff p e dp de) = "b(" ++ showEqTerm p ++ ", " ++ showEqTerm e ++ ", " ++ showEqTerm dp ++ ", " ++ showEqTerm de  ++ ")"
showEqTerm (Recip x) = "1/(" ++ showEqTerm x ++ ")"
showEqTerm (Minus x) = "-(" ++ showEqTerm x ++ ")"
showEqTerm (x := y) = showEqTerm x ++ " = " ++ showEqTerm y

showEqTerms :: [EqTerm] -> String
showEqTerms ts = L.intercalate "\n" $ map showEqTerm ts

envToEqTerms :: (MkVarC k) => M.Map k v -> [EqTerm]
envToEqTerms m = map (give . fst) (M.toList m)


-- | This function takes a predicate p that determines, wether
-- a term is a variable or not. It then takes a term and
-- determines the set of variables contained in the term, 
-- according to the predicate.
mkVarSet :: (EqTerm -> Bool) -> EqTerm -> S.Set EqTerm
mkVarSet p t = mkVarSet' t
  where mkVarSet' v | p v = S.singleton v
        mkVarSet' (x :+ y) = S.union (mkVarSet' x) (mkVarSet' y)
        mkVarSet' (x :* y) = S.union (mkVarSet' x) (mkVarSet' y)
        --mkVarSet' (FAbs x y) = S.union (mkVarSet' x) (mkVarSet' y)
        --mkVarSet' (BAbs x y) = S.union (mkVarSet' x) (mkVarSet' y)
        mkVarSet' (FDiff p e dp de) = S.unions (map mkVarSet' [p, e, dp, de])
        mkVarSet' (BDiff p e dp de) = S.unions (map mkVarSet' [p, e, dp, de])
        mkVarSet' (Minus x) = mkVarSet' x
        mkVarSet' (Recip x) = mkVarSet' x
        mkVarSet' (x := y) = S.union (mkVarSet' x) (mkVarSet' y)
        mkVarSet' _ = S.empty


-- The following functions transform an equation.

data Dir = L | R deriving (Show, Eq)

type TPath = [Dir]


-- test terms
{-
p1 = Power (PowerIdx 0 1)
p2 = Power (PowerIdx 0 2)
p3 = Power (PowerIdx 0 3)
p4 = Power (PowerIdx 0 4)

dp1 = DPower (DPowerIdx 0 1)
dp2 = DPower (DPowerIdx 0 2)
dp3 = DPower (DPowerIdx 0 3)
dp4 = DPower (DPowerIdx 0 4)


c = Const 1.0

e = Eta (EtaIdx 0 1)
de = DEta (DEtaIdx 0 1)

t = dp2 := BDiff p1 e dp1 de
-}

findVar :: EqTerm -> EqTerm -> Maybe TPath
findVar t s | t == s = Just []
findVar t s
  | (Nothing, x) <- h = fmap (R:) x
  | (x, Nothing) <- h = fmap (L:) x
  | otherwise = error $ "error in looking for path to (" ++ show t ++ ") in (" ++ show s ++ ")"
  where h = help t s
        help t (u := v) = (findVar t u, findVar t v)
        help t (u :+ v) = (findVar t u, findVar t v)
        help t (u :* v) = (findVar t u, findVar t v)
        help t (Minus u) = (findVar t u, Nothing)    -- coding: Minus has only left operand.
        help t (Recip u) = (findVar t u, Nothing)    -- coding: Recip has only left operand.
        --help t u@(FAbs _ _) = (findVar t (fAbsPower u), Nothing)   -- etc.
        --help t u@(BAbs _ _) = (findVar t (bAbsPower u), Nothing)
        help t u@(FDiff _ _ _ _) = (findVar t (fDiffDPower u), Nothing)
        help t u@(BDiff _ _ _ _) = (findVar t (bDiffDPower u), Nothing)
        help _ _ = (Nothing, Nothing)

isolateVar :: EqTerm -> EqTerm -> TPath -> EqTerm
isolateVar s t@(u := v) (L:p) = (s := transform v)
  where transform = isolateVar' u p
isolateVar s t@(u := v) (R:p) = (s := transform u)
  where transform = isolateVar' v p

isolateVar' :: EqTerm -> TPath -> (EqTerm -> EqTerm)
isolateVar' _ [] = id
isolateVar' (u :+ v) (L:p) = isolateVar' u p . ((Minus v) :+)
isolateVar' (u :+ v) (R:p) = isolateVar' v p . ((Minus u) :+)
isolateVar' (u :* v) (L:p) = isolateVar' u p . ((Recip v) :*)
isolateVar' (u :* v) (R:p) = isolateVar' v p . ((Recip u) :*)
isolateVar' (Minus u) (L:p) = isolateVar' u p . Minus
isolateVar' (Recip u) (L:p) = isolateVar' u p . Recip
--isolateVar' (FAbs u v) (L:p) = isolateVar' u p . (flip BAbs v)
--isolateVar' (BAbs u v) (L:p) = isolateVar' u p . (flip FAbs v)
isolateVar' (FDiff p' e dp de) (L:p) = isolateVar' dp p . f
  where f x@(DPower (DPowerIdx s r a b)) = BDiff (Power (PowerIdx s r a b)) e x de
isolateVar' (BDiff p' e dp de) (L:p) = isolateVar' dp p . f
  where f x@(DPower (DPowerIdx s r a b)) = FDiff (Power (PowerIdx s r a b)) e x de


-- this is the main function for transforming Equations
-- It takes an unknown variable and an equation.
-- The resulting equation has
-- the unknown variable isolated on its left hand side (lhs),
-- such that we can evaluate the rhs in order to calculate
-- the value of the unknown variable.
transformEq :: EqTerm -> EqTerm -> EqTerm
transformEq unknown t
  | Nothing <- fv = t
  | Just p <- fv = isolateVar unknown t p
  where fv = findVar unknown t


