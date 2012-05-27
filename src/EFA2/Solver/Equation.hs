{-# LANGUAGE TypeSynonymInstances #-}


module EFA2.Solver.Equation where

import Control.Exception

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

import Debug.Trace


import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith

-- TOTHINK: Die Algorithmen aus dem Verzeichnis Solver sollten
-- über den Datentyp EqTerm parametrisierbar sein. Die Abhängigkeisanalyse
-- sollte nichts mit den konkreten Termen zu tun haben. Diese Entscheidung
-- haette wahrscheinlich auch Einfluss auf InVar...

data EqTerm = EqTerm := EqTerm
          | Const Val -- Double
          | Given
          | Energy EnergyIdx
          | DEnergy DEnergyIdx
          | Power PowerIdx
          | DPower DPowerIdx
          | FEta FEtaIdx
          | DEta DEtaIdx
          | DTime DTimeIdx
          | X XIdx
          | Var VarIdx
          | Store StorageIdx
 --         | FAbs { fAbsPower :: EqTerm, fAbsEta :: EqTerm }
 --         | BAbs { bAbsPower :: EqTerm, bAbsEta :: EqTerm }
          | FDiff { fDiffPower :: EqTerm, fDiffEta :: EqTerm, fDiffDPower :: EqTerm, fDiffDEta :: EqTerm }
          | BDiff { bDiffPower :: EqTerm, bDiffEta :: EqTerm, bDiffDPower :: EqTerm, bDiffDEta :: EqTerm }
          | Minus EqTerm
          | Recip EqTerm
          | EqTerm :+ EqTerm
          | EqTerm :* EqTerm deriving (Show, Eq, Ord)

infixl 1 !=, :=
infixl 7  !*, :*
infixl 6  !+, :+

(!+) :: (MkVarC a, MkVarC b) => a -> b -> EqTerm
x !+ y = mkVar x :+ mkVar y

(!*) :: (MkVarC a, MkVarC b) => a -> b -> EqTerm
x !* y = mkVar x :* mkVar y

(!=) :: (MkVarC a, MkVarC b) => a -> b -> EqTerm
x != y = mkVar x := mkVar y

class MkVarC a where
      mkVar :: a -> EqTerm
      give :: a -> EqTerm
      give idx = mkVar idx := Given

instance MkVarC EnergyIdx where
         mkVar = Energy

instance MkVarC DEnergyIdx where
         mkVar = DEnergy

instance MkVarC PowerIdx where
         mkVar = Power

instance MkVarC DPowerIdx where
         mkVar = DPower

--instance MkVarC EtaIdx where
--         mkVar = Eta

instance MkVarC FEtaIdx where
         --mkVar idx@(FEtaIdx s r f t) = FEta idx (mkVar (PowerIdx s r f t))
         mkVar = FEta

instance MkVarC DEtaIdx where
         mkVar = DEta

instance MkVarC DTimeIdx where
         mkVar = DTime

instance MkVarC XIdx where
         mkVar = X

instance MkVarC VarIdx where
         mkVar = Var

instance MkVarC StorageIdx where
         mkVar = Store

instance MkVarC Val where
         mkVar = Const

instance MkVarC EqTerm where
         mkVar = id

add :: [EqTerm] -> EqTerm
add ts = assert (length ts > 0) (L.foldl1' (:+) ts)

mult :: [EqTerm] -> EqTerm
mult = L.foldl1' (:*)

showEqTerm :: EqTerm -> String
showEqTerm (Const x) = show x
showEqTerm Given = "given"

showEqTerm (Energy (EnergyIdx s r x y)) = "E_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (DEnergy (DEnergyIdx s r x y)) = "dE_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y

showEqTerm (Power (PowerIdx s r x y)) = "P_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (DPower (DPowerIdx s r x y)) = "dP_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y

--showEqTerm (Eta (EtaIdx s r x y)) = "n_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (FEta (FEtaIdx s r x y)) = "n_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y -- ++ "(" ++ showEqTerm p ++ ")"
showEqTerm (DEta (DEtaIdx s r x y)) = "dn_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y

showEqTerm (DTime (DTimeIdx s r)) =  "dt_" ++ show s ++ "." ++ show r

showEqTerm (X (XIdx s r x y)) =  "x_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (Var (VarIdx s r x y)) = "v_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (Store (StorageIdx s r n)) = "s_" ++ show s ++ "." ++ show r ++ "_" ++ show n
showEqTerm (x :+ y) = "(" ++ showEqTerm x ++ " + " ++ showEqTerm y ++ ")"
showEqTerm (x :* y) = showEqTerm x ++ " * " ++ showEqTerm y
showEqTerm (FDiff p e dp de) = "f(" ++ showEqTerm p ++ ", " ++ showEqTerm e ++ ", " ++ showEqTerm dp ++ ", " ++ showEqTerm de ++")"
showEqTerm (BDiff p e dp de) = "b(" ++ showEqTerm p ++ ", " ++ showEqTerm e ++ ", " ++ showEqTerm dp ++ ", " ++ showEqTerm de  ++ ")"
showEqTerm (Recip x) = "1/(" ++ showEqTerm x ++ ")"
showEqTerm (Minus x) = "-(" ++ showEqTerm x ++ ")"
showEqTerm (x := y) = showEqTerm x ++ " = " ++ showEqTerm y

showEqTerms :: [EqTerm] -> String
showEqTerms ts = L.intercalate "\n" $ map showEqTerm ts


-- | This function takes a predicate p that determines, wether
-- a term is a variable or not. It then takes a term and
-- determines the set of variables contained in the term, 
-- according to the predicate.
mkVarSet :: (EqTerm -> Bool) -> EqTerm -> S.Set EqTerm
mkVarSet p t = mkVarSet' t
  where mkVarSet' v | p v = S.singleton v
        -- mkVarSet' fn@(FEta _) = S.insert fn (mkVarSet' p)
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
p1 = Energy (EnergyIdx 0 1)
p2 = Energy (EnergyIdx 0 2)
p3 = Energy (EnergyIdx 0 3)
p4 = Energy (EnergyIdx 0 4)

dp1 = DEnergy (DEnergyIdx 0 1)
dp2 = DEnergy (DEnergyIdx 0 2)
dp3 = DEnergy (DEnergyIdx 0 3)
dp4 = DEnergy (DEnergyIdx 0 4)


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
isolateVar s t p = error $ "isolateVar:\n" ++ show s ++ "\n" ++ show t ++ "\n" ++ show p

isolateVar' :: EqTerm -> TPath -> (EqTerm -> EqTerm)
isolateVar' _ [] = id
isolateVar' (u :+ v) (L:p) = isolateVar' u p . ((Minus v) :+)
isolateVar' (u :+ v) (R:p) = isolateVar' v p . ((Minus u) :+)

--isolateVar' (u :* (FEta (FEtaIdx s r f t))) (L:p) = isolateVar' u p . ((Recip (FEta (FEtaIdx s r t f))) :*)
--isolateVar' ((FEta (FEtaIdx s r f t)) :* v) (R:p) = isolateVar' v p . ((Recip (FEta (FEtaIdx s r t f))) :*)

isolateVar' (u :* v) (L:p) = isolateVar' u p . ((Recip v) :*)
isolateVar' (u :* v) (R:p) = isolateVar' v p . ((Recip u) :*)

isolateVar' (Minus u) (L:p) = isolateVar' u p . Minus
isolateVar' (Recip u) (L:p) = isolateVar' u p . Recip
--isolateVar' (FAbs u v) (L:p) = isolateVar' u p . (flip BAbs v)
--isolateVar' (BAbs u v) (L:p) = isolateVar' u p . (flip FAbs v)
isolateVar' (FDiff p' e dp de) (L:p) = isolateVar' dp p . f
  where f x@(DEnergy (DEnergyIdx s r a b)) = BDiff (Energy (EnergyIdx s r a b)) e x de
isolateVar' (BDiff p' e dp de) (L:p) = isolateVar' dp p . f
  where f x@(DEnergy (DEnergyIdx s r a b)) = FDiff (Energy (EnergyIdx s r a b)) e x de


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


