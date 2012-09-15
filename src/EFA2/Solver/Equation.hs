{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}
{-# LANGUAGE PatternGuards #-}


module EFA2.Solver.Equation where

import Control.Exception

import Data.Maybe

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

import Debug.Trace
import Text.Printf


import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith
import EFA2.Utils.Utils

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
          | DX DXIdx
          | Var VarIdx
          | Store StorageIdx
          | FEdge EqTerm EqTerm -- power, eta
          | BEdge EqTerm EqTerm
          | NEdge EqTerm EqTerm

          | FNode EqTerm EqTerm -- node equations
          | BNode EqTerm EqTerm
          | XNode EqTerm EqTerm

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

instance MkVarC FEtaIdx where
         mkVar = FEta

instance MkVarC DEtaIdx where
         mkVar = DEta

instance MkVarC DTimeIdx where
         mkVar = DTime

instance MkVarC XIdx where
         mkVar = X

instance MkVarC DXIdx where
         mkVar = DX

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

showEqTerm (FEta (FEtaIdx s r x y)) = "n_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (DEta (DEtaIdx s r x y)) = "dn_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y

showEqTerm (DTime (DTimeIdx s r)) =  "dt_" ++ show s ++ "." ++ show r

showEqTerm (X (XIdx s r x y)) =  "x_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (DX (DXIdx s r x y)) =  "dx_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y

showEqTerm (Var (VarIdx s r x y)) = "v_" ++ show s ++ "." ++ show r ++ "_" ++ show x ++ "." ++ show y
showEqTerm (Store (StorageIdx s r n)) = "s_" ++ show s ++ "." ++ show r ++ "_" ++ show n

showEqTerm (x :+ y) = "(" ++ showEqTerm x ++ " + " ++ showEqTerm y ++ ")"
showEqTerm (x :* y) = showEqTerm x ++ " * " ++ showEqTerm y
showEqTerm (FEdge power eta) = "f(" ++ showEqTerm power ++ ", " ++ showEqTerm eta ++ ")"
showEqTerm (BEdge power eta) = "b(" ++ showEqTerm power ++ ", " ++ showEqTerm eta ++ ")"
showEqTerm (NEdge power0 power1) = "n(" ++ showEqTerm power0 ++ ", " ++ showEqTerm power1 ++ ")"

showEqTerm (FNode power eta) = "fn(" ++ showEqTerm power ++ ", " ++ showEqTerm eta ++ ")"
showEqTerm (BNode power eta) = "bn(" ++ showEqTerm power ++ ", " ++ showEqTerm eta ++ ")"
showEqTerm (XNode power0 power1) = "xn(" ++ showEqTerm power0 ++ ", " ++ showEqTerm power1 ++ ")"

showEqTerm (Recip x) = "1/(" ++ showEqTerm x ++ ")"
showEqTerm (Minus x) = "-(" ++ showEqTerm x ++ ")"
showEqTerm (x := y) = showEqTerm x ++ " = " ++ showEqTerm y

showEqTerms :: [EqTerm] -> String
showEqTerms ts = L.intercalate "\n" $ map showEqTerm ts


newtype LatexString = LatexString { unLatexString :: String } deriving (Show, Eq)

toLatexString' :: EqTerm -> String
toLatexString' (Const x) = printf "%.6f   " x
toLatexString' Given = "\\mbox{given}"

toLatexString' (Energy (EnergyIdx s r x y)) = "E_{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"
toLatexString' (DEnergy (DEnergyIdx s r x y)) = "\\Delta E_{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"

toLatexString' (Power (PowerIdx s r x y)) = "P_{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"
toLatexString' (DPower (DPowerIdx s r x y)) = "\\Delta P_{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"

toLatexString' (FEta (FEtaIdx s r x y)) = "\\eta_{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"
toLatexString' (DEta (DEtaIdx s r x y)) = "\\Delta \\eta_{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"

toLatexString' (DTime (DTimeIdx s r)) =  "\\Delta t_{" ++ show s ++ "." ++ show r ++ "}"

toLatexString' (X (XIdx s r x y)) =  "x_{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"
toLatexString' (DX (DXIdx s r x y)) =  "\\Delta x_{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"

toLatexString' (Var (VarIdx s r x y)) = "v_{" ++ show s ++ "." ++ show r ++ "." ++ show x ++ "." ++ show y ++ "}"
toLatexString' (Store (StorageIdx s r n)) = "s_{" ++ show s ++ "." ++ show r ++ "." ++ show n ++ "}"


toLatexString' (x :+ y) = "(" ++ toLatexString' x ++ " + " ++ toLatexString' y ++ ")"
toLatexString' (x :* y) = toLatexString' x ++ " * " ++ toLatexString' y
toLatexString' (FEdge power eta) = "f(" ++ toLatexString' power ++ ", " ++ toLatexString' eta ++ ")"
toLatexString' (BEdge power eta) = "b(" ++ toLatexString' power ++ ", " ++ toLatexString' eta ++ ")"
toLatexString' (NEdge power0 power1) = "n(" ++ toLatexString' power0 ++ ", " ++ toLatexString' power1 ++ ")"

toLatexString' (FNode power eta) = "fn(" ++ toLatexString' power ++ ", " ++ toLatexString' eta ++ ")"
toLatexString' (BNode power eta) = "bn(" ++ toLatexString' power ++ ", " ++ toLatexString' eta ++ ")"
toLatexString' (XNode power0 power1) = "xn(" ++ toLatexString' power0 ++ ", " ++ toLatexString' power1 ++ ")"

toLatexString' (Recip x) = "\\frac{1}{" ++ toLatexString' x ++ "}"
toLatexString' (Minus x) = "-(" ++ toLatexString' x ++ ")"
toLatexString' (x := y) = toLatexString' x ++ " = " ++ toLatexString' y

toLatexString :: EqTerm -> LatexString
toLatexString t = LatexString $ "$" ++ toLatexString' t ++ "$"


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
        mkVarSet' (FEdge x y) = S.union (mkVarSet' x) (mkVarSet' y)
        mkVarSet' (BEdge x y) = S.union (mkVarSet' x) (mkVarSet' y)
        mkVarSet' (NEdge x y) = S.union (mkVarSet' x) (mkVarSet' y)

        mkVarSet' (FNode x y) = S.union (mkVarSet' x) (mkVarSet' y)
        mkVarSet' (BNode x y) = S.union (mkVarSet' x) (mkVarSet' y)
        mkVarSet' (XNode x y) = S.union (mkVarSet' x) (mkVarSet' y)

        mkVarSet' (Minus x) = mkVarSet' x
        mkVarSet' (Recip x) = mkVarSet' x
        mkVarSet' (x := y) = S.union (mkVarSet' x) (mkVarSet' y)
        mkVarSet' _ = S.empty


-- The following functions transform an equation.

data Dir = L | R deriving (Show, Eq)

type TPath = [Dir]


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
        help t (FEdge power eta) = (findVar t power, findVar t eta)
        help t (BEdge power eta) = (findVar t power, findVar t eta)
        help t (NEdge power eta) = (findVar t power, findVar t eta)

        help t (FNode power eta) = (findVar t power, findVar t eta)
        help t (BNode power eta) = (findVar t power, findVar t eta)
        help t (XNode power eta) = (findVar t power, findVar t eta)

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

isolateVar' (u :* v) (L:p) = isolateVar' u p . ((Recip v) :*)
isolateVar' (u :* v) (R:p) = isolateVar' v p . ((Recip u) :*)

isolateVar' (Minus u) (L:p) = isolateVar' u p . Minus
isolateVar' (Recip u) (L:p) = isolateVar' u p . Recip

-- e = u * v
isolateVar' (FEdge u v) (L:p) = isolateVar' u p . (flip BEdge v)
isolateVar' (FEdge u v) (R:p) = isolateVar' v p . (flip NEdge u)

-- e = u / v
isolateVar' (BEdge u v) (L:p) = isolateVar' u p . (flip FEdge v)
isolateVar' (BEdge u v) (R:p) = isolateVar' v p . (flip NEdge u)

-- n = u/v
isolateVar' (NEdge u v) (L:p) = isolateVar' u p . (flip FEdge v)
isolateVar' (NEdge u v) (R:p) = isolateVar' v p . (flip BEdge u)

{-
-- e = u * v
isolateVar' (FNode u v) (L:p) = isolateVar' u p . (flip BNode v)
isolateVar' (FNode u v) (R:p) = isolateVar' v p . (flip XNode u)

-- e = u / v
isolateVar' (BNode u v) (L:p) = isolateVar' u p . (flip FNode v)
isolateVar' (BNode u v) (R:p) = isolateVar' v p . (flip XNode u)

-- n = u/v
isolateVar' (XNode u v) (L:p) = isolateVar' u p . (flip FNode v)
isolateVar' (XNode u v) (R:p) = isolateVar' v p . (flip BNode u)
-}



{-
isolateVar' (FDiff p' e dp de) (L:p) = isolateVar' dp p . f
  where f x@(DEnergy (DEnergyIdx s r a b)) = BDiff (Energy (EnergyIdx s r a b)) e x de
isolateVar' (BDiff p' e dp de) (L:p) = isolateVar' dp p . f
  where f x@(DEnergy (DEnergyIdx s r a b)) = FDiff (Energy (EnergyIdx s r a b)) e x de
-}

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

--------------------------------------------------------------------


pushMult :: EqTerm -> EqTerm
pushMult t 
  | (u := v) <- t = add (pushMult' u) := add (pushMult' v)
  | otherwise = add (pushMult' t)
  where pushMult' :: EqTerm -> [EqTerm]
        pushMult' (Minus u) = map Minus (pushMult' u)
        pushMult' (Recip u) = [Recip (L.foldl1' (:+) $ pushMult' u)]
        pushMult' (u :+ v) = pushMult' u ++ pushMult' v
        pushMult' (u :* v) = map mult (sequence [pushMult' u, pushMult' v])
        pushMult' t = [t]

simplify :: EqTerm -> EqTerm
simplify = fst . head . dropWhile pred . pairs . iterate simplify' . pushMult
  where pred (s, t) = s /= t
        simplify' :: EqTerm -> EqTerm
        simplify' ((Const 0.0) :+ x) = simplify' x
        simplify' (x :+ (Const 0.0)) = simplify' x
        simplify' ((Const 1.0) :* x) = simplify' x
        simplify' (x :* (Const 1.0)) = simplify' x
        simplify' ((Const 0.0) :* _) = Const 0.0
        simplify' (x :* (Const 0.0)) = Const 0.0

        simplify' (Recip (Const 1.0)) = Const 1.0
        simplify' (x :* (Recip y)) | x == y = Const 1.0
        simplify' ((Minus x) :* (Recip y)) | x == y = Const (-1.0)
        simplify' ((Recip x) :* y) | x == y = Const 1.0
        simplify' ((Recip x) :* (Minus y)) | x == y = Const (-1.0)

        simplify' (Recip (Recip x)) = simplify' x 
        simplify' (Recip x) = Recip (simplify' x)

        simplify' (Minus (Const 0.0)) = Const 0.0
        simplify' (Minus (Minus x)) = simplify' x
        simplify' (Minus x) = Minus (simplify' x)
        simplify' ((Minus x) :* (Minus y)) = simplify' x :* simplify' y
        simplify' (x :+ y) = simplify' x :+ simplify' y
        simplify' (x :* y) = simplify' x :* simplify' y
        simplify' (x := y) = simplify' x := simplify' y
        simplify' x = x

additiveTerms :: EqTerm -> [EqTerm]
additiveTerms = additiveTerms' . simplify . pushMult 
  where additiveTerms' (x :+ y) = additiveTerms' x ++ additiveTerms' y
        additiveTerms' t = [t]


setEqTerms :: Envs EqTerm -> EqTerm -> EqTerm
setEqTerms envs (Power idx) | Just t <- M.lookup idx (powerMap envs) = t
setEqTerms envs (DPower idx) | Just t <- M.lookup idx (dpowerMap envs) = t
setEqTerms envs (Energy idx) | Just t <- M.lookup idx (energyMap envs) = t
setEqTerms envs (DEnergy idx) | Just t <- M.lookup idx (denergyMap envs) = t
setEqTerms envs (FEta idx) | Just t <- M.lookup idx (fetaMap envs) = trace "setEqTerms" $ t undefined
setEqTerms envs (DEta idx) | Just t <- M.lookup idx (detaMap envs) = trace "setEqTerms" $ t undefined
setEqTerms envs (X idx) | Just t <- M.lookup idx (xMap envs) = t
setEqTerms envs (DX idx) | Just t <- M.lookup idx (dxMap envs) = t
setEqTerms envs (Var idx) | Just t <- M.lookup idx (varMap envs) = t
setEqTerms envs (Store idx) | Just t <- M.lookup idx (storageMap envs) = t
setEqTerms envs (DTime idx) | Just t <- M.lookup idx (dtimeMap envs) = t
setEqTerms envs (Minus t) = Minus (setEqTerms envs t)
setEqTerms envs (Recip t) = Recip (setEqTerms envs t)
setEqTerms envs (s :+ t) = setEqTerms envs s :+ setEqTerms envs t
setEqTerms envs (s :* t) = setEqTerms envs s :* setEqTerms envs t
setEqTerms envs (s := t) = setEqTerms envs s := setEqTerms envs t
setEqTerms _ t = t
--------------------------------------------------------------------

toAbsEqTerm :: EqTerm -> EqTerm
toAbsEqTerm (FEdge p n) = p :* n
toAbsEqTerm (BEdge p n) = p :* (Recip n)
toAbsEqTerm (NEdge p0 p1) = p0 :* (Recip p1)

toAbsEqTerm (FNode p n) = p :* n
toAbsEqTerm (BNode p n) = p :* (Recip n)
toAbsEqTerm (XNode p0 p1) = p0 :* (Recip p1)

toAbsEqTerm (Minus x) = Minus (toAbsEqTerm x)
toAbsEqTerm (Recip x) = Recip (toAbsEqTerm x)
toAbsEqTerm (x :+ y) = toAbsEqTerm x :+ toAbsEqTerm y
toAbsEqTerm (x :* y) = toAbsEqTerm x :* toAbsEqTerm y
toAbsEqTerm (x := y) = toAbsEqTerm x := toAbsEqTerm y
toAbsEqTerm t = t

toAbsEqTermEquations :: [EqTerm] -> [EqTerm]
toAbsEqTermEquations ts = map toAbsEqTerm ts
 

mkDiffEqTerm :: Int -> EqTerm -> Maybe [EqTerm]

-- v_0.1_OutSum.0 = P_0.1_0.1
mkDiffEqTerm _ z@(Var (VarIdx s r use n) := Power (PowerIdx _ _ f t)) = 
  {- trace ("--->: " ++ showEqTerm z ++ " s=> " ++ showEqTerm eres) $ -} Just [res, eres]
  where res = v := dp
        v = mkVar $ VarIdx s r (toDiffUse use) n
        dp = mkVar $ DPowerIdx s r f t

        eres = de := dp :* dt
        dt = mkVar $ DTimeIdx s r
        de = mkVar $ DEnergyIdx s r f t



-- v_0.1_OutSum.1 = (P_0.1_1.2 + P_0.1_1.3) ...
mkDiffEqTerm _ z@(Var (VarIdx s r use n) := as@(x :+ y)) =
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm res) $ -} Just [res]
  where res = v := dps
        ats = additiveTerms as      
        v = mkVar $ VarIdx s r (toDiffUse use) n
        dps = add $ map f ats
        f (Power (PowerIdx _ _ f t)) = mkVar $ DPowerIdx s r f t

-- P_0.1_0.1 = v_0.1_OutSum.0
mkDiffEqTerm _ z@(Power (PowerIdx _ _ f t) := Var (VarIdx s r use n)) = 
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm eres) $ -} Just [res, eres]
  where res = dp := v
        v = mkVar $ VarIdx s r (toDiffUse use) n
        dp = mkVar $ DPowerIdx s r f t

        eres = de := dp :* dt
        dt = mkVar $ DTimeIdx s r
        de = mkVar $ DEnergyIdx s r f t


-- v_0.1_OutSum.1 = v_0.1_InSum.1
mkDiffEqTerm _ z@(Var (VarIdx s r use0 n) := Var (VarIdx _ _ use1 _)) = 
  {- trace (showEqTerm z ++ " => " ++ showEqTerm res) $ -} Just [res]
  where res = v0 := v1
        v0 = mkVar $ VarIdx s r (toDiffUse use0) n
        v1 = mkVar $ VarIdx s r (toDiffUse use1) n

-- P_0.1_1.0 = f(P_0.1_0.1, n_0.1_0.1)
mkDiffEqTerm oldrec z@(Power (PowerIdx s newrec f t) := FEdge (Power _) _) = 
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm eres) $ -} Just [res, eres]
  where res = dq := (dp :* n) :+ (p :* dn) :+ (dp :* dn)
        dq = mkVar $ DPowerIdx s newrec f t
        dp = mkVar $ DPowerIdx s newrec t f
        n = mkVar $ FEtaIdx s oldrec t f
        dn = mkVar $ DEtaIdx s newrec t f
        p = mkVar $ PowerIdx s oldrec t f

        eres = de := dq :* dt
        dt = mkVar $ DTimeIdx s newrec
        de = mkVar $ DEnergyIdx s newrec f t

-- P_0.1_1.2 = f(v_0.1_OutSum.1, x_0.1_1.2)
mkDiffEqTerm oldrec z@(Power (PowerIdx s newrec f t) := FEdge (Var (VarIdx _ _ use _)) _) = 
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm eres) $ -} Just [res, eres]
  where res = dq := (dv :* x) :+ (v :* dx) :+ (dv :* dx)
        dq = mkVar $ DPowerIdx s newrec f t
        dv = mkVar $ VarIdx s newrec (toDiffUse use) f
        x = mkVar $ XIdx s oldrec f t
        dx = mkVar $ DXIdx s newrec f t
        v = mkVar $ VarIdx s oldrec use f

        eres = de := dq :* dt
        dt = mkVar $ DTimeIdx s newrec
        de = mkVar $ DEnergyIdx s newrec f t

-- P_0.1_1.2 = b(P_0.1_2.1, n_0.1_1.2)
mkDiffEqTerm oldrec z@(Power (PowerIdx s newrec f t) := BEdge _ (FEta _)) = 
  {- trace (showEqTerm z ++ " => " ++ showEqTerm res) $ -} Just [res, eres]
  where res = dq := (dp :* (Recip n)) :+ (Minus ((p :* dn) :* nom)) :+ (Minus ((dp :* dn) :* nom))
        dq = mkVar $ DPowerIdx s newrec f t
        dp = mkVar $ DPowerIdx s newrec t f
        p = mkVar $ PowerIdx s oldrec t f
        n = mkVar $ FEtaIdx s oldrec f t
        dn = mkVar $ DEtaIdx s newrec f t
        nom = Recip ((dn :* n) :+ (n :* n))

        eres = de := dq :* dt
        dt = mkVar $ DTimeIdx s newrec
        de = mkVar $ DEnergyIdx s newrec f t


-- v_0.1_OutSum.1 = b(P_0.1_1.2, x_0.1_1.2)
mkDiffEqTerm oldrec z@(Var (VarIdx s newrec use n) := BEdge (Power (PowerIdx _ _ f t)) _) =
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm eres) $ -} Just [res, eres]
  where res = v := (dp :* (Recip x)) :+ (Minus ((p :* dx) :* nom)) :+ (Minus ((dp :* dx) :* nom))
        v = mkVar $ VarIdx s newrec (toDiffUse use) f
        p = mkVar $ PowerIdx s oldrec f t
        dp = mkVar $ DPowerIdx s newrec f t
        x = mkVar $ XIdx s oldrec f t
        dx = mkVar $ DXIdx s newrec f t
        nom = Recip ((dx :* x) :+ (x :* x))

        eres = de := dq :* dt
        dq = mkVar $ DPowerIdx s newrec f t
        dt = mkVar $ DTimeIdx s newrec
        de = mkVar $ DEnergyIdx s newrec f t

mkDiffEqTerm _ t = Nothing


mkDiffEqTermEquations :: Int -> [EqTerm] -> [EqTerm]
mkDiffEqTermEquations rec ts = concat $ mapMaybe (mkDiffEqTerm rec) ts

--------------------------------------------------------------------
-- interpretEq len envs (InEqual (EIdx idx) rhs) = envs { energyMap = insert len idx envs rhs (energyMap envs) }

interpretEqTermRhs :: Envs EqTerm -> EqTerm -> EqTerm
interpretEqTermRhs envs (Power idx) | Just s <- M.lookup idx (powerMap envs) = s
interpretEqTermRhs envs (DPower idx) | Just s <- M.lookup idx (dpowerMap envs) = s
interpretEqTermRhs envs (Energy idx) | Just s <- M.lookup idx (energyMap envs) = s
interpretEqTermRhs envs (DEnergy idx) | Just s <- M.lookup idx (denergyMap envs) = s
interpretEqTermRhs envs (FEta idx) | Just s <- M.lookup idx (fetaMap envs) = s undefined
interpretEqTermRhs envs (DEta idx) | Just s <- M.lookup idx (detaMap envs) = s undefined
interpretEqTermRhs envs (Var idx) | Just s <- M.lookup idx (varMap envs) = s
interpretEqTermRhs envs (X idx) | Just s <- M.lookup idx (xMap envs) = s
interpretEqTermRhs envs (Store idx) | Just s <- M.lookup idx (storageMap envs) = s
interpretEqTermRhs envs (DTime idx) | Just s <- M.lookup idx (dtimeMap envs) = s
interpretEqTermRhs envs (Minus x) = Minus $ interpretEqTermRhs envs x
interpretEqTermRhs envs (Recip x) = Recip $ interpretEqTermRhs envs x
interpretEqTermRhs envs (x :+ y) = interpretEqTermRhs envs x :+ interpretEqTermRhs envs y
interpretEqTermRhs envs (x :* y) = interpretEqTermRhs envs x :* interpretEqTermRhs envs y
interpretEqTermRhs _ t = t

insertEqTerm idx envs rhs m = M.insert idx (interpretEqTermRhs envs rhs) m

interpretEqTermEq :: Envs EqTerm -> EqTerm -> Envs EqTerm

interpretEqTermEq envs (t@(Power idx) := Given) = envs { powerMap = insertEqTerm idx envs t (powerMap envs) }
interpretEqTermEq envs (t@(DPower idx) := Given) = envs { dpowerMap = insertEqTerm idx envs t (dpowerMap envs) }
interpretEqTermEq envs (t@(Energy idx) := Given) = envs { energyMap = insertEqTerm idx envs t (energyMap envs) }
interpretEqTermEq envs (t@(DEnergy idx) := Given) = envs { denergyMap = insertEqTerm idx envs t (denergyMap envs) }
interpretEqTermEq envs (t@(FEta idx) := Given) = envs { fetaMap = M.insert idx (const t) (fetaMap envs) }
interpretEqTermEq envs (t@(DEta idx) := Given) = envs { detaMap = M.insert idx (const t) (detaMap envs) }
interpretEqTermEq envs (t@(Var idx) := Given) = envs { varMap = insertEqTerm idx envs t (varMap envs) }
interpretEqTermEq envs (t@(X idx) := Given) = envs { xMap = insertEqTerm idx envs t (xMap envs) }
interpretEqTermEq envs (t@(Store idx) := Given) = envs { storageMap = insertEqTerm idx envs t (storageMap envs) }
interpretEqTermEq envs (t@(DTime idx) := Given) = envs { dtimeMap = insertEqTerm idx envs t (dtimeMap envs) }

interpretEqTermEq envs (Power idx := rhs) = envs { powerMap = insertEqTerm idx envs rhs (powerMap envs) }
interpretEqTermEq envs (DPower idx := rhs) = envs { dpowerMap = insertEqTerm idx envs rhs (dpowerMap envs) }
interpretEqTermEq envs (Energy idx := rhs) = envs { energyMap = insertEqTerm idx envs rhs (energyMap envs) }
interpretEqTermEq envs (DEnergy idx := rhs) = envs { denergyMap = insertEqTerm idx envs rhs (denergyMap envs) }
interpretEqTermEq envs (FEta idx := rhs) = envs { fetaMap = M.insert idx (const rhs) (fetaMap envs) }
interpretEqTermEq envs (DEta idx := rhs) = envs { detaMap = M.insert idx (const rhs) (detaMap envs) }
interpretEqTermEq envs (X idx := rhs) = envs { xMap = insertEqTerm idx envs rhs (xMap envs) }
interpretEqTermEq envs (DX idx := rhs) = envs { dxMap = insertEqTerm idx envs rhs (dxMap envs) }
interpretEqTermEq envs (Var idx := rhs) = envs { varMap = insertEqTerm idx envs rhs (varMap envs) }
interpretEqTermEq envs (Store idx := rhs) = envs { storageMap = insertEqTerm idx envs rhs (storageMap envs) }
interpretEqTermEq envs (DTime idx := rhs) = envs { dtimeMap = insertEqTerm idx envs rhs (dtimeMap envs) }
interpretEqTermEq envs t = error $ "interpretEqTerm: " ++ show t

interpretEqTermFromScratch :: [EqTerm] -> Envs EqTerm
interpretEqTermFromScratch ts = L.foldl' interpretEqTermEq emptyEnv ts

mapEqTermEnv :: forall a b. (a -> b) -> Envs a -> Envs b
mapEqTermEnv f env = emptyEnv { recordNumber = recordNumber env,
                                energyMap = M.map f (energyMap env),
                                denergyMap = M.map f (denergyMap env),
                                powerMap = M.map f (powerMap env),
                                dpowerMap = M.map f (dpowerMap env),
                                --fetaMap = M.map (f .) (fetaMap env),  -- geht nicht?
                                --detaMap = M.map (S.map f .) (detaMap env),
                                dtimeMap = M.map f (dtimeMap env),
                                xMap = M.map f (xMap env),
                                dxMap = M.map f (dxMap env),
                                varMap = M.map f (varMap env),
                                storageMap = M.map f (storageMap env) }

--------------------------------------------------------------------
