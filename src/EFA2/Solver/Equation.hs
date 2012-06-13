{-# LANGUAGE TypeSynonymInstances #-}


module EFA2.Solver.Equation where

import Control.Exception

import Data.Maybe

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


-- test terms

p1 = Power (PowerIdx 0 0 0 1)
p2 = Power (PowerIdx 0 0 0 2)
p3 = Power (PowerIdx 0 0 0 3)
p4 = Power (PowerIdx 0 0 0 4)

dp1 = DPower (DPowerIdx 0 0 0 1)
dp2 = DPower (DPowerIdx 0 0 0 2)
dp3 = DPower (DPowerIdx 0 0 0 3)
dp4 = DPower (DPowerIdx 0 0 0 4)


c = Const 1.0

e = FEta (FEtaIdx 0 0 0 1)
de = DEta (DEtaIdx 0 0 0 1)

t =  p2 := FEdge p1 e


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

-- e = u * v
isolateVar' (FNode u v) (L:p) = isolateVar' u p . (flip BNode v)
isolateVar' (FNode u v) (R:p) = isolateVar' v p . (flip XNode u)

-- e = u / v
isolateVar' (BNode u v) (L:p) = isolateVar' u p . (flip FNode v)
isolateVar' (BNode u v) (R:p) = isolateVar' v p . (flip XNode u)

-- n = u/v
isolateVar' (XNode u v) (L:p) = isolateVar' u p . (flip FNode v)
isolateVar' (XNode u v) (R:p) = isolateVar' v p . (flip BNode u)




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

{-
u = (e :* ((p2 :* p3) :+ (p4 :* p1))) :+ (e :* ((p2 :* p3) :+ (p4 :* p1)))
-}

pushMult' :: EqTerm -> [EqTerm]
pushMult' (Minus u) = map Minus (pushMult' u)
pushMult' (Recip u) = [Recip (L.foldl1' (:+) $ pushMult' u)]
pushMult' (u :+ v) = pushMult' u ++ pushMult' v
pushMult' (u :* v) = map f (sequence [u', v'])
  where u' = pushMult' u
        v' = pushMult' v
        f [x, y] = x :* y
pushMult' t = [t]

pushMult :: EqTerm -> EqTerm
pushMult (u := v) = add (pushMult' u) := add (pushMult' v)
pushMult u = add (pushMult' u)

-- break into additive Terms
additiveTerms' :: EqTerm -> [EqTerm]
additiveTerms' (x :+ y) = additiveTerms' x ++ additiveTerms' y
additiveTerms' t = [t]

additiveTerms = additiveTerms' . pushMult

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
 


mkDiffEqTerm :: Int -> EqTerm -> Maybe EqTerm
mkDiffEqTerm rec z@( Power (PowerIdx s'' _ f'' t'') :=
                   (FEdge p@(Power (PowerIdx s _ f t)) n@(FEta (FEtaIdx s' _ f' t')))) = Just res
  where res = dq := (dp :* n) :+ (p :* dn) :+ (dp :* dn)
        dq = mkVar $ DPowerIdx s'' rec f'' t''
        dn = mkVar $ DEtaIdx s' rec f' t'
        dp = mkVar $ DPowerIdx s rec f t


mkDiffEqTerm rec z@( Power (PowerIdx s'' _ f'' t'') :=
                   (FNode v@(Var (VarIdx s _ use t)) x@(X (XIdx s' r f' t')))) = Just res
  where res = dq := (dv :* x) :+ (v :* dx) :+ (dv :* dx)
        dq = mkVar $ DPowerIdx s'' rec f'' t''
        dx = mkVar $ DXIdx s' rec f' t'
        dv = (mkVar $ VarIdx s' rec use t) :+ Minus (mkVar $ VarIdx s r use t)


{- works a little bit...
mkDiffEqTerm rec ( Power (PowerIdx s'' _ f'' t'') :=
                   (FNode (Var (VarIdx s _ use t)) x@(X (XIdx s' r f' t')))) = trace (showEqTerm res) $ Just res
  where res = dq := (dp :* x) :+ (p :* dx) :+ (dp :* dx)
        dq = mkVar $ DPowerIdx s'' rec f'' t''
        dx = mkVar $ DXIdx s' rec f' t'
        p = mkVar $ PowerIdx s'' rec (f''-1) (t''-1)
        dp = mkVar $ DPowerIdx s' rec (t''-1) (f''-1)
-}

mkDiffEqTerm rec ((Var (VarIdx s _ use t)) := BNode p@(Power (PowerIdx s'' _ f'' t'')) x@(X (XIdx s' r f' t'))) = Just res
  where res = v := (dp :* (Recip x)) :+ (Minus ((p :* dx) :* nom)) :+ (Minus ((dp :* dx) :* nom))
        v = mkVar $ VarIdx s rec use t
        dp = mkVar $ DPowerIdx s'' rec f'' t''
        dx = mkVar $ DXIdx s' rec f' t'
        nom = ((dx :* x) :+ (x :* x))

mkDiffEqTerm rec ( Power (PowerIdx s'' _ f'' t'') :=
                   (BEdge p@(Power (PowerIdx s _ f t)) n@(FEta (FEtaIdx s' _ f' t')))) = Just res
  where res = dq := (dp :* (Recip n)) :+ (Minus ((p :* dn) :* nom)) :+ (Minus ((dp :* dn) :* nom))
        dq = mkVar $ DPowerIdx s'' rec f'' t''
        dn = mkVar $ DEtaIdx s' rec f' t'
        dp = mkVar $ DPowerIdx s rec f t
        nom = Recip ((dn :* n) :+ (n :* n))


mkDiffEqTerm _ t@(_ := (NEdge _ _)) = error $ "mkDiffEq: " ++ showEqTerm t ++ " cannot be computed with Differenzenrechnung"
mkDiffEqTerm _ _ = Nothing


mkDiffEqTermEquations :: Int -> [EqTerm] -> [EqTerm]
mkDiffEqTermEquations rec ts = catMaybes (map (mkDiffEqTerm rec) ts)

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


mapEqTermEnv :: (a -> b) -> Envs a -> Envs b
mapEqTermEnv f env = emptyEnv { recordNumber = recordNumber env,
                                energyMap = M.map f (energyMap env),
                                denergyMap = M.map f (denergyMap env),
                                powerMap = M.map f (powerMap env),
                                dpowerMap = M.map f (dpowerMap env),
                                --fetaMap = M.map (smap f .) (fetaMap env),
                                --detaMap = M.map (smap f .) (detaMap env),
                                dtimeMap = M.map f (dtimeMap env),
                                xMap = M.map f (xMap env),
                                varMap = M.map f (varMap env),
                                storageMap = M.map f (storageMap env) }


--------------------------------------------------------------------
