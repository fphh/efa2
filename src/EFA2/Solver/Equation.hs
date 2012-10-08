module EFA2.Solver.Equation where

import Control.Monad (liftM2)
import Data.Maybe (mapMaybe)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M

import Debug.Trace (trace)
import Text.Printf (printf)


import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith (Val)
import EFA2.Utils.Utils (pairs)

-- TOTHINK: Die Algorithmen aus dem Verzeichnis Solver sollten
-- über den Datentyp EqTerm parametrisierbar sein. Die Abhängigkeitsanalyse
-- sollte nichts mit den konkreten Termen zu tun haben. Diese Entscheidung
-- haette wahrscheinlich auch Einfluss auf InVar...

data Equation =
            EqTerm := EqTerm
          | Given EqTerm deriving (Show, Eq, Ord)

data EqTerm =
            Const Val -- Double
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

(!=) :: (MkVarC a, MkVarC b) => a -> b -> Equation
x != y = mkVar x := mkVar y

give :: MkVarC a => a -> Equation
give idx = Given (mkVar idx)

class MkVarC a where
      mkVar :: a -> EqTerm

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

instance MkVarC Double where
         mkVar = Const

instance MkVarC EqTerm where
         mkVar = id

add :: NonEmpty.T [] EqTerm -> EqTerm
add = NonEmpty.foldl1 (:+)

mult :: NonEmpty.T [] EqTerm -> EqTerm
mult = NonEmpty.foldl1 (:*)

showEqTerm :: EqTerm -> String
showEqTerm (Const x) = show x

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

showEquation :: Equation -> String
showEquation (Given x) = showEqTerm x ++ " given"
showEquation (x := y) = showEqTerm x ++ " = " ++ showEqTerm y

showEqTerms :: [EqTerm] -> String
showEqTerms ts = L.intercalate "\n" $ map showEqTerm ts

showEquations :: [Equation] -> String
showEquations ts = L.intercalate "\n" $ map showEquation ts


newtype LatexString = LatexString { unLatexString :: String } deriving (Show, Eq)

toLatexString' :: EqTerm -> String
toLatexString' (Const x) = printf "%.6f   " x

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

eqToLatexString' :: Equation -> String
eqToLatexString' (Given x) = toLatexString' x ++ " \\mbox{given}"
eqToLatexString' (x := y) = toLatexString' x ++ " = " ++ toLatexString' y

toLatexString :: EqTerm -> LatexString
toLatexString t = LatexString $ "$" ++ toLatexString' t ++ "$"

eqToLatexString :: Equation -> LatexString
eqToLatexString t = LatexString $ "$" ++ eqToLatexString' t ++ "$"


-- | This function takes a predicate p that determines, wether
-- a term is a variable or not. It then takes a term and
-- determines the set of variables contained in the term,
-- according to the predicate.
mkVarSetEq :: (EqTerm -> Bool) -> Equation -> S.Set EqTerm
mkVarSetEq p (Given x) = mkVarSet p x
mkVarSetEq p (x := y) = S.union (mkVarSet p x) (mkVarSet p y)

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
        mkVarSet' _ = S.empty


-- The following functions transform an equation.

data Dir = L | R deriving (Show, Eq)

type TPath = [Dir]


prepStep ::
   (Show eq0, Show eq1) =>
   eq0 -> eq1 -> (Maybe TPath, Maybe TPath) -> Maybe TPath
prepStep t s p =
   case p of
      (Nothing, x) -> fmap (R:) x
      (x, Nothing) -> fmap (L:) x
      _ -> error $ "error in looking for path to (" ++ show t ++ ") in (" ++ show s ++ ")"

findVarEq :: EqTerm -> Equation -> Maybe TPath
findVarEq t s@(Given u) =
   prepStep t s (findVar t u, Nothing)
--   if t == u then Just [] else Nothing
findVarEq t s@(u := v) =
   prepStep t s (findVar t u, findVar t v)

findVar :: EqTerm -> EqTerm -> Maybe TPath
findVar t s =
   if t == s
     then Just []
     else prepStep t s $
        case s of
           (u :+ v) -> (findVar t u, findVar t v)
           (u :* v) -> (findVar t u, findVar t v)
           (Minus u) -> (findVar t u, Nothing)    -- coding: Minus has only left operand.
           (Recip u) -> (findVar t u, Nothing)    -- coding: Recip has only left operand.
           (FEdge power eta) -> (findVar t power, findVar t eta)
           (BEdge power eta) -> (findVar t power, findVar t eta)
           (NEdge power eta) -> (findVar t power, findVar t eta)

           (FNode power eta) -> (findVar t power, findVar t eta)
           (BNode power eta) -> (findVar t power, findVar t eta)
           (XNode power eta) -> (findVar t power, findVar t eta)

           _ -> (Nothing, Nothing)

isolateVar :: EqTerm -> Equation -> TPath -> Equation
isolateVar s (Given u) [L] = (Given s)
isolateVar s (Given u) [R] = (s := u)
isolateVar s (u := v) (L:p) = (s := isolateVar' u p v)
isolateVar s (u := v) (R:p) = (s := isolateVar' v p u)
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
transformEq :: EqTerm -> Equation -> Equation
transformEq unknown t =
   maybe t (isolateVar unknown t) $ findVarEq unknown t

--------------------------------------------------------------------


pushMult :: EqTerm -> EqTerm
pushMult t = add $ pushMult' t

{-
pushMult :: EqTerm -> EqTerm
pushMult t =
   case t of
      u := v  ->  add (pushMult' u) := add (pushMult' v)
      _       ->  add (pushMult' t)
-}

pushMult' :: EqTerm -> NonEmpty.T [] EqTerm
pushMult' (Minus u) = fmap Minus (pushMult' u)
pushMult' (Recip u) = NonEmpty.singleton $ Recip $ pushMult u
pushMult' (u :+ v) = NonEmpty.append (pushMult' u) (pushMult' v)
pushMult' (u :* v) = liftM2 (:*) (pushMult' u) (pushMult' v)
pushMult' t = NonEmpty.singleton t

simplify :: EqTerm -> EqTerm
simplify = fst . head . dropWhile (uncurry (/=)) . pairs . iterate simplify' . pushMult
  where simplify' :: EqTerm -> EqTerm
        simplify' ((Const 0.0) :+ x) = simplify' x
        simplify' (x :+ (Const 0.0)) = simplify' x
        simplify' ((Const 1.0) :* x) = simplify' x
        simplify' (x :* (Const 1.0)) = simplify' x
        simplify' ((Const 0.0) :* _) = Const 0.0
        simplify' (_ :* (Const 0.0)) = Const 0.0

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
        simplify' x = x

simplifyEq :: Equation -> Equation
simplifyEq (Given x) = Given (simplify x)
simplifyEq (x := y) = simplify x := simplify y

additiveTerms :: EqTerm -> [EqTerm]
additiveTerms = NonEmpty.flatten . additiveTermsNonEmpty

additiveTermsNonEmpty :: EqTerm -> NonEmpty.T [] EqTerm
additiveTermsNonEmpty = recourse . simplify . pushMult
  where recourse (x :+ y) =
           NonEmpty.append (recourse x) (recourse y)
        recourse t = NonEmpty.singleton t


setEqTerms :: Envs EqTerm -> EqTerm -> EqTerm
setEqTerms envs term =
   case term of
      (Power idx) -> M.findWithDefault term idx (powerMap envs)
      (DPower idx) -> M.findWithDefault term idx (dpowerMap envs)
      (Energy idx) -> M.findWithDefault term idx (energyMap envs)
      (DEnergy idx) -> M.findWithDefault term idx (denergyMap envs)
      (FEta idx) -> maybe term (\t -> trace "setEqTerms" $ t undefined) $ M.lookup idx (fetaMap envs)
      (DEta idx) -> maybe term (\t -> trace "setEqTerms" $ t undefined) $ M.lookup idx (detaMap envs)
      (X idx) -> M.findWithDefault term idx (xMap envs)
      (DX idx) -> M.findWithDefault term idx (dxMap envs)
      (Var idx) -> M.findWithDefault term idx (varMap envs)
      (Store idx) -> M.findWithDefault term idx (storageMap envs)
      (DTime idx) -> M.findWithDefault term idx (dtimeMap envs)
      (Minus t) -> Minus (setEqTerms envs t)
      (Recip t) -> Recip (setEqTerms envs t)
      (s :+ t) -> setEqTerms envs s :+ setEqTerms envs t
      (s :* t) -> setEqTerms envs s :* setEqTerms envs t
      _ -> term

setEquations :: Envs EqTerm -> Equation -> Equation
setEquations envs (Given s)  =  Given (setEqTerms envs s)
setEquations envs (s := t)  =  setEqTerms envs s := setEqTerms envs t

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
toAbsEqTerm t = t

toAbsEqTerms :: [EqTerm] -> [EqTerm]
toAbsEqTerms ts = map toAbsEqTerm ts

toAbsEquation :: Equation -> Equation
toAbsEquation (Given x) = Given (toAbsEqTerm x)
toAbsEquation (x := y) = toAbsEqTerm x := toAbsEqTerm y

toAbsEquations :: [Equation] -> [Equation]
toAbsEquations = map toAbsEquation


mkDiffEqTerm :: Int -> Equation -> Maybe [Equation]

-- v_0.1_OutSum.0 = P_0.1_0.1
mkDiffEqTerm _ (Var (VarIdx s r use n) := Power (PowerIdx _ _ f t)) =
  {- trace ("--->: " ++ showEqTerm z ++ " s=> " ++ showEqTerm eres) $ -} Just [res, eres]
  where res = v := dp
        v = mkVar $ VarIdx s r (toDiffUse use) n
        dp = mkVar $ DPowerIdx s r f t

        eres = de := dp :* dt
        dt = mkVar $ DTimeIdx s r
        de = mkVar $ DEnergyIdx s r f t



-- v_0.1_OutSum.1 = (P_0.1_1.2 + P_0.1_1.3) ...
mkDiffEqTerm _ (Var (VarIdx s r use n) := as@(_x :+ _y)) =
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm res) $ -} Just [res]
  where res = v := dps
        ats = additiveTermsNonEmpty as
        v = mkVar $ VarIdx s r (toDiffUse use) n
        dps = add $ fmap g ats
        g (Power (PowerIdx _ _ f t)) = mkVar $ DPowerIdx s r f t

-- P_0.1_0.1 = v_0.1_OutSum.0
mkDiffEqTerm _ (Power (PowerIdx _ _ f t) := Var (VarIdx s r use n)) =
  {- trace ("--->: " ++ showEqTerm z ++ " => " ++ showEqTerm eres) $ -} Just [res, eres]
  where res = dp := v
        v = mkVar $ VarIdx s r (toDiffUse use) n
        dp = mkVar $ DPowerIdx s r f t

        eres = de := dp :* dt
        dt = mkVar $ DTimeIdx s r
        de = mkVar $ DEnergyIdx s r f t


-- v_0.1_OutSum.1 = v_0.1_InSum.1
mkDiffEqTerm _ (Var (VarIdx s r use0 n) := Var (VarIdx _ _ use1 _)) =
  {- trace (showEqTerm z ++ " => " ++ showEqTerm res) $ -} Just [res]
  where res = v0 := v1
        v0 = mkVar $ VarIdx s r (toDiffUse use0) n
        v1 = mkVar $ VarIdx s r (toDiffUse use1) n

-- P_0.1_1.0 = f(P_0.1_0.1, n_0.1_0.1)
mkDiffEqTerm oldrec (Power (PowerIdx s newrec f t) := FEdge (Power _) _) =
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
mkDiffEqTerm oldrec (Power (PowerIdx s newrec f t) := FEdge (Var (VarIdx _ _ use _)) _) =
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
mkDiffEqTerm oldrec (Power (PowerIdx s newrec f t) := BEdge _ (FEta _)) =
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
mkDiffEqTerm oldrec (Var (VarIdx s newrec use _n) := BEdge (Power (PowerIdx _ _ f t)) _) =
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

mkDiffEqTerm _ _ = Nothing


mkDiffEqTermEquations :: Int -> [Equation] -> [Equation]
mkDiffEqTermEquations rec ts = concat $ mapMaybe (mkDiffEqTerm rec) ts

--------------------------------------------------------------------
-- interpretEq len envs (InEqual (EIdx idx) rhs) = envs { energyMap = insert len idx envs rhs (energyMap envs) }

interpretEqTermRhs :: Envs EqTerm -> EqTerm -> EqTerm
interpretEqTermRhs envs t =
   case t of
      (Power idx) -> M.findWithDefault t idx (powerMap envs)
      (DPower idx) -> M.findWithDefault t idx (dpowerMap envs)
      (Energy idx) -> M.findWithDefault t idx (energyMap envs)
      (DEnergy idx) -> M.findWithDefault t idx (denergyMap envs)
      (FEta idx) -> maybe t ($undefined) $ M.lookup idx (fetaMap envs)
      (DEta idx) -> maybe t ($undefined) $ M.lookup idx (detaMap envs)
      (Var idx) -> M.findWithDefault t idx (varMap envs)
      (X idx) -> M.findWithDefault t idx (xMap envs)
      (DX idx) -> M.findWithDefault t idx (dxMap envs)
      (Store idx) -> M.findWithDefault t idx (storageMap envs)
      (DTime idx) -> M.findWithDefault t idx (dtimeMap envs)
      (Minus x) -> Minus $ interpretEqTermRhs envs x
      (Recip x) -> Recip $ interpretEqTermRhs envs x
      (x :+ y) -> interpretEqTermRhs envs x :+ interpretEqTermRhs envs y
      (x :* y) -> interpretEqTermRhs envs x :* interpretEqTermRhs envs y
      _ -> t

insertEqTerm ::
   Ord k =>
   k -> Envs EqTerm -> EqTerm -> M.Map k EqTerm -> M.Map k EqTerm
insertEqTerm idx envs rhs m = M.insert idx (interpretEqTermRhs envs rhs) m

interpretEqTermEq :: Envs EqTerm -> Equation -> Envs EqTerm
interpretEqTermEq envs (Given t) =
   case t of
      Power idx -> envs { powerMap = insertEqTerm idx envs t (powerMap envs) }
      DPower idx -> envs { dpowerMap = insertEqTerm idx envs t (dpowerMap envs) }
      Energy idx -> envs { energyMap = insertEqTerm idx envs t (energyMap envs) }
      DEnergy idx -> envs { denergyMap = insertEqTerm idx envs t (denergyMap envs) }
      FEta idx -> envs { fetaMap = M.insert idx (const t) (fetaMap envs) }
      DEta idx -> envs { detaMap = M.insert idx (const t) (detaMap envs) }
      Var idx -> envs { varMap = insertEqTerm idx envs t (varMap envs) }
      X idx -> envs { xMap = insertEqTerm idx envs t (xMap envs) }
      DX idx -> envs { dxMap = insertEqTerm idx envs t (dxMap envs) }
      Store idx -> envs { storageMap = insertEqTerm idx envs t (storageMap envs) }
      DTime idx -> envs { dtimeMap = insertEqTerm idx envs t (dtimeMap envs) }
      _ -> error $ "interpretEqTerm/Given: " ++ show t

interpretEqTermEq envs (t := rhs) =
   case t of
      Power idx -> envs { powerMap = insertEqTerm idx envs rhs (powerMap envs) }
      DPower idx -> envs { dpowerMap = insertEqTerm idx envs rhs (dpowerMap envs) }
      Energy idx -> envs { energyMap = insertEqTerm idx envs rhs (energyMap envs) }
      DEnergy idx -> envs { denergyMap = insertEqTerm idx envs rhs (denergyMap envs) }
      FEta idx -> envs { fetaMap = M.insert idx (const rhs) (fetaMap envs) }
      DEta idx -> envs { detaMap = M.insert idx (const rhs) (detaMap envs) }
      X idx -> envs { xMap = insertEqTerm idx envs rhs (xMap envs) }
      DX idx -> envs { dxMap = insertEqTerm idx envs rhs (dxMap envs) }
      Var idx -> envs { varMap = insertEqTerm idx envs rhs (varMap envs) }
      Store idx -> envs { storageMap = insertEqTerm idx envs rhs (storageMap envs) }
      DTime idx -> envs { dtimeMap = insertEqTerm idx envs rhs (dtimeMap envs) }
      _ -> error $ "interpretEqTerm/Rhs: " ++ show t

interpretEqTermFromScratch :: [Equation] -> Envs EqTerm
interpretEqTermFromScratch ts = L.foldl' interpretEqTermEq emptyEnv ts

mapEqTermEnv :: (a -> b) -> Envs a -> Envs b
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
