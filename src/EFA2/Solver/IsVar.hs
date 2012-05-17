

module EFA2.Solver.IsVar where

import Data.Graph.Inductive
import qualified Data.Vector.Unboxed as UV
import qualified Data.List as L
import qualified Data.Set as S

import Debug.Trace

import EFA2.Solver.Equation
import EFA2.Interpreter.Env
import EFA2.Utils.Utils

{-- This algorithm is fast, but buggy.

-- | Section, record, from, to.
data Tableau = Tableau (UV.Vector Bool) (UV.Vector Bool) (UV.Vector Bool) (UV.Vector Bool) deriving (Show)

mkTableau :: (UpdateAcc -> EqTerm -> UpdateAcc) -> Int -> [EqTerm] -> Tableau
mkTableau updatef len ts = Tableau sec rec from to
  where empty = UV.replicate len False
        sec = UV.update empty us
        rec = UV.update empty ur
        from = UV.update empty uf
        to = UV.update empty ut
        (s, r, f, t) = L.foldl' updatef ([], [], [], []) ts
        b = repeat True
        (us, ur, uf, ut) = (UV.fromList $ zip s b, UV.fromList $ zip r b, UV.fromList $ zip f b, UV.fromList $ zip t b)






type UpdateAcc = ([Int], [Int], [Int], [Int])

powerUpdateVec :: UpdateAcc -> EqTerm -> UpdateAcc
powerUpdateVec (s, r, f, t) (Power (PowerIdx u v w x) := Given) = (u:s, v:s, w:f, x:t)
powerUpdateVec acc _ = acc

etaUpdateVec :: UpdateAcc -> EqTerm -> UpdateAcc
etaUpdateVec (s, r, f, t) (Eta (EtaIdx u v w x) := Given) = (u:s, v:s, w:f, x:t)
etaUpdateVec acc _ = acc

dpowerUpdateVec :: UpdateAcc -> EqTerm -> UpdateAcc
dpowerUpdateVec (s, r, f, t) (DPower (DPowerIdx u v w x) := Given) = (u:s, v:s, w:f, x:t)
dpowerUpdateVec acc _ = acc

detaUpdateVec :: UpdateAcc -> EqTerm -> UpdateAcc
detaUpdateVec (s, r, f, t) (DEta (DEtaIdx u v w x) := Given) = (u:s, v:s, w:f, x:t)
detaUpdateVec acc _ = acc

xUpdateVec :: UpdateAcc -> EqTerm -> UpdateAcc
xUpdateVec (s, r, f, t) (X (XIdx u v w x) := Given) = (u:s, v:s, w:f, x:t)
xUpdateVec acc _ = acc

-- TODO: Das Tableau wird bei jedem Aufruf neu generiert. Das muss nicht sein!
isVar :: Gr a b -> [EqTerm] -> (EqTerm -> Bool)
isVar g ts t
  | (Power (PowerIdx s r f t)) <- t = not $ (ps UV.! s) && (pr UV.! r) && (pf UV.! f) && (pt UV.! t)
  | (Eta (EtaIdx s r f t)) <- t = not $ (es UV.! s) && (er UV.! r) && (ef UV.! f) && (et UV.! t)
  | (DPower (DPowerIdx s r f t)) <- t = not $ (dps UV.! s) && (dpr UV.! r) && (dpf UV.! f) && (dpt UV.! t)
  | (DEta (DEtaIdx s r f t)) <- t = not $ (des UV.! s) && (der UV.! r) && (def UV.! f) && (det UV.! t)
  | (X (XIdx s r f t)) <- t = trace (show tab) $ not $ (xs UV.! s) && (xr UV.! r) && (xf UV.! f) && (xt UV.! t)
  | otherwise = False
  where len = 1 + (snd $ nodeRange g)
        Tableau ps pr pf pt = mkTableau powerUpdateVec len ts
        Tableau es er ef et = mkTableau etaUpdateVec len ts
        Tableau dps dpr dpf dpt = mkTableau dpowerUpdateVec len ts
        Tableau des der def det = mkTableau detaUpdateVec len ts
        tab@(Tableau xs xr xf xt) = mkTableau xUpdateVec len ts
-}

-- | True for 'EqTerm's that are of the form:
-- > ... := Given ...
isGiven :: EqTerm -> Bool
isGiven (_ := Given) = True
isGiven _ = False

-- | True for 'EqTerm's that don't contain variables and for which 'isGiven' is False.
-- We assume for the given predicate (isVar :: EqTerm -> Bool) that: 
-- > not (isVar t) == isGiven t
noVar :: (EqTerm -> Bool) -> EqTerm -> Bool
noVar isVar t = (not $ isGiven t) && (S.size (mkVarSet isVar t) == 0)


-- | True for 'EqTerm's that contain exactly one variable and for which 'isGiven' is False.
-- We assume for the given predicate (isVar :: EqTerm -> Bool) that: 
-- > not (isVar t) == isGiven t
isGivenExtended :: (EqTerm -> Bool) -> EqTerm -> Bool
isGivenExtended isVar t = (not $ isGiven t) && (S.size (mkVarSet isVar t) == 1)


splitTerms :: (EqTerm -> Bool) -> [EqTerm] -> ([EqTerm], [EqTerm], [EqTerm], [EqTerm])
splitTerms isVar ts = (given, nov, givenExt, rest)
  where (given, r0) = L.partition isGiven ts
        (nov, r1) = L.partition (noVar isVar) r0
        (givenExt, rest) = L.partition (isGivenExtended isVar) r1

-- | Predicate to indicate what should be viewed as a variable. Ask me for further explanation.
-- Static version for optimisation.
isVar' :: EqTerm -> Bool
isVar' (Energy (EnergyIdx 0 0 0 1)) = False
isVar' (Energy _) = True
--isVar' (Eta _) = True
isVar' (DEnergy _) = True
isVar' (DEta _) = True
--isVar (X _) = True
isVar' (Var _) = True
isVar' _ = False


-- | True for compound terms.
isCompoundTerm :: EqTerm -> Bool
isCompoundTerm (Energy _) = False
isCompoundTerm (Eta _) = False
isCompoundTerm (DEnergy _) = False
isCompoundTerm (DEta _) = False
isCompoundTerm (X _) = False
isCompoundTerm (Var _) = False
isCompoundTerm (Store _) = False
isCompoundTerm _ = True

-- | True for yntactic variables.
-- > isStaticVar == not . isCompoundTerm
isStaticVar :: EqTerm -> Bool
isStaticVar = not . isCompoundTerm

{-
-- | True for variables that don't appear in 'Given' equations.
-- Used mainly as a reference implementation for 'isVar'.
isVarFromEqs :: S.Set EqTerm -> (EqTerm -> Bool)
isVarFromEqs s t = not (S.member t s || isCompoundTerm t) 
-}

-- | True for variables that don't appear in 'Given' equations.
-- Used mainly as a reference implementation for 'isVar'.
isVarFromEqs :: [EqTerm] -> (EqTerm -> Bool)
isVarFromEqs ts t = not (S.member t s || isCompoundTerm t) 
  where s = L.foldl' f S.empty ts
        f acc (v := Given) = S.insert v acc
        f acc _ = acc
