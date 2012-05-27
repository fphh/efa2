{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances, MultiParamTypeClasses, ScopedTypeVariables #-}

module EFA2.Interpreter.Interpreter where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.Error

import Debug.Trace

import EFA2.Solver.Equation
import EFA2.Interpreter.Arith
import EFA2.Interpreter.InTerm
import EFA2.Interpreter.Env
--import EFA2.Interpreter.Eta
import EFA2.Signal.Signal
import EFA2.Signal.Typ
import EFA2.Utils.Utils
import EFA2.Signal.Data


--eqToInTerm :: Envs a -> EqTerm -> InTerm a
eqToInTerm envs term = eqToInTerm' term
  where eqToInTerm' (Const x) = InConst x
        eqToInTerm' (Energy idx := Given) = InEqual (EIdx idx) (InGiven (energyMap envs `safeLookup` idx))
        eqToInTerm' (DEnergy idx := Given) = InEqual (DEIdx idx) (InGiven (denergyMap envs `safeLookup` idx))
        eqToInTerm' (Power idx := Given) = InEqual (PIdx idx) (InGiven (powerMap envs `safeLookup` idx))
        eqToInTerm' (DPower idx := Given) = InEqual (DPIdx idx) (InGiven (dpowerMap envs `safeLookup` idx))
        eqToInTerm' (FEta idx := Given) = InEqual (FNIdx idx) (InFunc (fetaMap envs `safeLookup` idx))
        eqToInTerm' (DEta idx := Given) = InEqual (DNIdx idx) (InGiven (detaMap envs `safeLookup` idx))
        eqToInTerm' (DTime idx := Given) = InEqual (DTIdx idx) (InGiven (dtimeMap envs `safeLookup` idx))
        eqToInTerm' (X idx := Given) = InEqual (ScaleIdx idx) (InGiven (xMap envs `safeLookup` idx))
        eqToInTerm' (Var idx := Given) = InEqual (VIdx idx) (InGiven (varMap envs `safeLookup` idx))
        eqToInTerm' (Store idx := Given) = InEqual (SIdx idx) (InGiven (storageMap envs `safeLookup` idx))
        eqToInTerm' (Energy idx) = EIdx idx
        eqToInTerm' (DEnergy idx) = DEIdx idx
        eqToInTerm' (Power idx) = PIdx idx
        eqToInTerm' (DPower idx) = DPIdx idx
        eqToInTerm' (FEta idx) = FNIdx idx -- (eqToInTerm' p)
        eqToInTerm' (DEta idx) = DNIdx idx
        eqToInTerm' (DTime idx) = DTIdx idx
        eqToInTerm' (X idx) = ScaleIdx idx
        eqToInTerm' (Var idx) = VIdx idx
        eqToInTerm' (Store idx) = SIdx idx
        eqToInTerm' (Recip x) = InRecip (eqToInTerm' x)
        eqToInTerm' (Minus x) = InMinus (eqToInTerm' x)
        eqToInTerm' (x :+ y) = InAdd (eqToInTerm' x) (eqToInTerm' y)
        eqToInTerm' (x :* y) = InMult (eqToInTerm' x) (eqToInTerm' y)
        eqToInTerm' (x := y) = InEqual (eqToInTerm' x) (eqToInTerm' y)
        eqToInTerm' t = error (show t)


showInTerm :: (Show a) => InTerm a -> String
showInTerm (EIdx (EnergyIdx s r x y)) = "E:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DEIdx (DEnergyIdx s r x y)) = "dE:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (PIdx (PowerIdx s r x y)) = "P:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DPIdx (DPowerIdx s r x y)) = "dP:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (FNIdx (FEtaIdx s r x y)) = "n:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "."  ++ show y
                                   --      ++ show y ++ "(" ++ showInTerm p ++ ")"
showInTerm (DNIdx (DEtaIdx s r x y)) = "dn:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DTIdx (DTimeIdx s r)) = "dt:" ++ show s ++ "." ++ show r
showInTerm (ScaleIdx (XIdx s r x y)) = "x:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (VIdx (VarIdx s r x y)) = "v:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (SIdx (StorageIdx s r n)) = "s:" ++ show s ++ "." ++ show r ++ ":" ++ show n
showInTerm (InConst x) = take 20 (show x) ++ "..."
showInTerm (InGiven xs) = "given" ++ show xs
showInTerm (InFunc xs) = "given <function>"
showInTerm (InMinus t) = "-(" ++ showInTerm t ++ ")"
showInTerm (InRecip t) = "1/(" ++ showInTerm t ++ ")"
showInTerm (InAdd s t) = "(" ++ showInTerm s ++ " + " ++ showInTerm t ++ ")"
showInTerm (InMult s t) = showInTerm s ++ " * " ++ showInTerm t
showInTerm (InEqual s t) = showInTerm s ++ " = " ++ showInTerm t

showInTerms :: (Show a) => [InTerm a] -> String
showInTerms ts = L.intercalate "\n" $ map showInTerm ts

interpretRhs :: ( SArith s s s, SMap c Val Val, TProd t t t, TSum t t t, DZipWith c c c Val Val Val,
                  FromToList c Val, Show (c Val)) =>
                  Int -> Envs (TC s t (c Val)) -> InTerm (TC s t (c Val)) -> TC s t (c Val)
interpretRhs len envs term = interpretRhs' term
  where interpretRhs' (InConst x) = sfromVal len x
        interpretRhs' (InGiven xs) = xs
        interpretRhs' (EIdx idx) = energyMap envs `safeLookup` idx
        interpretRhs' (DEIdx idx) = denergyMap envs `safeLookup` idx
        interpretRhs' (PIdx idx) = powerMap envs `safeLookup` idx
        interpretRhs' (DPIdx idx) = dpowerMap envs `safeLookup` idx
        interpretRhs' (FNIdx idx@(FEtaIdx s r f t)) = (fetaMap envs `safeLookup` idx) (powerMap envs `safeLookup` pidx)
          where pidx = PowerIdx s r f t
        interpretRhs' (DNIdx idx) = detaMap envs `safeLookup` idx
        interpretRhs' (DTIdx idx) = dtimeMap envs `safeLookup` idx
        interpretRhs' (ScaleIdx idx) = xMap envs `safeLookup` idx
        interpretRhs' (VIdx idx) = varMap envs `safeLookup` idx
        interpretRhs' (SIdx idx) = storageMap envs `safeLookup` idx
        interpretRhs' (InMinus t) = sneg (interpretRhs' t)
        interpretRhs' (InRecip (FNIdx idx@(FEtaIdx s r f t))) = srec $ (fetaMap envs `safeLookup` idx) pval
          where pidx = PowerIdx s r t f
                pval = powerMap envs `safeLookup` pidx
        interpretRhs' (InRecip t) = srec (interpretRhs' t)
        interpretRhs' (InAdd s t) = (interpretRhs' s) .+ (interpretRhs' t)
        interpretRhs' (InMult s t) = (interpretRhs' s) .* (interpretRhs' t)
        interpretRhs' t = error (show t)


insert :: ( SArith s s s, SMap c Val Val, TProd t t t, TSum t t t, DZipWith c c c Val Val Val,
            FromToList c Val, Show (c Val), Ord k) =>
            Int -> k -> Envs (TC s t (c Val)) -> InTerm (TC s t (c Val)) -> M.Map k (TC s t (c Val)) -> M.Map k (TC s t (c Val))
insert len idx envs rhs m = M.insert idx (interpretRhs len envs rhs) m


interpretEq :: ( SArith s s s, SMap c Val Val, TProd t t t, TSum t t t,
                 DZipWith c c c Val Val Val, FromToList c Val, Show (c Val)) =>
                 Int -> Envs (TC s t (c Val)) -> InTerm (TC s t (c Val)) -> Envs (TC s t (c Val))
interpretEq len envs (InEqual (EIdx idx) rhs) = envs { energyMap = insert len idx envs rhs (energyMap envs) }
interpretEq len envs (InEqual (DEIdx idx) rhs) = envs { denergyMap = insert len idx envs rhs (denergyMap envs) }
interpretEq len envs (InEqual (PIdx idx) rhs) = envs { powerMap = insert len idx envs rhs (powerMap envs) }
interpretEq len envs (InEqual (DPIdx idx) rhs) = envs { dpowerMap = insert len idx envs rhs (dpowerMap envs) }
interpretEq len envs (InEqual (FNIdx idx@(FEtaIdx s r f t)) (InFunc feta)) = envs { fetaMap = M.insert idx feta (fetaMap envs) }

{-
interpretEq envs (InEqual (FNIdx idx@(FEtaIdx s r f t) _) (InMult (InRecip (PIdx pidx1)) (PIdx pidx2))) = envs''
  where envs' = envs { fetaMap = M.insert idx (mkEtaFunc pts) (fetaMap envs) }
        envs'' = envs' { fetaMap = M.insert (FEtaIdx s r t f) (mkEtaFunc (reversePts pts)) (fetaMap envs') }
        p1 = powerMap envs M.! pidx1
        p2 = powerMap envs M.! pidx2
        pts = Pt p2 (p2 ./ p1) 
-}
interpretEq len envs (InEqual (DNIdx idx) rhs) = envs { detaMap = insert len idx envs rhs (detaMap envs) }
interpretEq len envs (InEqual (DTIdx idx) rhs) = envs { dtimeMap = insert len idx envs rhs (dtimeMap envs) }
interpretEq len envs (InEqual (ScaleIdx idx) rhs) = envs { xMap = insert len idx envs rhs (xMap envs) }
interpretEq len envs (InEqual (VIdx idx) rhs) = envs { varMap = insert len idx envs rhs (varMap envs) }
interpretEq len envs (InEqual (SIdx idx) rhs) = envs { storageMap = insert len idx envs rhs (storageMap envs) }
interpretEq len envs t = error ("interpretEq: " ++ showInTerm t)



interpretFromScratch :: ( Show (c Val), SArith s s s, SMap c Val Val, DZipWith c c c Val Val Val,
                          FromToList c Val, TProd t t t, TSum t t t) => 
                          Int -> [InTerm (TC s t (c Val))] -> Envs (TC s t (c Val))
interpretFromScratch len ts = L.foldl' (interpretEq len) emptyEnv ts
