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
import EFA2.Interpreter.Eta
import EFA2.Utils.Utils


--eqToInTerm :: (Arith a) => Envs a -> EqTerm -> InTerm a
eqToInTerm envs (Const x) = InConst (cst x)
eqToInTerm envs (Energy idx := Given) = InEqual (EIdx idx) (InGiven (energyMap envs M.! idx))
eqToInTerm envs (DEnergy idx := Given) = InEqual (DEIdx idx) (InGiven (denergyMap envs M.! idx))
eqToInTerm envs (Power idx := Given) = InEqual (PIdx idx) (InGiven (powerMap envs M.! idx))
eqToInTerm envs (DPower idx := Given) = InEqual (DPIdx idx) (InGiven (dpowerMap envs M.! idx))
--eqToInTerm envs (Eta idx := Given) = InEqual (NIdx idx) (InGiven (etaMap envs M.! idx))

eqToInTerm envs (FEta idx p := Given) = InEqual (FNIdx idx (eqToInTerm envs p)) (InFunc (fetaMap envs M.! idx))

eqToInTerm envs (DEta idx := Given) = InEqual (DNIdx idx) (InGiven (detaMap envs M.! idx))
eqToInTerm envs (DTime idx := Given) = InEqual (DTIdx idx) (InGiven (dtimeMap envs M.! idx))

eqToInTerm envs (X idx := Given) = InEqual (ScaleIdx idx) (InGiven (xMap envs M.! idx))
eqToInTerm envs (Var idx := Given) = InEqual (VIdx idx) (InGiven (varMap envs M.! idx))
eqToInTerm envs (Store idx := Given) = InEqual (SIdx idx) (InGiven (storageMap envs M.! idx))
eqToInTerm _ (Energy idx) = EIdx idx
eqToInTerm _ (DEnergy idx) = DEIdx idx
eqToInTerm _ (Power idx) = PIdx idx
eqToInTerm _ (DPower idx) = DPIdx idx
-- eqToInTerm _ (Eta idx) = NIdx idx
eqToInTerm envs (FEta idx p) = FNIdx idx (eqToInTerm envs p)
eqToInTerm _ (DEta idx) = DNIdx idx

eqToInTerm _ (DTime idx) = DTIdx idx
eqToInTerm _ (X idx) = ScaleIdx idx
eqToInTerm _ (Var idx) = VIdx idx
eqToInTerm _ (Store idx) = SIdx idx
eqToInTerm envs (Recip x) = InRecip (eqToInTerm envs x)
eqToInTerm envs (Minus x) = InMinus (eqToInTerm envs x)
eqToInTerm envs (x :+ y) = InAdd (eqToInTerm envs x) (eqToInTerm envs y)
eqToInTerm envs (x :* y) = InMult (eqToInTerm envs x) (eqToInTerm envs y)
eqToInTerm envs (x := y) = InEqual (eqToInTerm envs x) (eqToInTerm envs y)
eqToInTerm _ t = error (show t)


--showInTerm :: (Show a) => InTerm a -> String
showInTerm (EIdx (EnergyIdx s r x y)) = "E:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DEIdx (DEnergyIdx s r x y)) = "dE:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (PIdx (PowerIdx s r x y)) = "P:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DPIdx (DPowerIdx s r x y)) = "dP:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (FNIdx (FEtaIdx s r x y) p) = "n:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." 
                                         ++ show y ++ "(" ++ showInTerm p ++ ")"
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


--interpretRhs :: (Arith a, Show a) => Envs a -> InTerm a -> a
interpretRhs _ (InConst xs) = xs
interpretRhs _ (InGiven xs) = xs
interpretRhs envs (EIdx idx) = energyMap envs M.! idx
interpretRhs envs (DEIdx idx) = denergyMap envs M.! idx
interpretRhs envs (PIdx idx) = powerMap envs M.! idx
interpretRhs envs (DPIdx idx) = dpowerMap envs M.! idx
interpretRhs envs (FNIdx idx p) = (fetaMap envs M.! idx) pval
  where pval = interpretRhs envs p
interpretRhs envs (DNIdx idx) = detaMap envs M.! idx
interpretRhs envs (DTIdx idx) = dtimeMap envs M.! idx
interpretRhs envs (ScaleIdx idx) = xMap envs M.! idx
interpretRhs envs (VIdx idx) = varMap envs M.! idx
interpretRhs envs (SIdx idx) = storageMap envs M.! idx
interpretRhs envs (InMinus t) = neg (interpretRhs envs t)
interpretRhs envs (InRecip t) = rec (interpretRhs envs t)
interpretRhs envs (InAdd s t) = (interpretRhs envs s) .+ (interpretRhs envs t)
interpretRhs envs (InMult s t) = (interpretRhs envs s) .* (interpretRhs envs t)
interpretRhs _ t = error (show t)


insert :: (Arith a, Show a, Ord k) => k -> Envs a -> InTerm a -> M.Map k a -> M.Map k a
insert idx envs rhs m = M.insert idx (interpretRhs envs rhs) m

--interpretEq :: (Show a, Arith a, Ord a) => Envs a -> InTerm a -> Envs a
interpretEq envs (InEqual (EIdx idx) rhs) = envs { energyMap = insert idx envs rhs (energyMap envs) }
interpretEq envs (InEqual (DEIdx idx) rhs) = envs { denergyMap = insert idx envs rhs (denergyMap envs) }
interpretEq envs (InEqual (PIdx idx) rhs) = envs { powerMap = insert idx envs rhs (powerMap envs) }
interpretEq envs (InEqual (DPIdx idx) rhs) = envs { dpowerMap = insert idx envs rhs (dpowerMap envs) }
interpretEq envs (InEqual (FNIdx idx _) (InFunc f)) = envs { fetaMap = M.insert idx f (fetaMap envs) }
interpretEq envs (InEqual (FNIdx idx@(FEtaIdx s r f t) _) (InMult (InRecip (PIdx pidx1)) (PIdx pidx2))) = envs''
  where envs' = envs { fetaMap = M.insert idx (mkEtaFunc pts) (fetaMap envs) }
        envs'' = envs' { fetaMap = M.insert (FEtaIdx s r t f) (mkEtaFunc (reversePts pts)) (fetaMap envs') }
        p1 = powerMap envs M.! pidx1
        p2 = powerMap envs M.! pidx2
        pts = Pt p2 (p2 ./ p1) 
interpretEq envs (InEqual (DNIdx idx) rhs) = envs { detaMap = insert idx envs rhs (detaMap envs) }
interpretEq envs (InEqual (DTIdx idx) rhs) = envs { dtimeMap = insert idx envs rhs (dtimeMap envs) }
interpretEq envs (InEqual (ScaleIdx idx) rhs) = envs { xMap = insert idx envs rhs (xMap envs) }
interpretEq envs (InEqual (VIdx idx) rhs) = envs { varMap = insert idx envs rhs (varMap envs) }
interpretEq envs (InEqual (SIdx idx) rhs) = envs { storageMap = insert idx envs rhs (storageMap envs) }
interpretEq envs t = error ("interpretEq: " ++ show t)


--interpretFromScratch :: (Show a, Arith a, Ord a) => [InTerm [a]] -> Envs [a]
interpretFromScratch ts = Envs (cut e) (cut de) (cut p) (cut dp) fn (cut dn) (cut t) (cut x) (cut v) (cut s)
  where Envs e de p dp fn dn t x v s  = L.foldl' interpretEq emptyEnv ts
        minLen = minimum $ map snd $ M.toList (M.map length e)
        cut = M.map (take minLen)
