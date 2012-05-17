{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, TypeSynonymInstances #-}

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


eqToInTerm :: (Arith a) => Envs a -> EqTerm -> InTerm a
eqToInTerm envs (Const x) = InConst (cst x)
eqToInTerm envs (Energy idx := Given) = InEqual (EIdx idx) (InGiven (energyMap envs M.! idx))
eqToInTerm envs (Eta idx := Given) = InEqual (NIdx idx) (InGiven (etaMap envs M.! idx))
eqToInTerm envs (DEnergy idx := Given) = InEqual (DEIdx idx) (InGiven (denergyMap envs M.! idx))
eqToInTerm envs (DEta idx := Given) = InEqual (DNIdx idx) (InGiven (detaMap envs M.! idx))
eqToInTerm envs (X idx := Given) = InEqual (ScaleIdx idx) (InGiven (xMap envs M.! idx))
eqToInTerm envs (Var idx := Given) = InEqual (VIdx idx) (InGiven (varMap envs M.! idx))
eqToInTerm envs (Store idx := Given) = InEqual (SIdx idx) (InGiven (storageMap envs M.! idx))
eqToInTerm _ (Energy idx) = EIdx idx
eqToInTerm _ (Eta idx) = NIdx idx
eqToInTerm _ (DEnergy idx) = DEIdx idx
eqToInTerm _ (DEta idx) = DNIdx idx
eqToInTerm _ (X idx) = ScaleIdx idx
eqToInTerm _ (Var idx) = VIdx idx
eqToInTerm _ (Store idx) = SIdx idx
eqToInTerm envs (Recip x) = InRecip (eqToInTerm envs x)
eqToInTerm envs (Minus x) = InMinus (eqToInTerm envs x)
eqToInTerm envs (x :+ y) = InAdd (eqToInTerm envs x) (eqToInTerm envs y)
eqToInTerm envs (x :* y) = InMult (eqToInTerm envs x) (eqToInTerm envs y)
eqToInTerm envs (x := y) = InEqual (eqToInTerm envs x) (eqToInTerm envs y)


showInTerm :: (Show a) => InTerm a -> String
showInTerm (EIdx (EnergyIdx s r x y)) = "E:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (NIdx (EtaIdx s r x y)) = "n:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DEIdx (DEnergyIdx s r x y)) = "dE:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DNIdx (DEtaIdx s r x y)) = "dn:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (ScaleIdx (XIdx s r x y)) = "x:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (VIdx (VarIdx s r x y)) = "v:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (SIdx (StorageIdx s r n)) = "s:" ++ show s ++ "." ++ show r ++ ":" ++ show n
showInTerm (InConst x) = take 20 (show x) ++ "..."
showInTerm (InGiven xs) = "given" ++ show xs
showInTerm (InMinus t) = "-(" ++ showInTerm t ++ ")"
showInTerm (InRecip t) = "1/(" ++ showInTerm t ++ ")"
showInTerm (InAdd s t) = "(" ++ showInTerm s ++ " + " ++ showInTerm t ++ ")"
showInTerm (InMult s t) = showInTerm s ++ " * " ++ showInTerm t
showInTerm (InEqual s t) = showInTerm s ++ " = " ++ showInTerm t

showInTerms :: (Show a) => [InTerm a] -> String
showInTerms ts = L.intercalate "\n" $ map showInTerm ts

interpretRhs :: (Arith a, Show a) => Envs a -> InTerm a -> a
interpretRhs _ (InConst xs) = xs
interpretRhs _ (InGiven xs) = xs
interpretRhs envs (EIdx idx) = energyMap envs M.! idx
interpretRhs envs (NIdx idx) = etaMap envs M.! idx
interpretRhs envs (DEIdx idx) = denergyMap envs M.! idx
interpretRhs envs (DNIdx idx) = detaMap envs M.! idx
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

interpretEq :: (Show a, Arith a) => Envs a -> InTerm a -> Envs a
interpretEq envs (InEqual (EIdx idx) rhs) = envs { energyMap = insert idx envs rhs (energyMap envs) }
interpretEq envs (InEqual (NIdx idx) rhs) = envs { etaMap = insert idx envs rhs (etaMap envs) }
interpretEq envs (InEqual (DEIdx idx) rhs) = envs { denergyMap = insert idx envs rhs (denergyMap envs) }
interpretEq envs (InEqual (DNIdx idx) rhs) = envs { detaMap = insert idx envs rhs (detaMap envs) }
interpretEq envs (InEqual (ScaleIdx idx) rhs) = envs { xMap = insert idx envs rhs (xMap envs) }
interpretEq envs (InEqual (VIdx idx) rhs) = envs { varMap = insert idx envs rhs (varMap envs) }
interpretEq envs (InEqual (SIdx idx) rhs) = envs { storageMap = insert idx envs rhs (storageMap envs) }


interpretFromScratch :: (Show a, Arith a) => [InTerm [a]] -> Envs [a]
interpretFromScratch ts = Envs (cut penv) (cut b) (cut c) (cut d) (cut e) (cut f) (cut g) (cut h) (cut i) (cut j)
  where Envs penv b c d e f g h i j = L.foldl' interpretEq emptyEnv ts
        minLen = minimum $ map snd $ M.toList (M.map length penv)
        cut = M.map (take minLen)
