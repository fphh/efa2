{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}

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


class ToConst a where
      toConst :: [Val] -> a
      toShow :: a -> String

instance ToConst [Val] where
         toConst = id
         toShow xs = "[" ++ L.intercalate ", " (map show $ take 3 xs) ++ ", ...]"


eqToInTerm :: (Arith a, ToConst a) => Envs a -> EqTerm -> InTerm a
eqToInTerm envs (Const x) = InConst (toConst $ cst x)
eqToInTerm envs (Power idx := Given) = InEqual (PIdx idx) (InGiven (powerMap envs M.! idx))
eqToInTerm envs (Eta idx := Given) = InEqual (EIdx idx) (InGiven (etaMap envs M.! idx))
eqToInTerm envs (DPower idx := Given) = InEqual (DPIdx idx) (InGiven (dpowerMap envs M.! idx))
eqToInTerm envs (DEta idx := Given) = InEqual (DEIdx idx) (InGiven (detaMap envs M.! idx))
eqToInTerm envs (X idx := Given) = InEqual (ScaleIdx idx) (InGiven (xMap envs M.! idx))
eqToInTerm envs (Var idx := Given) = InEqual (VIdx idx) (InGiven (varMap envs M.! idx))
eqToInTerm _ (Power idx) = PIdx idx
eqToInTerm _ (Eta idx) = EIdx idx
eqToInTerm _ (DPower idx) = DPIdx idx
eqToInTerm _ (DEta idx) = DEIdx idx
eqToInTerm _ (X idx) = ScaleIdx idx
eqToInTerm _ (Var idx) = VIdx idx
eqToInTerm envs (Recip x) = InRecip (eqToInTerm envs x)
eqToInTerm envs (Minus x) = InMinus (eqToInTerm envs x)
eqToInTerm envs (x :+ y) = InAdd (eqToInTerm envs x) (eqToInTerm envs y)
eqToInTerm envs (x :* y) = InMult (eqToInTerm envs x) (eqToInTerm envs y)
eqToInTerm envs (x := y) = InEqual (eqToInTerm envs x) (eqToInTerm envs y)


showInTerm :: (ToConst a, Show a) => InTerm a -> String
showInTerm (PIdx (PowerIdx s r x y)) = "E:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (EIdx (EtaIdx s r x y)) = "n:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DPIdx (DPowerIdx s r x y)) = "dE:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DEIdx (DEtaIdx s r x y)) = "dn:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (ScaleIdx (XIdx s r x y)) = "x:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (InConst x) = toShow x
showInTerm (InGiven xs) = "given" ++ show xs
showInTerm (InMinus t) = "-(" ++ showInTerm t ++ ")"
showInTerm (InRecip t) = "1/(" ++ showInTerm t ++ ")"
showInTerm (InAdd s t) = "(" ++ showInTerm s ++ " + " ++ showInTerm t ++ ")"
showInTerm (InMult s t) = showInTerm s ++ " * " ++ showInTerm t
showInTerm (InEqual s t) = showInTerm s ++ " = " ++ showInTerm t

showInTerms :: (ToConst a, Show a) => [InTerm a] -> String
showInTerms ts = L.intercalate "\n" $ map showInTerm ts


emptyEnv :: Envs a
emptyEnv = Envs M.empty M.empty M.empty M.empty M.empty M.empty

interpretLhs :: (Arith a, Show a) => Envs a -> InTerm a -> a
interpretLhs _ (InConst xs) = xs
interpretLhs _ (InGiven xs) = xs
interpretLhs envs (PIdx idx) = powerMap envs M.! idx
interpretLhs envs (EIdx idx) = etaMap envs M.! idx
interpretLhs envs (DPIdx idx) = dpowerMap envs M.! idx
interpretLhs envs (DEIdx idx) = detaMap envs M.! idx
interpretLhs envs (ScaleIdx idx) = xMap envs M.! idx
interpretLhs envs (VIdx idx) = varMap envs M.! idx
interpretLhs envs (InMinus t) = neg (interpretLhs envs t)
interpretLhs envs (InRecip t) = rec (interpretLhs envs t)
interpretLhs envs (InAdd s t) = (interpretLhs envs s) .+ (interpretLhs envs t)
interpretLhs envs (InMult s t) = (interpretLhs envs s) .* (interpretLhs envs t)
interpretLhs _ t = error (show t)

insert :: (Arith a, Show a, Ord k) => k -> Envs a -> InTerm a -> M.Map k a -> M.Map k a
insert idx envs lhs m = M.insert idx (interpretLhs envs lhs) m

interpretEq :: (Show a, Arith a) => Envs a -> InTerm a -> Envs a
interpretEq envs (InEqual (PIdx idx) lhs) = envs { powerMap = insert idx envs lhs (powerMap envs) }
interpretEq envs (InEqual (EIdx idx) lhs) = envs { etaMap = insert idx envs lhs (etaMap envs) }
interpretEq envs (InEqual (DPIdx idx) lhs) = envs { dpowerMap = insert idx envs lhs (dpowerMap envs) }
interpretEq envs (InEqual (DEIdx idx) lhs) = envs { detaMap = insert idx envs lhs (detaMap envs) }
interpretEq envs (InEqual (ScaleIdx idx) lhs) = envs { xMap = insert idx envs lhs (xMap envs) }
interpretEq envs (InEqual (VIdx idx) lhs) = envs { varMap = insert idx envs lhs (varMap envs) }


interpretFromScratch :: (Num a, Show a, Arith a) => [InTerm [a]] -> Envs [a]
interpretFromScratch ts = Envs (cut penv) (cut b) (cut c) (cut d) (cut e) (cut f)
  where Envs penv b c d e f = L.foldl' interpretEq emptyEnv ts
        minLen = minimum $ map snd $ M.toList (M.map length penv)
        cut = M.map (take minLen)
