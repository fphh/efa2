{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}

module EFA2.Solver.EqInterpreter where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.Error

import Debug.Trace

import EFA2.Solver.TermData
import EFA2.Solver.Env
import EFA2.Solver.Equation
import EFA2.Signal.Arith



showInTerm :: InTerm -> String
showInTerm (PIdx (PowerIdx s r x y)) = "E:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (EIdx (EtaIdx s r x y)) = "n:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DPIdx (DPowerIdx s r x y)) = "dE:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (DEIdx (DEtaIdx s r x y)) = "dn:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (ScaleIdx (XIdx s r x y)) = "x:" ++ show s ++ "." ++ show r ++ ":" ++ show x ++ "." ++ show y
showInTerm (InConst x) = show x
showInTerm (InRConst x) = show x

showInTerm (InMinus t) = "-(" ++ showInTerm t ++ ")"
showInTerm (InRecip t) = "1/(" ++ showInTerm t ++ ")"
showInTerm (InAdd s t) = "(" ++ showInTerm s ++ " + " ++ showInTerm t ++ ")"
showInTerm (InMult s t) = showInTerm s ++ " * " ++ showInTerm t
showInTerm (InEqual s t) = showInTerm s ++ " = " ++ showInTerm t

showInTerms :: [InTerm] -> String
showInTerms ts = L.intercalate "\n" $ map showInTerm ts


data Envs a = Envs { powerMap :: PowerMap a,
                     etaMap :: EtaMap a,
                     dpowerMap :: DPowerMap a,
                     detaMap :: DEtaMap a,
                     xMap :: XMap a,
                     varMap :: VarMap a } deriving (Show)

emptyEnv :: Envs a
emptyEnv = Envs M.empty M.empty M.empty M.empty M.empty M.empty

interpretLhs :: (Arith a) => Envs a -> EqTerm -> a
interpretLhs envs (Given xs) = xs :: [a]
interpretLhs envs (Power idx) = powerMap envs M.! idx
interpretLhs envs (Eta idx) = etaMap envs M.! idx
interpretLhs envs (DPower idx) = dpowerMap envs M.! idx
interpretLhs envs (DEta idx) = detaMap envs M.! idx
interpretLhs envs (X idx) = xMap envs M.! idx
interpretLhs envs (Var idx) = varMap envs M.! idx
interpretLhs envs (Minus t) = neg (interpretLhs envs t)
interpretLhs envs (Recip t) = rec (interpretLhs envs t)
interpretLhs envs (s :+ t) = (interpretLhs envs s) .+ (interpretLhs envs t)
interpretLhs envs (s :* t) = (interpretLhs envs s) .* (interpretLhs envs t)
interpretLhs _ t = error (show t)


interpretEq :: (Arith a) => Envs a -> EqTerm -> Envs a
interpretEq envs (Power idx := lhs) = envs { powerMap = M.insert idx (interpretLhs envs lhs) (powerMap envs) }
interpretEq envs (Eta idx := lhs) = envs { etaMap = M.insert idx (interpretLhs envs lhs) (etaMap envs) }
interpretEq envs (DPower idx := lhs) = envs { dpowerMap = M.insert idx (interpretLhs envs lhs) (dpowerMap envs) }
interpretEq envs (DEta idx := lhs) = envs { detaMap = M.insert idx (interpretLhs envs lhs) (detaMap envs) }
interpretEq envs (X idx := lhs) = envs { xMap = M.insert idx (interpretLhs envs lhs) (xMap envs) }
interpretEq envs (Var idx := lhs) = envs { varMap = M.insert idx (interpretLhs envs lhs) (varMap envs) }


interpretFromScratch :: (Arith a) => [EqTerm] -> Envs a
interpretFromScratch ts = L.foldl' interpretEq emptyEnv ts






