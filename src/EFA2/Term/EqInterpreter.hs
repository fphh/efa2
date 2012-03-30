{-# LANGUAGE FlexibleInstances #-}

module EFA2.Term.EqInterpreter where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.Error

import Debug.Trace

import EFA2.Graph.GraphData
import EFA2.Graph.Graph
import EFA2.Term.Equation
import EFA2.Term.TermData

--import EFA2.Signal.SignalData
import EFA2.Signal.TH

import EFA2.Term.TermData


showInTerm :: InTerm a -> String
showInTerm (PIdx (PowerIdx x y)) = "E_" ++ show x ++ "_" ++ show y
showInTerm (EIdx (EtaIdx x y)) = "n_" ++ show x ++ "_" ++ show y
showInTerm (ScaleIdx (XIdx x y)) = "x_" ++ show x ++ "_" ++ show y
showInTerm (InConst x) = show x
showInTerm (InRConst x) = show x

showInTerm (InMinus t) = "-(" ++ showInTerm t ++ ")"
showInTerm (InRecip t) = "1/(" ++ showInTerm t ++ ")"
showInTerm (InAdd s t) = "(" ++ showInTerm s ++ " + " ++ showInTerm t ++ ")"
showInTerm (InMult s t) = showInTerm s ++ " * " ++ showInTerm t
showInTerm (InEqual s t) = showInTerm s ++ " = " ++ showInTerm t

showInTerms :: [InTerm a] -> String
showInTerms ts = L.intercalate "\n" $ map showInTerm ts


eqTermToInTerm :: (EdgeFormula a) => EqTerm -> InTerm a
eqTermToInTerm (Energy idx) = PIdx idx
eqTermToInTerm (Eta idx) = EIdx idx
eqTermToInTerm (X idx) = ScaleIdx idx
eqTermToInTerm (Const x) = InRConst x
eqTermToInTerm (Minus t) = InMinus (eqTermToInTerm t)
eqTermToInTerm (Recip t) = InRecip (eqTermToInTerm t)
eqTermToInTerm (Add s t) = InAdd (eqTermToInTerm s) (eqTermToInTerm t)
eqTermToInTerm (Mult s t) = InMult (eqTermToInTerm s) (eqTermToInTerm t)
eqTermToInTerm t@(F _) = toEdgeFormula t
eqTermToInTerm t@(B _) = toEdgeFormula t
eqTermToInTerm (s := t) = InEqual (eqTermToInTerm s) (eqTermToInTerm t)
eqTermToInTerm t = error ("Bad term: " ++ showEqTerm t)

class EdgeFormula a where
      toEdgeFormula :: EqTerm -> InTerm a

instance EdgeFormula Abs where
         toEdgeFormula (F (Energy idx@(PowerIdx x y))) = InMult (eqTermToInTerm (Eta (EtaIdx x y))) 
                                                                (eqTermToInTerm (Energy idx))
         toEdgeFormula (B (Energy idx@(PowerIdx x y))) = InMult (eqTermToInTerm (Recip (Eta (EtaIdx x y))))
                                                                (eqTermToInTerm (Energy idx))

instance EdgeFormula Diff where
         toEdgeFormula (F x) = undefined -- InConst 1.0
         toEdgeFormula (B x) = undefined -- InMult (InConst 2.0) (InConst 4.0)

class Interpreter a where
      interpret :: PowerEnv a -> EtaEnv a -> XEnv a -> InTerm b -> a

instance (Arith a) => Interpreter [a] where
         interpret = valInterpret


valInterpret :: (Arith a) => PowerEnv [a] -> EtaEnv [a] -> XEnv [a] -> InTerm b -> [a]
valInterpret penv eenv xenv t = interpret t
  where interpret (PIdx idx) = penv idx
        interpret (EIdx idx) = eenv idx
        interpret (ScaleIdx idx) = xenv idx
        interpret (InConst x) = [cst x]
        interpret (InRConst x) = repeat (cst x)      -- Constants can only be multiplied with finite lists.
        interpret (InMinus t) = map neg (interpret t)
        interpret (InRecip t) = map rec (interpret t)
        interpret (InAdd s t) = zipWith (.+) (interpret s) (interpret t)
        interpret (InMult s t) = zipWith (.*) (interpret s) (interpret t)

toInTerms :: (EdgeFormula a) => [EqTerm] -> [InTerm a]
toInTerms ts = map eqTermToInTerm (filter (not . isGiven) ts)

solveInTerms :: (Interpreter a, EnvClass a) => (M.Map PowerIdx a) -> LREtaEnv a -> LRXEnv a -> [InTerm b] -> M.Map PowerIdx a
solveInTerms penv eenv xenv ts = M.fromList $ snd $ L.foldl' f (penv', []) ts
  where eenv' = mkEnv eenv
        xenv' = mkEnv xenv
        penv' = mkPowerEnv penv
        f (pacc, sol) (InEqual (PIdx idx) t) = (newPEnv `composeLREnv` pacc, (idx, val):sol)
          where val = interpret (mkEnv pacc) eenv' xenv' t
                newPEnv x | idx == x = return val
                newPEnv idx = throwError (PowerIdxError idx)