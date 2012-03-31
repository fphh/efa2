{-# LANGUAGE FlexibleInstances #-}

module EFA2.Term.EqInterpreter where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.Error

import Debug.Trace

import EFA2.Graph.GraphData
import EFA2.Term.Equation
import EFA2.Term.TermData
import EFA2.Signal.Arith



showInTerm :: InTerm -> String
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

showInTerms :: [InTerm] -> String
showInTerms ts = L.intercalate "\n" $ map showInTerm ts


eqTermToInTerm :: (EdgeFormula a) => EqTerm a -> InTerm
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
      toEdgeFormula :: EqTerm a -> InTerm

instance EdgeFormula Abs where
         toEdgeFormula (F (Energy idx@(PowerIdx x y))) = InMult (eqTermToInTerm s) (eqTermToInTerm t)
           where s :: EqTerm Abs  -- why cant we derive this type?
                 s = Eta (EtaIdx x y)
                 t :: EqTerm Abs
                 t = Energy idx

         toEdgeFormula (B (Energy idx@(PowerIdx x y))) = InMult (eqTermToInTerm s) (eqTermToInTerm t)
           where s :: EqTerm Abs
                 s = Recip (Eta (EtaIdx x y))
                 t :: EqTerm Abs
                 t = Energy idx



instance EdgeFormula Diff where
         toEdgeFormula (F x) = InConst 1.0
         toEdgeFormula (B x) = InMult (InConst 20.0) (InConst 40.0)

instance (Arith a) => Interpreter [a] where
         interpret = valInterpret


valInterpret :: (Arith a) => PowerEnv [a] -> EtaEnv [a] -> XEnv [a] -> InTerm -> [a]
valInterpret penv eenv xenv t = interpret t
  where interpret (PIdx idx) = penv idx
        interpret (EIdx idx) = eenv idx
        interpret (ScaleIdx idx) = xenv idx
        interpret (InConst x) = [cst x]
        interpret (InRConst x) = repeat (cst x)      -- This constants can only be multiplied with finite lists.
        interpret (InMinus t) = map neg (interpret t)
        interpret (InRecip t) = map rec (interpret t)
        interpret (InAdd s t) = zipWith (.+) (interpret s) (interpret t)
        interpret (InMult s t) = zipWith (.*) (interpret s) (interpret t)

toInTerms :: (EdgeFormula a) => [EqTerm a] -> [InTerm]
toInTerms ts = map eqTermToInTerm (filter (not . isGiven) ts)
