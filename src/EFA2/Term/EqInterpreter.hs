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


eqTermToInTerm :: (EdgeFormula a) => EqTerm a -> [InTerm]
eqTermToInTerm (Energy idx) = [PIdx idx]
eqTermToInTerm (Eta idx) = [EIdx idx]
eqTermToInTerm (X idx) = [ScaleIdx idx]
eqTermToInTerm (Const x) = [InRConst x]
eqTermToInTerm (Minus t) = map InMinus (eqTermToInTerm t)
eqTermToInTerm (Recip t) = map InRecip (eqTermToInTerm t)
eqTermToInTerm (Add s t) = addf [eqTermToInTerm s, eqTermToInTerm t]
eqTermToInTerm (Mult s t) = multf [eqTermToInTerm s, eqTermToInTerm t]
eqTermToInTerm t@(F _) = toEdgeFormula t
eqTermToInTerm t@(B _) = toEdgeFormula t
eqTermToInTerm (s := t) = [InEqual (L.foldl1' InAdd $ eqTermToInTerm s) (L.foldl1' InAdd $ eqTermToInTerm t)]
eqTermToInTerm t = error ("Bad term: " ++ showEqTerm t)

addf :: [[InTerm]] -> [InTerm]
addf xs = concat xs

multf :: [[InTerm]] -> [InTerm]
multf xs = map fgg (sequence xs)
  where fgg ys = L.foldl1' InMult ys


class EdgeFormula a where
      toEdgeFormula :: EqTerm a -> [InTerm]

instance EdgeFormula Abs where
         toEdgeFormula (F (Energy idx@(PowerIdx x y))) = multf [eqTermToInTerm s, eqTermToInTerm t]
           where s :: EqTerm Abs  -- why cant we derive this type?
                 s = Eta (EtaIdx x y)
                 t :: EqTerm Abs
                 t = Energy idx

         toEdgeFormula (B (Energy idx@(PowerIdx x y))) = multf [eqTermToInTerm s, eqTermToInTerm t]
           where s :: EqTerm Abs
                 s = Recip (Eta (EtaIdx x y))
                 t :: EqTerm Abs
                 t = Energy idx

{-
instance EdgeFormula Diff where
         toEdgeFormula (F x) = InConst 1.0
         toEdgeFormula (B x) = InMult (InConst 20.0) (InConst 40.0)
-}

-- interpreter for terms with only constants
constInterpret :: [InTerm] -> [Val]
constInterpret ts = map interpret ts
  where interpret (InConst x) = x
        interpret (InMinus t) = negate (interpret t)
        interpret (InRecip t) = recip (interpret t)
        interpret (InAdd s t) = interpret s + interpret t
        interpret (InMult s t) = interpret s + interpret t

instance (Arith a) => Interpreter [a] where
         interpret penv eenv xenv t = interpret' t
           where interpret' (PIdx idx) = penv idx
                 interpret' (EIdx idx) = eenv idx
                 interpret' (ScaleIdx idx) = xenv idx
                 interpret' (InConst x) = [cst x]
                 interpret' (InRConst x) = repeat (cst x)      -- This constants can only be multiplied with finite lists.
                 interpret' (InMinus t) = (interpret' t)
                 interpret' (InRecip t) = map rec (interpret' t)
                 interpret' (InAdd s t) = zipWith (.+) (interpret' s) (interpret' t)
                 interpret' (InMult s t) = zipWith (.*) (interpret' s) (interpret' t)

toInTerms :: (EdgeFormula a) => [EqTerm a] -> [InTerm]
toInTerms ts = concatMap eqTermToInTerm (filter (not . isGiven) ts)
