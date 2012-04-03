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
showInTerm (PIdx (PowerIdx x y)) = "E." ++ show x ++ "." ++ show y
showInTerm (EIdx (EtaIdx x y)) = "n." ++ show x ++ "." ++ show y
showInTerm (DPIdx (DPowerIdx x y)) = "dE." ++ show x ++ "." ++ show y
showInTerm (DEIdx (DEtaIdx x y)) = "dn." ++ show x ++ "." ++ show y
showInTerm (ScaleIdx (XIdx x y)) = "x." ++ show x ++ "." ++ show y
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
eqTermToInTerm (DEnergy idx) = [DPIdx idx]
eqTermToInTerm (DEta idx) = [DEIdx idx]
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


instance EdgeFormula Diff where
         toEdgeFormula (F e@(Energy idx@(PowerIdx x y))) = eqTermToInTerm (Mult de dn) 
                                                           ++ multf [eqTermToInTerm e, eqTermToInTerm n]
                                                           ++ multf [eqTermToInTerm e, eqTermToInTerm dn]
           where n :: EqTerm Diff
                 n = Eta (EtaIdx x y)
                 de :: EqTerm Diff
                 de = DEnergy (DPowerIdx x y)
                 dn :: EqTerm Diff
                 dn = DEta (DEtaIdx x y)


         toEdgeFormula (B x) = [InMult (InConst 20.0) (InConst 40.0)]


interpret :: (Arith a) => DPowerEnv [a] -> DEtaEnv [a] -> EtaEnv [a] -> XEnv [a] -> PowerEnv [a] -> InTerm -> [a]
interpret dpenv deenv eenv xenv penv t = go t
  where go (PIdx idx) = penv idx
        go (EIdx idx) = eenv idx
        go (DPIdx idx) = dpenv idx
        go (DEIdx idx) = deenv idx
        go (ScaleIdx idx) = xenv idx
        go (InConst x) = [cst x]
        go (InRConst x) = repeat (cst x)   -- This constants can only be multiplied with finite lists.
        go (InMinus t) = (go t)
        go (InRecip t) = map rec (go t)
        go (InAdd s t) = zipWith (.+) (go s) (go t)
        go (InMult s t) = zipWith (.*) (go s) (go t)

toInTerms :: (EdgeFormula a) => [EqTerm a] -> [InTerm]
toInTerms ts = concatMap eqTermToInTerm (filter (not . isGiven) ts)
