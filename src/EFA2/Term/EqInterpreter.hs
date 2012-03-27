

module EFA2.Term.EqInterpreter where

import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L

import Control.Monad.Error

import Debug.Trace

import EFA2.Graph.Graph
import EFA2.Term.Equation

import EFA2.Signal.SignalData
import EFA2.Signal.TH


--data NodeIdx = NodeIdx !Int deriving (Show, Ord, Eq)
--data EtaIdx = EtaIdx !Int !Int deriving  (Show)
--data PowerIdx = PowerIdx !Int !Int deriving (Show, Ord, Eq)
--data XIdx = XIdx !Int !Int deriving (Show, Ord, Eq)

data Abs
data Diff

data InTerm a = PIdx PowerIdx
              | EIdx EtaIdx
              | ScaleIdx XIdx
              | InMinus (InTerm a)
              | InRecip (InTerm a)
              | InAdd (InTerm a) (InTerm a)
              | InMult (InTerm a) (InTerm a)
              | InEqual (InTerm a) (InTerm a) deriving (Eq, Ord, Show)

showInTerm (PIdx (PowerIdx x y)) = "E_" ++ show x ++ "_" ++ show y
showInTerm (EIdx (EtaIdx x y)) = "n_" ++ show x ++ "_" ++ show y
showInTerm (ScaleIdx (XIdx x y)) = "x_" ++ show x ++ "_" ++ show y
showInTerm (InMinus t) = "-(" ++ showInTerm t ++ ")"
showInTerm (InRecip t) = "1/(" ++ showInTerm t ++ ")"
showInTerm (InAdd s t) = "(" ++ showInTerm s ++ " + " ++ showInTerm t ++ ")"
showInTerm (InMult s t) = showInTerm s ++ " * " ++ showInTerm t
showInTerm (InEqual s t) = showInTerm s ++ " = " ++ showInTerm t


showInTerms ts = L.intercalate "\n" $ map showInTerm ts


eqTermToInTerm :: (EdgeFormula a) => EqTerm -> InTerm a
eqTermToInTerm (Energy idx) = PIdx idx
eqTermToInTerm (Eta idx) = EIdx idx
eqTermToInTerm (X idx) = ScaleIdx idx
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


absInterpret :: PowerEnv [Val] -> EtaEnv [Val] -> XEnv [Val] -> InTerm Abs -> [Val]
absInterpret penv eenv xenv t = interpret t
  where interpret (PIdx idx) = penv idx
        interpret (EIdx idx) = eenv idx
        interpret (ScaleIdx idx) = xenv idx
        interpret (InMinus t) = map negate (interpret t)
        interpret (InRecip t) = map recip (interpret t)
        interpret (InAdd s t) = zipWith (+) (interpret s) (interpret t)
        interpret (InMult s t) = zipWith (*) (interpret s) (interpret t)


toInTerms :: (EdgeFormula a) => [EqTerm] -> [InTerm a]
toInTerms ts = map eqTermToInTerm (filter (not . isGiven) ts)

solveInTerms :: LRPowerEnv [Val] -> LREtaEnv [Val] -> LRXEnv [Val] -> [InTerm Abs] -> [(PowerIdx, [Val])]
solveInTerms penv eenv xenv ts = reverse $ snd $ L.foldl' f (penv, []) ts
  where eenv' = mkEnv eenv
        xenv' = mkEnv xenv
        f (pacc, sol) (InEqual (PIdx idx) t) = (composeEnv newPEnv pacc, (idx, val):sol)
          where val = absInterpret (mkEnv pacc) eenv' xenv' t
                newPEnv x | idx == x = return val
                newPEnv idx = throwError (PowerIdxError idx)