

module EFA2.Term.EqInterpreter where

import Data.Maybe
import qualified Data.Map as M

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
              | InMult (InTerm a) (InTerm a) deriving (Eq, Ord, Show)


eqTermToInTerm :: (EdgeFormula a) => EqTerm -> InTerm a
eqTermToInTerm (Energy x y) = PIdx (mkPowerIdx x y)
eqTermToInTerm (Eta x y) = EIdx (mkEtaIdx x y)
eqTermToInTerm (X x y) = ScaleIdx (mkXIdx x y)
eqTermToInTerm (Given t) = eqTermToInTerm t
eqTermToInTerm (Minus t) = InMinus (eqTermToInTerm t)
eqTermToInTerm (Recip t) = InRecip (eqTermToInTerm t)
eqTermToInTerm (Add s t) = InAdd (eqTermToInTerm s) (eqTermToInTerm t)
eqTermToInTerm (Mult s t) = InMult (eqTermToInTerm s) (eqTermToInTerm t)
eqTermToInTerm t@(F _) = toEdgeFormula t
eqTermToInTerm t@(B _) = toEdgeFormula t
eqTermToInTerm t = error ("bad term: " ++ toString t)

class EdgeFormula a where
      toEdgeFormula :: EqTerm -> InTerm a


instance EdgeFormula Abs where
         toEdgeFormula (F (Energy x y)) = InMult (eqTermToInTerm (Eta x y)) (eqTermToInTerm (Energy x y))
         toEdgeFormula (B (Energy x y)) = InMult (eqTermToInTerm (Recip (Eta x y))) (eqTermToInTerm (Energy x y))


absInterpret :: PowerEnv [Val] -> EtaEnv [Val] -> XEnv [Val] -> InTerm Abs -> [Val]
absInterpret penv eenv xenv t = interpret t
  where interpret (PIdx idx) = penv idx
        interpret (EIdx idx) = eenv idx
        interpret (ScaleIdx idx) = xenv idx
        interpret (InMinus t) = map negate (interpret t)
        interpret (InRecip t) = map recip (interpret t)
        interpret (InAdd s t) = zipWith (+) (interpret s) (interpret t)
        interpret (InMult s t) = zipWith (*) (interpret s) (interpret t)

