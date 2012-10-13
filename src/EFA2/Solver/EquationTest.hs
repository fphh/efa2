-- | test terms

module EFA2.Solver.EquationTest where

import EFA2.Solver.Equation (Term(..), Equation(..))

import EFA2.Interpreter.Env (PowerIdx(..), DPowerIdx(..), FEtaIdx(..), DEtaIdx(..), Index(..))


p1 = Atom $ Power $ PowerIdx 0 0 0 1
p2 = Atom $ Power $ PowerIdx 0 0 0 2
p3 = Atom $ Power $ PowerIdx 0 0 0 3
p4 = Atom $ Power $ PowerIdx 0 0 0 4

dp1 = Atom $ DPower $ DPowerIdx 0 0 0 1
dp2 = Atom $ DPower $ DPowerIdx 0 0 0 2
dp3 = Atom $ DPower $ DPowerIdx 0 0 0 3
dp4 = Atom $ DPower $ DPowerIdx 0 0 0 4


c = Const 1.0

e = Atom $ FEta $ FEtaIdx 0 0 0 1
de = Atom $ DEta $ DEtaIdx 0 0 0 1

t =  p2 := FEdge p1 e

testt = de :* (Recip e) :* (Recip e)
