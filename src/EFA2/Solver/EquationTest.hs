-- | test terms

module EFA2.Solver.EquationTest where

import EFA2.Solver.Equation (EqTerm(..), Equation(..))

import EFA2.Interpreter.Env (PowerIdx(..), DPowerIdx(..), FEtaIdx(..), DEtaIdx(..), Index(..))


p1 = Idx $ Power $ PowerIdx 0 0 0 1
p2 = Idx $ Power $ PowerIdx 0 0 0 2
p3 = Idx $ Power $ PowerIdx 0 0 0 3
p4 = Idx $ Power $ PowerIdx 0 0 0 4

dp1 = Idx $ DPower $ DPowerIdx 0 0 0 1
dp2 = Idx $ DPower $ DPowerIdx 0 0 0 2
dp3 = Idx $ DPower $ DPowerIdx 0 0 0 3
dp4 = Idx $ DPower $ DPowerIdx 0 0 0 4


c = Const 1.0

e = Idx $ FEta $ FEtaIdx 0 0 0 1
de = Idx $ DEta $ DEtaIdx 0 0 0 1

t =  p2 := FEdge p1 e

testt = de :* (Recip e) :* (Recip e)
