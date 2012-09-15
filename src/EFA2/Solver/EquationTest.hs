-- | test terms

module EFA2.Solver.EquationTest where

import EFA2.Solver.Equation

import EFA2.Interpreter.Env (PowerIdx(..), DPowerIdx(..), FEtaIdx(..), DEtaIdx(..))


p1 = Power (PowerIdx 0 0 0 1)
p2 = Power (PowerIdx 0 0 0 2)
p3 = Power (PowerIdx 0 0 0 3)
p4 = Power (PowerIdx 0 0 0 4)

dp1 = DPower (DPowerIdx 0 0 0 1)
dp2 = DPower (DPowerIdx 0 0 0 2)
dp3 = DPower (DPowerIdx 0 0 0 3)
dp4 = DPower (DPowerIdx 0 0 0 4)


c = Const 1.0

e = FEta (FEtaIdx 0 0 0 1)
de = DEta (DEtaIdx 0 0 0 1)

t =  p2 := FEdge p1 e

testt = de :* (Recip e) :* (Recip e)
