-- | test terms

module EFA2.Solver.EquationTest where

import EFA2.Solver.Equation (EqTerm, Term(..), Equation(..), (!/), (&/), MkIdxC, mkVar)

import qualified EFA2.Signal.Index as Idx
import EFA2.Interpreter.Env (Index(..))


deflt ::
   (MkIdxC a) =>
   (Idx.Record -> Idx.SecNode -> Idx.SecNode -> a) ->
   Int -> Int -> Index
deflt mkIdx x y =
   mkVar $ mkIdx (Idx.Record Idx.Absolute)
      (Idx.SecNode (Idx.Section 0) (Idx.Node x))
      (Idx.SecNode (Idx.Section 0) (Idx.Node y))

p1, p2, p3, p4 :: Index
p1 = deflt Idx.Power 0 1
p2 = deflt Idx.Power 0 2
p3 = deflt Idx.Power 0 3
p4 = deflt Idx.Power 0 4

dp1, dp2, dp3, dp4 :: Index
dp1 = deflt Idx.DPower 0 1
dp2 = deflt Idx.DPower 0 2
dp3 = deflt Idx.DPower 0 3
dp4 = deflt Idx.DPower 0 4


c :: Term a
c = Const 1.0

e, de :: Index
e = deflt Idx.FEta 0 1
de = deflt Idx.DEta 0 1

t :: Equation
t = EqEdge p1 e p2

testt :: EqTerm
testt = de !/ Atom e &/ Atom e
