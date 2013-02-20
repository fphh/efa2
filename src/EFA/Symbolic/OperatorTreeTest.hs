-- | test terms

module EFA.Symbolic.OperatorTreeTest where

import EFA.Equation.Variable (Index(..), MkIdxC, mkVar, MkVarC)
import EFA.Symbolic.OperatorTree (Term(..))

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology.Index as Idx


deflt ::
  (MkVarC term, MkIdxC t) =>
  (Idx.Record -> Idx.SecNode Node.Int -> Idx.SecNode Node.Int -> t a)
  -> Int -> Int -> term (Index a)
deflt f x y =
   mkVar $ f Idx.recAbs
             (Idx.SecNode (Idx.Section 0) (Node.Int x))
             (Idx.SecNode (Idx.Section 0) (Node.Int y))


p1, p2, p3, p4 :: Term (Index Node.Int)
p1 = deflt Idx.Power 0 1
p2 = deflt Idx.Power 0 2
p3 = deflt Idx.Power 0 3
p4 = deflt Idx.Power 0 4

dp1, dp2, dp3, dp4 :: Term (Index Node.Int)
dp1 = deflt Idx.DPower 0 1
dp2 = deflt Idx.DPower 0 2
dp3 = deflt Idx.DPower 0 3
dp4 = deflt Idx.DPower 0 4


c :: Term a
c = Const 1.0

e, de :: Term (Index Node.Int)
e = deflt Idx.Eta 0 1
de = deflt Idx.DEta 0 1
