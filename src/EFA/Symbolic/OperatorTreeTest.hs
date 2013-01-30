-- | test terms

module EFA.Symbolic.OperatorTreeTest where

import EFA.Equation.Variable (Index(..), MkIdxC, mkVar, MkVarC)
import EFA.Symbolic.OperatorTree (Term(..))

import EFA.Graph.Topology.Nodes (Nodes(..))

import qualified EFA.Graph.Topology.Index as Idx


deflt ::
  (MkVarC term, MkIdxC t) =>
  (Idx.Record -> Idx.SecNode Nodes -> Idx.SecNode Nodes -> t a)
  -> Int -> Int -> term (Index a)
deflt f x y =
   mkVar $ f (Idx.Record Idx.Absolute)
             (Idx.SecNode (Idx.Section 0) (Node x))
             (Idx.SecNode (Idx.Section 0) (Node y))


p1, p2, p3, p4 :: Term (Index Nodes)
p1 = deflt Idx.Power 0 1
p2 = deflt Idx.Power 0 2
p3 = deflt Idx.Power 0 3
p4 = deflt Idx.Power 0 4

dp1, dp2, dp3, dp4 :: Term (Index Nodes)
dp1 = deflt Idx.DPower 0 1
dp2 = deflt Idx.DPower 0 2
dp3 = deflt Idx.DPower 0 3
dp4 = deflt Idx.DPower 0 4


c :: Term a
c = Const 1.0

e, de :: Term (Index Nodes)
e = deflt Idx.Eta 0 1
de = deflt Idx.DEta 0 1