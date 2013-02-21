-- | test terms

module EFA.Symbolic.OperatorTreeTest where

import EFA.Equation.Variable (Index(..), MkIdxC, mkVar, MkVarC)
import EFA.Symbolic.OperatorTree (Term(..))

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology.Index as Idx


deflt ::
  (MkVarC term, MkIdxC t) =>
  (rec ->
   Idx.SecNode Node.Int ->
   Idx.SecNode Node.Int ->
   t rec a) ->
  rec ->
  Int -> Int -> term (Index rec a)
deflt f r x y =
   mkVar $ f r
             (Idx.SecNode (Idx.Section 0) (Node.Int x))
             (Idx.SecNode (Idx.Section 0) (Node.Int y))

absolute ::
  (MkVarC term, MkIdxC t) =>
  (Idx.Absolute ->
   Idx.SecNode Node.Int ->
   Idx.SecNode Node.Int ->
   t Idx.Absolute a) ->
  Int -> Int -> term (Index Idx.Absolute a)
absolute f = deflt f Idx.Absolute

delta ::
  (MkVarC term, MkIdxC t) =>
  (Idx.Delta ->
   Idx.SecNode Node.Int ->
   Idx.SecNode Node.Int ->
   t Idx.Delta a) ->
  Int -> Int -> term (Index Idx.Delta a)
delta f = deflt f Idx.Delta


p1, p2, p3, p4 :: Term (Index Idx.Absolute Node.Int)
p1 = absolute Idx.Power 0 1
p2 = absolute Idx.Power 0 2
p3 = absolute Idx.Power 0 3
p4 = absolute Idx.Power 0 4

dp1, dp2, dp3, dp4 :: Term (Index Idx.Delta Node.Int)
dp1 = delta Idx.Power 0 1
dp2 = delta Idx.Power 0 2
dp3 = delta Idx.Power 0 3
dp4 = delta Idx.Power 0 4


c :: Term a
c = Const 1.0

e :: Term (Index Idx.Absolute Node.Int)
e = absolute Idx.Eta 0 1

de :: Term (Index Idx.Delta Node.Int)
de = delta Idx.Eta 0 1
