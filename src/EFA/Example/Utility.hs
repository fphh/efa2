module EFA.Example.Utility where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr
import EFA.Graph.Topology.StateAnalysis (bruteForce)

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.System as EqGen
import EFA.Equation.System ((=.=))
import EFA.Equation.Variable (MkIdxC, MkVarC, mkVar)
import qualified EFA.Equation.Variable as Var
import Data.Monoid ((<>))

import EFA.Signal.SequenceData (SequData(SequData))



{-
makeNode :: Int -> Idx.Node
makeNode = Idx.Node

makeNodes :: [(Int, TD.NodeType)] -> [Gr.LNode Idx.Node TD.NodeType]
makeNodes ns = map f ns
  where f (n, ty) = (makeNode n, ty)
-}

makeEdges :: [(nty, nty)] -> [Gr.LEdge nty ()]
makeEdges = map (\(a, b) -> (Gr.Edge a b, ()))

{-
makeSimpleEdges :: [(Int, Int)] -> [Gr.LEdge Idx.Node ()]
makeSimpleEdges es = map f es
  where f (a, b) = (Gr.Edge (Idx.Node a) (Idx.Node b), ())
-}

constructSeqTopo ::
  (Ord nty) =>
  TD.Topology nty -> [Int] -> TD.SequFlowGraph nty
constructSeqTopo topo states = mkSeqTopo (select sol states)
  where sol = bruteForce topo
        select ts = map (ts!!)
        mkSeqTopo = Flow.mkSequenceTopology
                    . Flow.genSectionTopology
                    . SequData

recAbs :: Idx.Record
recAbs = EqGen.recAbs


selfAssign ::
  (Eq (term (Var.Index nty)), Ord (t nty), MkVarC term, MkIdxC t,
   Env.AccessMap t) =>
  t nty -> EqGen.EquationSystem nty s (term (Var.Index nty))
selfAssign idx =
   EqGen.getVar idx .= mkVar idx

infixr 6 =<>
(=<>) ::
  ( Eq (term (Var.Index nty)), Ord (t nty), Env.AccessMap t,
    MkVarC term, MkIdxC t) =>
  t nty
  -> EqGen.EquationSystem nty s (term (Var.Index nty))
  -> EqGen.EquationSystem nty s (term (Var.Index nty))
idx =<> eqsys = selfAssign idx <> eqsys


edgeVar ::
   (Idx.SecNode nty -> Idx.SecNode nty -> idx) ->
   Idx.Section -> nty -> nty -> idx
edgeVar idx sec x y =
   idx
      (Idx.SecNode sec x)
      (Idx.SecNode sec y)

interVar ::
   (Idx.SecNode nty -> Idx.SecNode nty -> idx) ->
   Idx.Section -> Idx.Section -> nty -> idx
interVar idx sec0 sec1 x =
   idx
      (Idx.SecNode sec0 x)
      (Idx.SecNode sec1 x)


infix 0 .=

(.=) ::
  (Eq a) =>
  EqGen.ExprWithVars nty s a -> a -> EqGen.EquationSystem nty s a
evar .= val  =  evar =.= EqGen.constToExprSys val
