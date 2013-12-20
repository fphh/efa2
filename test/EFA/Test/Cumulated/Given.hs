module EFA.Test.Cumulated.Given where

import qualified EFA.Test.EquationSystem.Given as TestGiven
import qualified EFA.Test.EquationSystem as TestEqSys

import EFA.Example.Topology.Tripod (Node, node0, node1, node2, node3)

import qualified EFA.Flow.Cumulated.Absolute as EqSys
import qualified EFA.Flow.Cumulated.Quantity as CumFlow
import qualified EFA.Flow.Cumulated.Index as XIdx

import qualified EFA.Flow.Sequence.Quantity as SeqFlow

import qualified EFA.Graph as Graph

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Pair as Pair
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.RecordIndex as RecIdx
import EFA.Equation.Result (Result)

import EFA.Symbolic.SumProduct (Term)

import qualified EFA.Report.Format as Format

import qualified Control.Monad.Exception.Synchronous as ME

import Data.Tuple.HT (mapFst)
import Data.Monoid (mconcat, mempty)


type ResultGraph a = CumFlow.Graph Node (Result a)

cumGraph :: CumFlow.CumGraph Node (Result Rational)
cumGraph =
   CumFlow.fromSequenceFlowResult $ SeqFlow.sequence $
   TestEqSys.solveIncomplete TestGiven.partialEquations

flowGraphRatio :: ResultGraph Rational
flowGraphRatio =
   Graph.mapEdge CumFlow.flowResultFromCumResult cumGraph

flowGraph :: (Arith.Constant a) => ResultGraph a
flowGraph =
   CumFlow.mapGraph (fmap Arith.fromRational) flowGraphRatio


fullGraph, solvedGraph ::
   (ME.Exceptional
      (Verify.Exception Format.Unicode)
      (ResultGraph Rational),
    Verify.Assigns Format.Unicode)
fullGraph =
   mapFst (fmap numericGraph) $
   EqSys.solveTracked flowGraph fullGiven

solvedGraph =
   mapFst (fmap numericGraph) $
   EqSys.solveTracked (flowGraph :: ResultGraph Tracked) mempty

numericGraph ::
   ResultGraph (Pair.T at an) ->
   ResultGraph an
numericGraph =
   CumFlow.mapGraph (fmap Pair.second)


type Tracked = Pair.T (Verify.CumTerm Term RecIdx.Absolute Node) Rational

type EquationSystem s =
        EqSys.EquationSystem
           (Verify.Track Format.Unicode) Node s Tracked


infix 0 .=

(.=) ::
   (Arith.Constant a, Verify.LocalVar mode a, CumFlow.Lookup idx) =>
   idx Node -> Rational -> EqSys.EquationSystem mode Node s a
evar .= val  =
   evar EqSys..= Arith.fromRational val


fullGiven :: EquationSystem s
fullGiven = mconcat $
   (XIdx.outSum node0 .= 11643/98) :
   (XIdx.inSum node1 .= 951/28) :
   (XIdx.inSum node2 .= 2599/42) :
   (XIdx.outSum node2 .= 2599/42) :
   (XIdx.inSum node3 .= 10) :
   (XIdx.outSum node3 .= 22) :
   (XIdx.dTime node0 node2 .= 4) :
   (XIdx.outX node0 node2 .= 1) :
   (XIdx.outPower node0 node2 .= 11643/392) :
   (XIdx.outEnergy node0 node2 .= 11643/98) :
   (XIdx.eta node0 node2 .= 77147/174645) :
   (XIdx.inEnergy node0 node2 .= 11021/210) :
   (XIdx.inPower node0 node2 .= 11021/840) :
   (XIdx.inX node0 node2 .= 11021/12995) :
   (XIdx.dTime node2 node1 .= 4) :
   (XIdx.outX node2 node1 .= 1899/2599) :
   (XIdx.outPower node2 node1 .= 633/56) :
   (XIdx.outEnergy node2 node1 .= 633/14) :
   (XIdx.eta node2 node1 .= 317/422) :
   (XIdx.inEnergy node2 node1 .= 951/28) :
   (XIdx.inPower node2 node1 .= 951/112) :
   (XIdx.inX node2 node1 .= 1) :
   (XIdx.dTime node2 node3 .= 2) :
   (XIdx.outX node2 node3 .= 700/2599) :
   (XIdx.outPower node2 node3 .= 25/3) :
   (XIdx.outEnergy node2 node3 .= 50/3) :
   (XIdx.eta node2 node3 .= 3/5) :
   (XIdx.inEnergy node2 node3 .= 10) :
   (XIdx.inPower node2 node3 .= 5) :
   (XIdx.inX node2 node3 .= 1) :
   (XIdx.dTime node3 node2 .= 2) :
   (XIdx.outX node3 node2 .= 1) :
   (XIdx.outPower node3 node2 .= 11) :
   (XIdx.outEnergy node3 node2 .= 22) :
   (XIdx.eta node3 node2 .= 47/110) :
   (XIdx.inEnergy node3 node2 .= 47/5) :
   (XIdx.inPower node3 node2 .= 47/10) :
   (XIdx.inX node3 node2 .= 1974/12995) :
   []
