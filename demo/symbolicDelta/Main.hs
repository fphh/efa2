module Main where

import qualified EFA.Example.Topology.Tripod as Tripod
import EFA.Example.Topology.Tripod (Node, node0, node1, node2, node3)

import qualified EFA.Flow.Sequence.Symbolic as Symbolic
import EFA.Flow.Sequence.Symbolic ((=<>), (.=))
import EFA.Application.Utility (seqFlowGraphFromStates, dirEdge)

import qualified EFA.Flow.Sequence.EquationSystem as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import EFA.Equation.Arithmetic (zero)

import Data.Monoid (mempty, (<>))


sec0 :: Idx.Section
sec0 = Idx.section0


{-
Use SumProduct.Term here since it simplifies automatically.
-}
given ::
   Symbolic.EquationSystem Symbolic.Ignore
      Record.Delta Node s SumProduct.Term
given =
   (RecIdx.delta (XIdx.dTime sec0) .= zero) <>

   RecIdx.before (XIdx.dTime sec0) =<>

   RecIdx.before (XIdx.storage Idx.initial node3) =<>
   RecIdx.delta (XIdx.storage (Idx.afterSection sec0) node3) =<>

   RecIdx.after (XIdx.power sec0 node3 node2) =<>
   RecIdx.after (XIdx.x sec0 node2 node3) =<>

   RecIdx.before (XIdx.power sec0 node3 node2) =<>
   RecIdx.before (XIdx.x sec0 node2 node3) =<>

   RecIdx.before (XIdx.eta sec0 node3 node2) =<>
   RecIdx.before (XIdx.eta sec0 node0 node2) =<>
   RecIdx.before (XIdx.eta sec0 node2 node1) =<>

   RecIdx.after (XIdx.eta sec0 node3 node2) =<>
   RecIdx.delta (XIdx.eta sec0 node0 node2) =<>
   RecIdx.delta (XIdx.eta sec0 node2 node1) =<>

   mempty


main :: IO ()
main =
   Draw.xterm $
      Draw.seqFlowGraph (Draw.deltaVariable Draw.optionsDefault) $
      SeqFlow.mapGraph Record.delta Record.delta $
      EqSys.solve
         (seqFlowGraphFromStates Tripod.topology
            [[dirEdge node0 node2, dirEdge node3 node2]])
         given
