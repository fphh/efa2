module Main where

import qualified EFA.Application.Topology.TripodA as Tripod
import qualified EFA.Application.Symbolic as Symbolic
import EFA.Application.Topology.TripodA (Node, node0, node1, node2, node3)
import EFA.Application.Symbolic ((=<>), (.=))
import EFA.Application.Utility (seqFlowGraphRecordFromStates)

import qualified EFA.Flow.Sequence.EquationSystem as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Equation.Record as Record
import EFA.Equation.Arithmetic (zero)

import qualified EFA.Graph.Topology.Index as Idx

import Data.Monoid (mempty, (<>))


sec0 :: Idx.Section
sec0 = Idx.Section 0


{-
Use SumProduct.Term here since it simplifies automatically.
-}
given ::
   Symbolic.EquationSystem Symbolic.Ignore
      Record.Delta Node s SumProduct.Term
given =
   (Idx.delta (XIdx.dTime sec0) .= zero) <>

   Idx.before (XIdx.dTime sec0) =<>

   Idx.before (XIdx.storage Idx.initial node3) =<>
   Idx.delta (XIdx.storage (Idx.afterSection sec0) node3) =<>

   Idx.after (XIdx.power sec0 node3 node2) =<>
   Idx.after (XIdx.x sec0 node2 node3) =<>

   Idx.before (XIdx.power sec0 node3 node2) =<>
   Idx.before (XIdx.x sec0 node2 node3) =<>

   Idx.before (XIdx.eta sec0 node3 node2) =<>
   Idx.before (XIdx.eta sec0 node0 node2) =<>
   Idx.before (XIdx.eta sec0 node2 node1) =<>

   Idx.after (XIdx.eta sec0 node3 node2) =<>
   Idx.delta (XIdx.eta sec0 node0 node2) =<>
   Idx.delta (XIdx.eta sec0 node2 node1) =<>

   mempty


main :: IO ()
main =
   Draw.xterm $
      Draw.sequFlowGraph (Draw.deltaVariable Draw.optionsDefault) $
      SeqFlow.mapGraph Record.delta Record.delta $
      EqSys.solve (seqFlowGraphRecordFromStates Tripod.topology [1]) given
