module Main where

import qualified EFA.Example.Topology.TripodA.Given as TripodGiven

import qualified EFA.Flow.Cumulated.Absolute as CumEqSys
import qualified EFA.Flow.Cumulated.Quantity as Cumulated

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Graph as Graph

import EFA.Utility.Async (concurrentlyMany_)

import Data.Monoid (Monoid, mempty)


main :: IO ()
main = do

   let solved = EqSys.solve TripodGiven.seqFlowGraph TripodGiven.equationSystem
       cum =
          Graph.mapEdge Cumulated.flowResultFromCumResult $
          Cumulated.fromSequenceFlowResult $ SeqFlow.sequence solved
       cumSolved = CumEqSys.solve cum mempty

   concurrentlyMany_ $ map Draw.xterm $
      Draw.sequFlowGraph Draw.optionsDefault solved :
      Draw.cumulatedFlow cum :
      Draw.cumulatedFlow cumSolved :
      []
