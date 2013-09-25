module Main where

import qualified EFA.Example.Topology.TripodA.Given as TripodGiven

import qualified EFA.Flow.State.Absolute as StateEqSys
import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Draw as Draw

import EFA.Utility.Async (concurrentlyMany_)

import Data.Monoid (Monoid, mempty)


main :: IO ()
main = do

   let seqFlowGraph =
          EqSys.solve TripodGiven.seqFlowGraph TripodGiven.equationSystem

       stateFlowGraph =
          StateFlow.flowGraphFromCumResult $
          StateFlow.fromSequenceFlowResult False seqFlowGraph

   concurrentlyMany_ $ map Draw.xterm $
      Draw.seqFlowGraph Draw.optionsDefault seqFlowGraph :
      Draw.stateFlowGraph Draw.optionsDefault stateFlowGraph :
      Draw.stateFlowGraph Draw.optionsDefault
         (StateEqSys.solve stateFlowGraph mempty) :
      []
