module Main where

import qualified EFA.Example.Topology.TripodA.Given as TripodGiven

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Draw as Draw

import qualified Data.GraphViz.Attributes.Colors.X11 as Colors


main :: IO ()
main =
  Draw.xterm $
    Draw.bgcolour Colors.Burlywood1 $
    Draw.title "Dies ist der Titel!" $
    Draw.sequFlowGraph
       (Draw.hideEtaNode $ Draw.showStorage $ Draw.showStorageEdge $
        Draw.hideVariableIndex $ Draw.optionsDefault) $
    EqSys.solve TripodGiven.seqFlowGraph TripodGiven.equationSystem
