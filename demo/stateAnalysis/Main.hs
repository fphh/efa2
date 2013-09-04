-- | This example demonstrates the functionality of StateAnalysis

module Main where

import qualified EFA.Application.Topology.TripodA as Tripod

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Draw as Draw


main :: IO ()
main = do
  let sol = StateAnalysis.advanced Tripod.topology
  print (length sol)
  Draw.xterm $ Draw.flowTopologies sol
