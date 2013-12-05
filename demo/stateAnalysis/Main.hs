module Main where

import qualified EFA.Example.Topology.Tripod as Tripod

import qualified EFA.Application.StateAnalysis as StateAnalysis


main :: IO ()
main = StateAnalysis.showStates Tripod.topology
