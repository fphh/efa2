{-# LANGUAGE TypeSynonymInstances #-}

module Main where

-- This example shows the functionality of StateAnalysis

import EFA.Example.Examples
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import EFA.Graph.Draw


-- Try topoLoop and topoDoubleLoop!
-- Try also your own topologies with constructors from NodeType
-- in TopologyData.hs

main :: IO ()
main = do
  let sol = StateAnalysis.advanced topoDreibein
  -- print sol
  print (length sol)
  drawTopologyXs' (map reorderEdges sol)
