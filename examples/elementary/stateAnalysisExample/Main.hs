{-# LANGUAGE TypeSynonymInstances #-}

module Main where

-- This example shows the functionality of StateAnalysis

import EFA2.Example.Examples
import qualified EFA2.StateAnalysis.StateAnalysis as StateAnalysis
import EFA2.Topology.Draw


-- Try topoLoop and topoDoubleLoop!
-- Try also your own topologies with constructors from NodeType
-- in TopologyData.hs

main :: IO ()
main = do
  let sol = StateAnalysis.advanced topoDreibein
  -- print sol
  print (length sol)
  drawTopologyXs' (map reorderEdges sol)
