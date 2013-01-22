-- | This example shows the functionality of StateAnalysis
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import EFA.Example.Examples
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Draw as Draw


-- Try topoLoop and topoDoubleLoop!
-- Try also your own topologies with constructors from NodeType
-- in TopologyData.hs

main :: IO ()
main = do
  let sol = StateAnalysis.advanced topoDreibein
  -- print sol
  print (length sol)
  Draw.flowTopologies (map reorderEdges sol)
