{-# LANGUAGE TypeSynonymInstances #-}

module Main where

-- This example shows the functionality of StateAnalysis

import EFA2.Example.Examples
import EFA2.StateAnalysis.StateAnalysis
import EFA2.Display.DrawGraph


-- Try topoLoop and topoDoubleLoop!
-- Try also your own topologies with constructors from NodeType
-- in TopologyData.hs

main :: IO ()
main = do
  let sol = stateAnalysis topoDreibein
  -- print sol
  print (length sol)
  drawTopologyXs' (map reorderEdges sol)