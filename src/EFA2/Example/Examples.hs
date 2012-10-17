
module EFA2.Example.Examples where

import EFA2.Topology.Topology (makeNodes, makeEdges)
import EFA2.Topology.TopologyData (Topology, NodeType(..), defaultELabel)
import Data.Graph.Inductive (mkGraph)


topoDreibein :: Topology
topoDreibein = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, Source), (1, Sink), (2, Crossing), (3, Storage 0)]
        edges = [(0, 2, defaultELabel), (1, 2, defaultELabel), (2, 3, defaultELabel)]


topoLoop :: Topology
topoLoop = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, NoRestriction), (1, Crossing), (2, Crossing), (3, Crossing), (4, NoRestriction), (5, NoRestriction) ]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel),
                 (2, 3, defaultELabel), (2, 4, defaultELabel), (3, 5, defaultELabel) ]

-- Topologie mit zwei Komponenten
topoDoubleLoop :: Topology
topoDoubleLoop = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, NoRestriction), (1, Crossing), (2, Crossing), (3, Crossing), (4, NoRestriction), (5, NoRestriction),
                 (6, NoRestriction), (7, Crossing), (8, Crossing), (9, Crossing), (10, NoRestriction), (11, NoRestriction) ]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel),
                 (2, 3, defaultELabel), (2, 4, defaultELabel), (3, 5, defaultELabel),
                 (6, 7, defaultELabel), (7, 8, defaultELabel), (7, 9, defaultELabel),
                 (8, 9, defaultELabel), (8, 10, defaultELabel), (9, 11, defaultELabel) ]
