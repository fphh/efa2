

module EFA2.Topology.TopologyData where

import Data.Graph.Inductive


data NodeType = Storage
              | Sink
              | Source
              | Crossing deriving (Show, Ord, Eq)

data NLabel = NLabel { sectionNLabel :: Int,
                       recordNLabel :: Int,
                       nodeNLabel :: Int,
                       nodetypeNLabel :: NodeType } deriving (Show, Eq, Ord)

data ELabel = ELabel deriving (Show)

newtype Topology' a b = Topology { unTopology :: Gr a b } deriving (Show)
type Topology = Topology' NLabel ELabel

instance Graph Topology' where
         empty = Topology empty
         isEmpty (Topology topo) = isEmpty topo
         match n (Topology topo) = (mcont, Topology g)
           where (mcont, g) = match n topo
         mkGraph ns es = Topology (mkGraph ns es)
         labNodes (Topology topo) = labNodes topo

instance DynGraph Topology' where
         cont & (Topology topo) = Topology (cont & topo)

-- | 
newtype FlowTopology' a b = FlowTopology { unFlowTopology :: Gr a b } deriving (Show)
type FlowTopology = FlowTopology' NLabel ELabel

instance Graph FlowTopology' where
         empty = FlowTopology empty
         isEmpty (FlowTopology topo) = isEmpty topo
         match n (FlowTopology topo) = (mcont, FlowTopology g)
           where (mcont, g) = match n topo
         mkGraph ns es = FlowTopology (mkGraph ns es)
         labNodes (FlowTopology topo) = labNodes topo

instance DynGraph FlowTopology' where
         cont & (FlowTopology topo) = FlowTopology (cont & topo)

-- | 
newtype SecTopology' a b = SecTopology { unSecTopology :: Gr a b } deriving (Show)
type SecTopology = SecTopology' NLabel ELabel

instance Graph SecTopology' where
         empty = SecTopology empty
         isEmpty (SecTopology topo) = isEmpty topo
         match n (SecTopology topo) = (mcont, SecTopology g)
           where (mcont, g) = match n topo
         mkGraph ns es = SecTopology (mkGraph ns es)
         labNodes (SecTopology topo) = labNodes topo

instance DynGraph SecTopology' where
         cont & (SecTopology topo) = SecTopology (cont & topo)
