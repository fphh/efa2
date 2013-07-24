module Modules.System where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
--import qualified EFA.Graph as Gr
import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Signal.Base as Base
--import qualified EFA.Signal.Record as Rec
--import qualified EFA.Signal.Typ as Typ
--import qualified EFA.Signal.Vector as Vec

--import EFA.Signal.Signal ((.+), (./))

import qualified EFA.Application.Utility as AppUt

import EFA.Application.Utility (select) -- makeEdges,
import qualified EFA.Application.Index as XIdx

-- import qualified Modules.Utility as ModUt

import EFA.Signal.Record (SigId(..))


import qualified Data.Map as Map
import Data.Map (Map)

data Node = Gas
          | Hausnetz
          | Kohle
          | Netz
          | Netzlast
          | Netzspeicher
          | Netzanschluss
          | Sonne
          | Verteiler
          | Batterie
          | Wasser
             deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault
{-
topology :: TD.Topology Node
topology = Gr.fromList nodes (makeEdges edges)
  where nodes = [(Sonne, TD.Source),
                 (Verteiler, TD.Crossing),
                 (Batterie, TD.Storage ()),
                 (Hausnetz, TD.Sink),
                 (Netz, TD.Crossing),
                 (Kohle, TD.Source),
                 (Gas, TD.Source),
                 (Wasser, TD.Storage()),
                 (Netzlast,TD.Sink)]

        edges = map f edgeList
          where f (n1,n2,_,_,_) = (n1,n2)-}

topology :: TD.Topology Node
topology = AppUt.makeTopology nodeList edgeList


nodeList :: [(Node,TD.NodeType ())]
nodeList = [(Sonne, TD.Source),
            (Verteiler, TD.Crossing),
            (Batterie, TD.Storage ()),
            (Hausnetz, TD.Sink),
            (Netz, TD.Crossing),
            (Kohle, TD.Source),
            (Gas, TD.Source),
            (Wasser, TD.Storage()),
            (Netzlast,TD.Sink)]

-- Define Edges with all their Properties
edgeList :: AppUt.LabeledEdgeList Node
edgeList = [(Sonne, Verteiler,"Solaranlage","Strahlung","Solar220V"),
            (Verteiler, Batterie, "Batterie","Batterie220V","Batterie DC"),
            (Verteiler, Hausnetz,"","",""),
            (Verteiler, Netz,"Trafo","Zähler",""),
            (Netz,Netzlast,"","",""),
            (Gas,Netz,"GasKW","",""),
            (Kohle,Netz,"KohleKW","",""),
            (Netz,Wasser,"SpeicherKW","","")]

edgeNames :: Map (Node, Node) String
edgeNames = Map.fromList el
  where el = map f edgeList
        f (x, y, lab, _, _) = (if x<y then (x, y) else (y,x), lab)


powerPositonNames :: Map (XIdx.PPos Node) SigId
powerPositonNames = Map.fromList $ concat $ map f edgeList
  where f (n1,n2,_,l1,l2) = [(XIdx.ppos n1 n2, SigId $ "Power-"++l1),
                             (XIdx.ppos n2 n1, SigId $ "Power-"++l2)]


flowStates :: [TD.FlowTopology Node]
flowStates = StateAnalysis.advanced topology


seqTopology :: Flow.RangeGraph Node
seqTopology = Flow.sequenceGraph (select flowStates [0,1])
