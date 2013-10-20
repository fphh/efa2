module Modules.System where

import qualified EFA.Application.Utility as AppUt

import EFA.Application.Simulation (EtaAssignMap)
import EFA.Application.Utility (identifyFlowState, dirEdge, undirEdge)

import qualified EFA.Flow.State.Quantity as StateFlow

import qualified EFA.Flow.Sequence.Index as SeqIdx

import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo

import EFA.Signal.Record (SigId(SigId))


import qualified Data.Map as Map
import Data.Map (Map)

data Node =
     Coal
--   | Oil
   | Gas
--   | Sun
--   | Wind
   | Water
   | Network
--   | Transformer
   | LocalNetwork
--   | HouseHold
--   | Industry
   | Rest
   | LocalRest
   deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault
   typ t =
      case t of
         Coal -> Node.AlwaysSource
         Gas -> Node.Source
         Water -> Node.storage
         Network -> Node.Crossing
         Rest -> Node.AlwaysSink
         LocalNetwork -> Node.Crossing
         LocalRest -> Node.AlwaysSink

topology :: Topo.Topology Node
topology = AppUt.makeTopology edgeList

edgeList :: AppUt.LabeledEdgeList Node
edgeList = [(Coal, Network, "CoalPlant", "Coal","ElCoal"),
               (Water, Network,"WaterPlant","Water","ElWater"),
               (Network, Rest,"toRest","toRest","toRest"),
               (Network, LocalNetwork, "Transformer", "HighVoltage", "LowVoltage"),
               (Gas, LocalNetwork,"GasPlant","Gas","ElGas"),
               (LocalNetwork, LocalRest, "toLocalRest", "toLocalRest", "toLocalRest")]

edgeNames :: Map (Node, Node) String
edgeNames = Map.fromList el
  where el = map f edgeList
        f (x, y, lab, _, _) = (if x<y then (x, y) else (y,x), lab)


powerPositonNames :: Map (SeqIdx.PPos Node) SigId
powerPositonNames = Map.fromList $ concatMap f edgeList
  where f (n1,n2,_,l1,l2) = [(SeqIdx.ppos n1 n2, SigId $ "Power-"++l1),
                             (SeqIdx.ppos n2 n1, SigId $ "Power-"++l2)]


flowStates :: [Topo.FlowTopology Node]
flowStates =
   map (identifyFlowState topology) $
      [[dirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, dirEdge Water Network],
       [dirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, dirEdge Network Water],
       [undirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, dirEdge Water Network],
       [undirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, dirEdge Network Water]]
 --      [undirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, undirEdge Network Water],
 --      [dirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, undirEdge Network Water]]


stateFlowGraph :: StateFlow.Graph Node (Result a) (Result v)
stateFlowGraph =
   StateFlow.graphFromStates flowStates


etaAssign ::
   node -> node -> name ->
   (Idx.StructureEdge node, (name, name))
etaAssign from to name =
   (Idx.StructureEdge from to, (name, name))

etaAssignMap :: EtaAssignMap Node
etaAssignMap = Map.fromList $
   etaAssign Network Water "storage" :
   etaAssign Network Coal "coal" :
   etaAssign LocalNetwork Gas "gas" :
   etaAssign LocalNetwork Network "transformer" :
   etaAssign LocalRest LocalNetwork "local" :
   etaAssign Rest Network "rest" :
   []
