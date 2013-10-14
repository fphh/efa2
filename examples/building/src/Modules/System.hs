module Modules.System where

import qualified EFA.Application.Utility as AppUt

import EFA.Application.Utility (identifyFlowState, dirEdge)

import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.State.Index as StateIdx

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph.StateFlow as StateFlow
import qualified EFA.Graph.Flow as Flow

import qualified EFA.Signal.Sequence as Sequ
--import qualified EFA.Signal.Base as Base
--import qualified EFA.Signal.Record as Rec
--import qualified EFA.Signal.Typ as Typ
--import qualified EFA.Signal.Vector as Vec

--import EFA.Signal.Signal ((.+), (./))

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
powerPositonNames = Map.fromList $ concat $ map f edgeList
  where f (n1,n2,_,l1,l2) = [(SeqIdx.ppos n1 n2, SigId $ "Power-"++l1),
                             (SeqIdx.ppos n2 n1, SigId $ "Power-"++l2)]


flowStates :: Sequ.List (Topo.FlowTopology Node)
flowStates =
   fmap (identifyFlowState topology) $
   Sequ.fromList $
      [[dirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, dirEdge Water Network],
       [dirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, dirEdge Network Water]]

seqTopology :: Flow.RangeGraph Node
seqTopology = Flow.sequenceGraph flowStates


stateFlowGraph :: Topo.StateFlowGraph Node
stateFlowGraph = StateFlow.stateGraphAllStorageEdges flowStates


etaAssign ::
   Idx.State ->
   node ->
   node ->
   t ->
   [(StateIdx.Eta node, (t, t, StateIdx.Power node))]
etaAssign state from to name =
   let powerIdx = StateIdx.power state to from
   in  (StateIdx.eta state from to, (name, name, powerIdx)) :
       (StateIdx.eta state to from, (name, name, powerIdx)) :
       []

etaAssignState ::
   Idx.State ->
   Map (StateIdx.Eta Node) (String, String, StateIdx.Power Node)
etaAssignState state = Map.fromList $
   etaAssign state Water Network "storage" ++
   etaAssign state Coal Network "coal" ++
   etaAssign state Gas LocalNetwork "gas" ++
   etaAssign state Network LocalNetwork "transformer" ++
   etaAssign state LocalNetwork LocalRest "local" ++
   etaAssign state Network Rest "rest" ++
   []
