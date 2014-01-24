module Modules.System where

import qualified EFA.Application.Utility as AppUt

import EFA.Application.OneStorage (EtaAssignMap, Name(Name))
import EFA.Application.Utility (identifyFlowState, dirEdge, undirEdge)

import qualified EFA.Flow.State.Quantity as StateFlow

import qualified EFA.Flow.Topology.Index as TopoIdx

import qualified EFA.Flow.SequenceState.Index as Idx

import EFA.Equation.Result (Result)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Utility.Filename as Filename

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import EFA.Signal.Record (SigId(SigId))

import qualified EFA.Report.Format as Format

import qualified Data.Map as Map
import Data.Map (Map)


state0, state1, state2, state3, state4, state5, state6, state7 :: Idx.State
state0 :~ state1 :~ state2 :~ state3 :~
  state4 :~ state5 :~ state6 :~ state7 :~ _ = Stream.enumFrom $ Idx.state0

data Node =
     Coal
   | Gas
   | Water
   | Network
   | LocalNetwork
   | Rest
   | LocalRest
   deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display Network = Format.literal "High Voltage"
   display LocalNetwork = Format.literal "Low Voltage"
   display Rest = Format.literal "Residual I"
   display LocalRest = Format.literal "Residual II"
   display x = Node.displayDefault x

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

instance Filename.Filename Node where
  filename = show

topology :: Topo.Topology Node
topology = Topo.plainFromLabeled labeledTopology

topology2 :: Topo.Topology Node
topology2 = Topo.plainFromLabeled labeledTopology2



labeledTopology :: Topo.LabeledTopology Node
labeledTopology = AppUt.topologyFromLabeledEdges edgeList

labeledTopology2 :: Topo.LabeledTopology Node
labeledTopology2 = AppUt.topologyFromLabeledEdges edgeList2

edgeList :: AppUt.LabeledEdgeList Node
edgeList = [(Coal, Network, "CoalPlant", "Coal","ElCoal"),
               (Water, Network, "WaterPlant","Water","ElWater"),

               (Network, Rest,"toRest","toRest","toRest"),
               (Network, LocalNetwork, "Transformer", "HighVoltage", "LowVoltage"),
               (Gas, LocalNetwork,"GasPlant","Gas","ElGas"),
               (LocalNetwork, LocalRest, "toLocalRest", "toLocalRest", "toLocalRest")]


edgeList2 :: AppUt.LabeledEdgeList Node
edgeList2 = [(Coal, Network, "CoalPlant", "Coal","ElCoal"),
               (Network, Water, "WaterPlant","Water","ElWater"),

               (Network, Rest,"toRest","toRest","toRest"),
               (Network, LocalNetwork, "Transformer", "HighVoltage", "LowVoltage"),
               (Gas, LocalNetwork,"GasPlant","Gas","ElGas"),
               (LocalNetwork, LocalRest, "toLocalRest", "toLocalRest", "toLocalRest")]

powerPositonNames :: Map (TopoIdx.Position Node) SigId
powerPositonNames = Map.fromList $ concatMap f edgeList
  where f (n1,n2,_,l1,l2) = [(TopoIdx.ppos n1 n2, SigId $ "Power-"++l1),
                             (TopoIdx.ppos n2 n1, SigId $ "Power-"++l2)]


flowStates :: [Topo.FlowTopology Node]
flowStates =
   map (identifyFlowState topology) $
      [ [dirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, dirEdge Water Network],
        [dirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, dirEdge Network Water],
        [undirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, dirEdge Network Water],
        [undirEdge Gas LocalNetwork, dirEdge Network LocalNetwork, dirEdge Water Network] ]

{-
        [dirEdge Gas LocalNetwork, dirEdge LocalNetwork Network, dirEdge Network Water],
        [dirEdge Gas LocalNetwork, dirEdge LocalNetwork Network, dirEdge Water Network] ]
-}


stateFlowGraph :: StateFlow.Graph Node (Result a) (Result v)
stateFlowGraph =
   StateFlow.graphFromStates flowStates


etaAssign ::
   node -> node -> name ->
   (TopoIdx.Position node, (name, name))
etaAssign from to name =
   (TopoIdx.Position from to, (name, name))

etaAssignMap :: EtaAssignMap Node
etaAssignMap = Map.fromList $
   etaAssign Network Water storage :
   etaAssign Network Coal coal :
   etaAssign LocalNetwork Gas gas :
   etaAssign LocalNetwork Network transformer :
   etaAssign LocalRest LocalNetwork local :
   etaAssign Rest Network rest :
   []

storage, coal, gas, transformer, local, rest :: Name
storage     = Name "storage"
coal        = Name "coal"
gas         = Name "gas"
transformer = Name "transformer"
local       = Name "local"
rest        = Name "rest"
