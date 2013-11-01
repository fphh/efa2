module Modules.System where

import qualified EFA.Application.Utility as AppUt
import EFA.Application.Utility (identifyFlowState, dirEdge)

import EFA.Equation.Result (Result)

import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Topology.Index as XIdx

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import EFA.Signal.Record (SigId(SigId))
import EFA.Signal.Data (Data, Nil)

import qualified Data.Map as Map
import Data.Map (Map)


data Node =
     Coal
--   | Nuclear
--   | Oil
   | Gas
   | Sun
   | Wind
   | Water
   | Network
--   | Transformer
   | LocalNetwork
   | HouseHold
   | Industry
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
         Sun -> Node.Source
         Wind -> Node.Source
         Network -> Node.Crossing
         LocalNetwork -> Node.Crossing
         HouseHold -> Node.AlwaysSink
         Industry -> Node.AlwaysSink
         Rest -> Node.AlwaysSink
         LocalRest -> Node.AlwaysSink


----------------------------------------------------------------------
-- * Define System Topology

topology :: Topo.Topology Node
topology = Topo.plainFromLabeled labeledTopology

labeledTopology :: Topo.LabeledTopology Node
labeledTopology = AppUt.topologyFromLabeledEdges edgeList

edgeList :: AppUt.LabeledEdgeList Node
edgeList = [(Coal, Network, "CoalPlant", "Coal","ElCoal"),
            (Gas, Network,"GasPlant","Gas","ElGas"),
            (Water, Network,"WaterPlant","Water","ElWater"),
            (Wind, Network,"WindPark","Wind","ElWind"),
            (Network, LocalNetwork, "Transformer", "HighVoltage", "LowVoltage"),
            (LocalNetwork, HouseHold, "toHouseHold", "toHouseHold", "MeterHouseHold"),
            (LocalNetwork, Industry, "toIndustrial", "toIndustrial", "MeterIndustrial"),
            (Sun, LocalNetwork, "SolarPlant", "SunLight", "elSolar")]


powerPositonNames :: Map (XIdx.PPos Node) SigId
powerPositonNames = Map.fromList $ concat $ map f edgeList
  where f (n1,n2,_,l1,l2) = [(XIdx.ppos n1 n2, SigId $ "Power-"++l1),
                             (XIdx.ppos n2 n1, SigId $ "Power-"++l2)]

showPowerId :: XIdx.PPos Node -> String
showPowerId ppos = f (Map.lookup ppos powerPositonNames)
  where
    f (Just sid) = show sid
    f Nothing = show ppos


convertPowerId :: XIdx.PPos Node -> SigId
convertPowerId ppos =  f (Map.lookup  ppos powerPositonNames)
  where
    f (Just sid) = sid
    f Nothing = SigId (show ppos)


----------------------------------------------------------------------
-- | Topology for Optimisation
topologyOpt :: Topo.Topology Node
topologyOpt = Topo.plainFromLabeled labeledTopologyOpt

labeledTopologyOpt :: Topo.LabeledTopology Node
labeledTopologyOpt = AppUt.topologyFromLabeledEdges edgeListOpt

edgeListOpt :: AppUt.LabeledEdgeList Node
edgeListOpt = [(Coal, Network, "CoalPlant", "Coal","ElCoal"),
               (Water, Network,"WaterPlant","Water","ElWater"),
               (Network, Rest,"toRest","toRest","toRest"),
               (Network, LocalNetwork, "Transformer", "HighVoltage", "LowVoltage"),
               (Gas, LocalNetwork,"GasPlant","Gas","ElGas"),
               (LocalNetwork, LocalRest, "toLocalRest", "toLocalRest", "toLocalRest")]

----------------------------------------------------------------------
-- | SequenceTopology for Optimisation

flowState0, flowState4 :: Topo.FlowTopology Node
flowState0 =
   identifyFlowState topologyOpt
      [dirEdge Gas LocalNetwork, dirEdge Network LocalNetwork,
       dirEdge Water Network]

flowState4 =
   identifyFlowState topologyOpt
      [dirEdge Gas LocalNetwork, dirEdge Network LocalNetwork,
       dirEdge Network Water]

flowGraphOpt ::
   SeqFlow.Graph Node (Result (Data Nil Double)) (Result (Data Nil Double))
flowGraphOpt =
   AppUt.seqFlowGraphFromFlowTopos [flowState4, flowState0]
