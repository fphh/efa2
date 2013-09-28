module Modules.System where

import qualified EFA.Application.Utility as AppUt
import EFA.Application.Utility (identifyFlowState, dirEdge)

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph.Flow as Flow

import qualified EFA.Signal.Sequence as Sequ
import EFA.Signal.Record (SigId(SigId))

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
topology = AppUt.makeTopology edgeList

edgeList :: AppUt.LabeledEdgeList Node
edgeList = [(Coal, Network, "CoalPlant", "Coal","ElCoal"),
            (Gas, Network,"GasPlant","Gas","ElGas"),
            (Water, Network,"WaterPlant","Water","ElWater"),
            (Wind, Network,"WindPark","Wind","ElWind"),
            (Network, LocalNetwork, "Transformer", "HighVoltage", "LowVoltage"),
            (LocalNetwork, HouseHold, "toHouseHold", "toHouseHold", "MeterHouseHold"),
            (LocalNetwork, Industry, "toIndustrial", "toIndustrial", "MeterIndustrial"),
            (Sun, LocalNetwork, "SolarPlant", "SunLight", "elSolar")]


edgeNames :: Map (Node, Node) String
edgeNames = Map.fromList el
  where el = map f edgeList
        f (x, y, lab, _, _) = ((x, y), lab)


powerPositonNames :: Map (XIdx.PPos Node) SigId
powerPositonNames = Map.fromList $ concat $ map f edgeList
  where f (n1,n2,_,l1,l2) = [(XIdx.ppos n1 n2, SigId $ "Power-"++l1),
                             (XIdx.ppos n2 n1, SigId $ "Power-"++l2)]

showPowerId :: XIdx.PPos Node -> String
showPowerId ppos = f (Map.lookup  ppos powerPositonNames)
  where
    f (Just sid) = show sid
    f Nothing = (show ppos)


convertPowerId :: XIdx.PPos Node -> SigId
convertPowerId ppos =  f (Map.lookup  ppos powerPositonNames)
  where
    f (Just sid) = sid
    f Nothing = SigId (show ppos)


----------------------------------------------------------------------
-- | Topology for Optimisation
topologyOpt :: Topo.Topology Node
topologyOpt = AppUt.makeTopology edgeListOpt

edgeListOpt :: AppUt.LabeledEdgeList Node
edgeListOpt = [(Coal, Network, "CoalPlant", "Coal","ElCoal"),
               (Water, Network,"WaterPlant","Water","ElWater"),
               (Network, Rest,"toRest","toRest","toRest"),
               (Network, LocalNetwork, "Transformer", "HighVoltage", "LowVoltage"),
               (Gas, LocalNetwork,"GasPlant","Gas","ElGas"),
               (LocalNetwork, LocalRest, "toLocalRest", "toLocalRest", "toLocalRest")]

edgeNamesOpt :: Map (Node, Node) String
edgeNamesOpt = Map.fromList el
  where el = map f edgeListOpt
        f (x, y, lab, _, _) = ((x, y), lab)

----------------------------------------------------------------------
-- | SequenceTopology for Optimisation

flowState0, flowState4 :: Topo.FlowTopology Node
flowState0 =
   identifyFlowState topologyOpt
      [dirEdge Gas Network, dirEdge Sun LocalNetwork, dirEdge Wind Network,
       dirEdge Water Network]

flowState4 =
   identifyFlowState topologyOpt
      [dirEdge Gas Network, dirEdge Sun LocalNetwork, dirEdge Wind Network,
       dirEdge Network Water, dirEdge Network LocalNetwork]

seqTopoOpt :: Flow.RangeGraph Node
seqTopoOpt =
   Flow.sequenceGraph $ Sequ.fromList [flowState4, flowState0]
