module Modules.System where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Graph as Gr

import qualified EFA.Application.Index as XIdx
import EFA.Application.Utility (makeEdges, select)

import EFA.Signal.Record (SigId(..))

import qualified Data.Map as Map
import Data.Map (Map)


data Node =  Nuclear | Coal | Oil | Gas |
             Sun | Wind | Water |
             Network | Transformer | LocalNetwork |
             HouseHold | Industry |
             Rest | LocalRest
          deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault

----------------------------------------------------------------------
-- * Define System Topology
topology :: TD.Topology Node
topology = Gr.fromList ns (makeEdges es)
  where ns = [(Coal, TD.AlwaysSource),
              (Gas, TD.Source),
              (Water, TD.storage),
              (Sun, TD.Source),
              (Wind, TD.Source),
              (Network, TD.Crossing),
              (LocalNetwork,TD.Crossing),
              (HouseHold, TD.AlwaysSink),
              (Industry, TD.AlwaysSink)]

        --extract edge Info
        es = map f edgeList
          where f (n1,n2,_,_,_) = (n1,n2)


-- Define Edges with all their Properties
type EdgeName = String

edgeList :: [(Node, Node, EdgeName, String, String)]
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
-- * Calculate Flow States

flowStates :: [TD.FlowTopology Node]
flowStates = StateAnalysis.advanced topology




----------------------------------------------------------------------
-- | Topology for Optimisation
topologyOpt :: TD.Topology Node
topologyOpt = Gr.fromList ns (makeEdges es)
  where ns = [(Coal, TD.AlwaysSource),
              (Gas, TD.Source),
              (Water, TD.storage),
              (Network,TD.Crossing),
              (Rest, TD.AlwaysSink),
              (LocalNetwork,TD.Crossing),
              (LocalRest, TD.AlwaysSink)]

        es = map f edgeListOpt
          where f (n1,n2,_,_,_) = (n1,n2)


edgeListOpt :: [(Node, Node, EdgeName, String, String)]
edgeListOpt = [(Coal, Network, "CoalPlant", "Coal","ElCoal"),
               (Water, Network,"WaterPlant","Water","ElWater"),
               (Network, Rest,"toRest","toRest","toRest"),
               (Network, LocalNetwork, "Transformer", "HighVoltage", "LowVoltage"),
               (Gas, LocalNetwork,"GasPlant","Gas","ElGas"),
               (LocalNetwork, LocalRest, "toLocalRest", "toLocalRest", "toLocalRest")]

flowStatesOpt :: [TD.FlowTopology Node]
flowStatesOpt = StateAnalysis.advanced topologyOpt

edgeNamesOpt :: Map (Node, Node) String
edgeNamesOpt = Map.fromList el
  where el = map f edgeListOpt
        f (x, y, lab, _, _) = ((x, y), lab)

----------------------------------------------------------------------
-- | SequenceTopology for Optimisation

seqTopoOpt :: Flow.RangeGraph Node
--seqTopoOpt = Flow.sequenceGraph (ModUt.select flowStatesOpt [5,1])
seqTopoOpt = Flow.sequenceGraph (select flowStatesOpt [4,0])
