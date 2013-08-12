module Modules.System where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.StateFlow as StateFlow
--import qualified EFA.Graph as Gr
import qualified EFA.Graph.Flow as Flow
--import qualified EFA.Signal.SequenceData as SD
--import qualified EFA.Signal.Base as Base
--import qualified EFA.Signal.Record as Rec
--import qualified EFA.Signal.Typ as Typ
--import qualified EFA.Signal.Vector as Vec
import qualified EFA.Graph.Topology.Index as TIdx

--import EFA.Signal.Signal ((.+), (./))

import qualified EFA.Application.Utility as AppUt

import EFA.Application.Utility (select) -- makeEdges,
import qualified EFA.Application.Index as XIdx
import qualified EFA.Application.IndexState as XIdxState
import EFA.Application.Optimisation (etaOverPowerInState, etaOverPowerOutState)

-- import qualified Modules.Utility as ModUt

import EFA.Signal.Record (SigId(..))


import qualified Data.Map as Map
import Data.Map (Map)

data Node = Coal | Oil | Gas |
            Sun | Wind | Water |
            Network | Transformer | LocalNetwork |
            HouseHold | Industry |
            Rest | LocalRest deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault

topology :: TD.Topology Node
topology = AppUt.makeTopology nodeList edgeList

nodeList :: [(Node,TD.NodeType ())]
nodeList = [(Coal, TD.AlwaysSource),
            (Gas, TD.Source),
            (Water, TD.storage),
            (Network,TD.Crossing),
            (Rest, TD.AlwaysSink),
            (LocalNetwork,TD.Crossing),
            (LocalRest, TD.AlwaysSink)]


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


powerPositonNames :: Map (XIdx.PPos Node) SigId
powerPositonNames = Map.fromList $ concat $ map f edgeList
  where f (n1,n2,_,l1,l2) = [(XIdx.ppos n1 n2, SigId $ "Power-"++l1),
                             (XIdx.ppos n2 n1, SigId $ "Power-"++l2)]


flowStates :: [TD.FlowTopology Node]
flowStates = StateAnalysis.advanced topology


seqTopology :: Flow.RangeGraph Node
seqTopology = Flow.sequenceGraph (select flowStates [0, 4])


stateFlowGraph :: TD.StateFlowGraph Node
stateFlowGraph =
  StateFlow.stateGraphAllStorageEdges
  $ AppUt.select flowStates [0, 4]

etaAssignState ::
  TIdx.State ->
  Map (XIdxState.Eta Node) (String, String, XIdxState.Eta Node -> XIdxState.Power Node)
etaAssignState sec = Map.fromList $
  (XIdxState.eta sec Water Network, ( "storage", "storage", etaOverPowerInState)) :
  (XIdxState.eta sec Network Water, ( "storage", "storage", etaOverPowerOutState)) :

  (XIdxState.eta sec Coal Network, ( "coal", "coal", etaOverPowerInState)) :
  (XIdxState.eta sec Network Coal, ( "coal", "coal", etaOverPowerOutState)) :

  (XIdxState.eta sec Gas LocalNetwork, ( "gas", "gas", etaOverPowerInState)) :
  (XIdxState.eta sec LocalNetwork Gas, ( "gas", "gas", etaOverPowerOutState)) :

  (XIdxState.eta sec Network LocalNetwork, ( "transformer", "transformer", etaOverPowerInState)) :
  (XIdxState.eta sec LocalNetwork Network, ( "transformer", "transformer", etaOverPowerOutState)) :

  (XIdxState.eta sec LocalNetwork LocalRest, ( "local", "local", etaOverPowerInState)) :
  (XIdxState.eta sec LocalRest LocalNetwork, ( "local", "local", etaOverPowerOutState)) :

  (XIdxState.eta sec Network Rest, ( "rest", "rest", etaOverPowerInState)) :
  (XIdxState.eta sec Rest Network, ( "rest", "rest", etaOverPowerOutState)) :

  []
