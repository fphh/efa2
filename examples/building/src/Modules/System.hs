module Modules.System where

import qualified EFA.Application.Utility as AppUt

import EFA.Application.Utility (select) -- makeEdges,
import EFA.Application.Optimisation (etaOverPowerInState, etaOverPowerOutState)

import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.State.Index as StateIdx

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
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

topology :: Topo.Topology Node
topology = AppUt.makeTopology nodeList edgeList

nodeList :: [(Node,Node.Type ())]
nodeList = [(Coal, Node.AlwaysSource),
            (Gas, Node.Source),
            (Water, Node.storage),
            (Network,Node.Crossing),
            (Rest, Node.AlwaysSink),
            (LocalNetwork,Node.Crossing),
            (LocalRest, Node.AlwaysSink)]


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


flowStates :: [Topo.FlowTopology Node]
flowStates = StateAnalysis.advanced topology


seqTopology :: Flow.RangeGraph Node
seqTopology = Flow.sequenceGraph (select flowStates [0, 4])


stateFlowGraph :: Topo.StateFlowGraph Node
stateFlowGraph =
  StateFlow.stateGraphAllStorageEdges
  $ AppUt.select flowStates [0, 4]

etaAssignState ::
  TIdx.State ->
  Map (StateIdx.Eta Node) (String, String, StateIdx.Eta Node -> StateIdx.Power Node)
etaAssignState sec = Map.fromList $
  (StateIdx.eta sec Water Network, ( "storage", "storage", etaOverPowerInState)) :
  (StateIdx.eta sec Network Water, ( "storage", "storage", etaOverPowerOutState)) :

  (StateIdx.eta sec Coal Network, ( "coal", "coal", etaOverPowerInState)) :
  (StateIdx.eta sec Network Coal, ( "coal", "coal", etaOverPowerOutState)) :

  (StateIdx.eta sec Gas LocalNetwork, ( "gas", "gas", etaOverPowerInState)) :
  (StateIdx.eta sec LocalNetwork Gas, ( "gas", "gas", etaOverPowerOutState)) :

  (StateIdx.eta sec Network LocalNetwork, ( "transformer", "transformer", etaOverPowerInState)) :
  (StateIdx.eta sec LocalNetwork Network, ( "transformer", "transformer", etaOverPowerOutState)) :

  (StateIdx.eta sec LocalNetwork LocalRest, ( "local", "local", etaOverPowerInState)) :
  (StateIdx.eta sec LocalRest LocalNetwork, ( "local", "local", etaOverPowerOutState)) :

  (StateIdx.eta sec Network Rest, ( "rest", "rest", etaOverPowerInState)) :
  (StateIdx.eta sec Rest Network, ( "rest", "rest", etaOverPowerOutState)) :

  []
