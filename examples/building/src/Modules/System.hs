module Modules.System where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.StateFlow as StateFlow
--import qualified EFA.Graph as Gr
import qualified EFA.Graph.Flow as Flow
import qualified EFA.Signal.SequenceData as SD
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

data Node = --Gas
           Hausnetz
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
nodeList = [(Sonne, TD.AlwaysSource),
            (Verteiler, TD.Crossing),
            (Batterie, TD.Storage ()),
            (Hausnetz, TD.AlwaysSink),
            (Netz, TD.Crossing),
            (Kohle, TD.AlwaysSource),
--            (Gas, TD.Source),
            (Wasser, TD.Storage()),
            (Netzlast,TD.AlwaysSink)]

-- Define Edges with all their Properties
edgeList :: AppUt.LabeledEdgeList Node
edgeList = [(Sonne, Verteiler,"Solaranlage","Strahlung","Solar220V"),
            (Verteiler, Batterie, "Batterie","Batterie220V","Batterie DC"),
            (Verteiler, Hausnetz,"","",""),
            (Verteiler, Netz,"Trafo","Zähler",""),
            (Netz,Netzlast,"","",""),
--            (Gas,Netz,"GasKW","",""),
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


stateFlowGraph :: TD.StateFlowGraph Node
stateFlowGraph = 
  StateFlow.stateGraphActualStorageEdges
  $ AppUt.select flowStates [0, 3, 9, 12]

{-
-- | TODO -- Wirkungsgrade nur in der gewollten Richtiung ansetzen !!
etaAssign ::
  TIdx.Section ->
  Map (XIdx.Eta Node) (String, String, XIdx.Eta Node -> XIdx.Power Node)
etaAssign sec = Map.fromList $
  (XIdx.eta sec Wasser Netz, ( "solar", "solar", etaOverPowerOut)) :
  (XIdx.eta sec Netz Wasser, ( "solar", "solar", etaOverPowerIn)) :

  (XIdx.eta sec Wasser Netz, ( "storage", "storage", etaOverPowerOut)) :
  (XIdx.eta sec Netz Wasser, ( "storage", "storage", etaOverPowerIn)) :

  (XIdx.eta sec Verteiler Netz, ( "trafo", "trafo", etaOverPowerOut)) :
  (XIdx.eta sec Netz Verteiler, ( "trafo", "trafo", etaOverPowerIn)) :

  (XIdx.eta sec Kohle Netz, ( "coal", "coal", etaOverPowerOut)) :
  (XIdx.eta sec Netz Kohle, ( "coal", "coal", etaOverPowerIn)) :

{-  (XIdx.eta sec Gas Netz, ( "gas", "gas", etaOverPowerOut)) :
  (XIdx.eta sec Netz Gas, ( "gas", "gas", etaOverPowerIn)) : -}

  (XIdx.eta sec Netz Netzlast, ( "constOne", "constOne", etaOverPowerOut)) :
  (XIdx.eta sec Netzlast Netz, ( "constOne", "constOne", etaOverPowerIn)) :

  (XIdx.eta sec Hausnetz Verteiler, ( "constOne", "constOne", etaOverPowerOut)) :
  (XIdx.eta sec Verteiler Hausnetz, ( "constOne", "constOne", etaOverPowerIn)) :

  []
-}

-- | TODO -- Wirkungsgrade nur in der gewollten Richtung ansetzen !!
etaAssignState ::
  TIdx.State ->
  Map (XIdxState.Eta Node) (String, String, XIdxState.Eta Node -> XIdxState.Power Node)
etaAssignState state = Map.fromList $
  (XIdxState.eta state Sonne Verteiler, ( "solar", "solar", etaOverPowerOutState)) :
  (XIdxState.eta state Verteiler Sonne, ( "solar", "solar", etaOverPowerInState)) :

  (XIdxState.eta state Netz Wasser, ( "water", "water", etaOverPowerOutState)) :
  (XIdxState.eta state Wasser Netz, ( "water", "water", etaOverPowerInState)) :

  (XIdxState.eta state Verteiler Netz, ( "trafo", "trafo", etaOverPowerOutState)) :
  (XIdxState.eta state Netz Verteiler, ( "trafo", "trafo", etaOverPowerInState)) :

  (XIdxState.eta state Kohle Netz, ( "coal", "coal", etaOverPowerOutState)) :
  (XIdxState.eta state Netz Kohle, ( "coal", "coal", etaOverPowerInState)) :

{-  (XIdxState.eta state Gas Netz, ( "gas", "gas", etaOverPowerOut)) :
  (XIdxState.eta state Netz Gas, ( "gas", "gas", etaOverPowerIn)) : -}

  (XIdxState.eta state Netz Netzlast, ( "constOne", "constOne", etaOverPowerOutState)) :
  (XIdxState.eta state Netzlast Netz, ( "constOne", "constOne", etaOverPowerInState)) :

  (XIdxState.eta state Hausnetz Verteiler, ( "constOne", "constOne", etaOverPowerOutState)) :
  (XIdxState.eta state Verteiler Hausnetz, ( "constOne", "constOne", etaOverPowerInState)) :

  (XIdxState.eta state Verteiler Batterie, ( "battery", "battery", etaOverPowerOutState)) :
  (XIdxState.eta state Batterie Verteiler, ( "battery", "battery", etaOverPowerInState)) :
  []
