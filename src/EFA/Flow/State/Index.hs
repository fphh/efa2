module EFA.Flow.State.Index where

import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Graph.Topology.Index as Idx

import Prelude hiding (sum)


type Energy node    = Idx.InState TopoIdx.Energy node
type Power node     = Idx.InState TopoIdx.Power node
type Eta node       = Idx.InState TopoIdx.Eta node
type X node         = Idx.InState TopoIdx.X node
type DTime node     = Idx.InState TopoIdx.DTime node
type Sum node       = Idx.InState TopoIdx.Sum node

type Storage node   = Idx.ForStorage StorageIdx.Content node
type MaxEnergy node = Idx.ForStorage StorageIdx.MaxEnergy node
type StEnergy node  = Idx.ForStorage (StorageIdx.Energy Idx.State) node
type StX node       = Idx.ForStorage (StorageIdx.X Idx.State) node
type StInSum node   = Idx.ForStorage (StorageIdx.InSum Idx.State) node
type StOutSum node  = Idx.ForStorage (StorageIdx.OutSum Idx.State) node

type CarryEdge = Idx.CarryEdge Idx.State
type CarryBond = Idx.CarryBond Idx.State


energy :: Idx.State -> node -> node -> Energy node
power :: Idx.State -> node -> node -> Power node
eta :: Idx.State -> node -> node -> Eta node
x :: Idx.State -> node -> node -> X node

energy    = topologyEdge TopoIdx.Energy
power     = topologyEdge TopoIdx.Power
eta       = topologyEdge TopoIdx.Eta
x         = topologyEdge TopoIdx.X

topologyEdge ::
   (TopoIdx.Edge node -> idx node) ->
   Idx.State -> node -> node -> Idx.InState idx node
topologyEdge mkIdx s from to =
   Idx.InPart s $ mkIdx $ TopoIdx.Edge from to


stX ::
   Idx.PartNode (Idx.CarryBond sec) node ->
   Idx.ForStorage (StorageIdx.X sec) node
stX = Idx.forStorage StorageIdx.X

stInSum ::
   Idx.StateOrExit -> node -> StInSum node
stInSum state =
   Idx.ForStorage (StorageIdx.InSum state)

stOutSum ::
   Idx.InitOrState -> node -> StOutSum node
stOutSum state =
   Idx.ForStorage (StorageIdx.OutSum state)


dTime :: Idx.State -> DTime node
dTime sec = Idx.InPart sec TopoIdx.DTime

sum :: Idx.State -> TopoIdx.Direction -> node -> Sum node
sum sec dir = Idx.InPart sec . TopoIdx.Sum dir


powerFromPPos :: Idx.State -> TopoIdx.PPos node -> Power node
powerFromPPos state (TopoIdx.PPos e) = Idx.InPart state $ TopoIdx.Power e


initSection :: Idx.Init Idx.State
initSection = Idx.Init

exitSection :: Idx.Exit Idx.State
exitSection = Idx.Exit
