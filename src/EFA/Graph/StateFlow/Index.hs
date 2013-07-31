module EFA.Graph.StateFlow.Index where

import qualified EFA.Graph.Topology.Index as Idx

import Prelude hiding (sum)


type Energy node    = Idx.InState Idx.Energy node
type Power node     = Idx.InState Idx.Power node
type Eta node       = Idx.InState Idx.Eta node
type X node         = Idx.InState Idx.X node
type DTime node     = Idx.InState Idx.DTime node
type Sum node       = Idx.InState Idx.Sum node

type Storage node   = Idx.ForNode Idx.Storage node
type MaxEnergy node = Idx.ForNode Idx.MaxEnergy node
type StEnergy node  = Idx.ForNode (Idx.StEnergy Idx.State) node
type StX node       = Idx.ForNode (Idx.StX Idx.State) node
type StInSum node   = Idx.ForNode (Idx.StInSum Idx.State) node
type StOutSum node  = Idx.ForNode (Idx.StOutSum Idx.State) node

type PPos = Idx.PPos

type StorageEdge  = Idx.StorageEdge  Idx.State
type StorageTrans = Idx.StorageTrans Idx.State


energy :: Idx.State -> node -> node -> Energy node
power :: Idx.State -> node -> node -> Power node
eta :: Idx.State -> node -> node -> Eta node
x :: Idx.State -> node -> node -> X node

energy    = structureEdge Idx.Energy
power     = structureEdge Idx.Power
eta       = structureEdge Idx.Eta
x         = structureEdge Idx.X

structureEdge ::
   (Idx.StructureEdge node -> idx node) ->
   Idx.State -> node -> node -> Idx.InState idx node
structureEdge mkIdx s from to =
   Idx.InPart s $ mkIdx $ Idx.StructureEdge from to

stx ::
   Idx.PartNode (Idx.StorageTrans sec node) node ->
   Idx.ForNode (Idx.StX sec) node
stx = Idx.forNode Idx.StX


dTime :: Idx.State -> DTime node
dTime sec = Idx.InPart sec Idx.DTime

sum :: Idx.State -> Idx.Direction -> node -> Sum node
sum sec dir = Idx.InPart sec . Idx.Sum dir


ppos :: node -> node -> Idx.PPos node
ppos a b = Idx.PPos $ Idx.StructureEdge a b


initSection :: Idx.Init Idx.State
initSection = Idx.Init

exitSection :: Idx.Exit Idx.State
exitSection = Idx.Exit
