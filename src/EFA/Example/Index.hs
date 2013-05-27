module EFA.Example.Index where

import qualified EFA.Graph.Topology.Index as Idx

import Prelude hiding (sum)


type Energy node    = Idx.InSection Idx.Energy node
type Power node     = Idx.InSection Idx.Power node
type Eta node       = Idx.InSection Idx.Eta node
type X node         = Idx.InSection Idx.X node
type DTime node     = Idx.InSection Idx.DTime node
type Sum node       = Idx.InSection Idx.Sum node

type MaxEnergy node = Idx.ForNode Idx.MaxEnergy node
type StEnergy node  = Idx.ForNode Idx.StEnergy node
type StX node       = Idx.ForNode Idx.StX node
type Storage node   = Idx.ForNode Idx.Storage node
type StSum node     = Idx.ForNode Idx.StSum node

type PPos = Idx.PPos


energy :: Idx.Section -> node -> node -> Energy node
power :: Idx.Section -> node -> node -> Power node
eta :: Idx.Section -> node -> node -> Eta node
x :: Idx.Section -> node -> node -> X node

energy    = Idx.structureEdge Idx.Energy
power     = Idx.structureEdge Idx.Power
eta       = Idx.structureEdge Idx.Eta
x         = Idx.structureEdge Idx.X

dTime :: Idx.Section -> DTime node
dTime sec = Idx.InSection sec Idx.DTime

sum :: Idx.Section -> Idx.Direction -> node -> Sum node
sum sec dir = Idx.InSection sec . Idx.Sum dir


maxEnergy :: Idx.Boundary -> Idx.Boundary -> node -> MaxEnergy node
stEnergy :: Idx.Boundary -> Idx.Boundary -> node -> StEnergy node
stX :: Idx.Boundary -> Idx.Boundary -> node -> StX node

maxEnergy = Idx.storageEdge Idx.MaxEnergy
stEnergy  = Idx.storageEdge Idx.StEnergy
stX       = Idx.storageEdge Idx.StX

stSum :: Idx.Direction -> Idx.Boundary -> node -> StSum node
stSum dir bnd node = Idx.ForNode (Idx.StSum dir bnd) node

storage :: Idx.Boundary -> node -> Storage node
storage = Idx.ForNode . Idx.Storage

ppos :: node -> node -> Idx.PPos node
ppos a b = Idx.PPos $ Idx.StructureEdge a b
