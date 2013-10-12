module EFA.Flow.Topology.Index where

import qualified EFA.Graph.Topology.Index as Idx

import Prelude hiding (sum)


type Energy node    = Idx.Energy node
type Power node     = Idx.Power node
type Eta node       = Idx.Eta node
type X node         = Idx.X node
type DTime node     = Idx.DTime node
type Sum node       = Idx.Sum node

type PPos = Idx.PPos

type StorageEdge  = Idx.StorageEdge  Idx.Section
type StorageTrans = Idx.StorageTrans Idx.Section


energy :: node -> node -> Energy node
power :: node -> node -> Power node
eta :: node -> node -> Eta node
x :: node -> node -> X node

energy    = structureEdge Idx.Energy
power     = structureEdge Idx.Power
eta       = structureEdge Idx.Eta
x         = structureEdge Idx.X

structureEdge ::
   (Idx.StructureEdge node -> idx node) ->
   node -> node -> idx node
structureEdge mkIdx from to =
   mkIdx $ Idx.StructureEdge from to


dTime :: DTime node
dTime = Idx.DTime

sum :: Idx.Direction -> node -> Sum node
sum dir = Idx.Sum dir

inSum, outSum :: node -> Sum node
inSum  = sum Idx.In
outSum = sum Idx.Out


ppos :: node -> node -> Idx.PPos node
ppos a b = Idx.PPos $ Idx.StructureEdge a b

powerFromPPos :: Idx.PPos node -> Power node
powerFromPPos (Idx.PPos e) = Idx.Power e

energyFromPPos :: Idx.PPos node -> Energy node
energyFromPPos (Idx.PPos e) = Idx.Energy e
