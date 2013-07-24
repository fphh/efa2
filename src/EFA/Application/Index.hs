module EFA.Application.Index where

import qualified EFA.Graph.Topology.Index as Idx

import Prelude hiding (sum)


type Energy node    = Idx.InSection Idx.Energy node
type Power node     = Idx.InSection Idx.Power node
type Eta node       = Idx.InSection Idx.Eta node
type X node         = Idx.InSection Idx.X node
type DTime node     = Idx.InSection Idx.DTime node
type Sum node       = Idx.InSection Idx.Sum node

type Storage node   = Idx.ForNode Idx.Storage node
type MaxEnergy node = Idx.ForNode Idx.MaxEnergy node
type StEnergy node  = Idx.ForNode (Idx.StEnergy Idx.Section) node
type StX node       = Idx.ForNode (Idx.StX Idx.Section) node
type StInSum node   = Idx.ForNode (Idx.StInSum Idx.Section) node
type StOutSum node  = Idx.ForNode (Idx.StOutSum Idx.Section) node

type PPos = Idx.PPos

type StorageEdge  = Idx.StorageEdge  Idx.Section
type StorageTrans = Idx.StorageTrans Idx.Section


energy :: Idx.Section -> node -> node -> Energy node
power :: Idx.Section -> node -> node -> Power node
eta :: Idx.Section -> node -> node -> Eta node
x :: Idx.Section -> node -> node -> X node

energy    = structureEdge Idx.Energy
power     = structureEdge Idx.Power
eta       = structureEdge Idx.Eta
x         = structureEdge Idx.X

structureEdge ::
   (Idx.StructureEdge node -> idx node) ->
   Idx.Section -> node -> node -> Idx.InSection idx node
structureEdge mkIdx s from to =
   Idx.InPart s $ mkIdx $ Idx.StructureEdge from to


dTime :: Idx.Section -> DTime node
dTime sec = Idx.InPart sec Idx.DTime

sum :: Idx.Section -> Idx.Direction -> node -> Sum node
sum sec dir = Idx.InPart sec . Idx.Sum dir


maxEnergy ::
   (Idx.ToInitOrSection from, Idx.ToSectionOrExit to) =>
   from -> to -> node -> MaxEnergy node

stEnergy ::
   (Idx.ToInitOrSection from, Idx.ToSectionOrExit to) =>
   from -> to -> node -> StEnergy node

stX ::
   (Idx.ToAugmentedSection from, Idx.ToAugmentedSection to) =>
   from -> to -> node -> StX node

maxEnergy = storageEdge Idx.MaxEnergy
stEnergy  = storageEdge Idx.StEnergy
stX       = storageTrans Idx.StX

storageEdge ::
   (Idx.ToInitOrSection from, Idx.ToSectionOrExit to) =>
   (StorageEdge node -> idx node) ->
   from -> to -> node -> Idx.ForNode idx node
storageEdge mkIdx a b =
   Idx.storageEdge mkIdx (Idx.initOrSection a) (Idx.sectionOrExit b)

storageTrans ::
   (Idx.ToAugmentedSection from, Idx.ToAugmentedSection to) =>
   (StorageTrans node -> idx node) ->
   from -> to -> node -> Idx.ForNode idx node
storageTrans mkIdx a b =
   Idx.storageTrans mkIdx (Idx.augmentSection a) (Idx.augmentSection b)


stInSum ::
   (Idx.ToSectionOrExit sec) =>
   sec -> node -> StInSum node
stInSum sec =
   Idx.ForNode (Idx.StInSum (Idx.sectionOrExit sec))

stOutSum ::
   (Idx.ToInitOrSection sec) =>
   sec -> node -> StOutSum node
stOutSum sec =
   Idx.ForNode (Idx.StOutSum (Idx.initOrSection sec))


storage :: Idx.Boundary -> node -> Storage node
storage = Idx.ForNode . Idx.Storage

ppos :: node -> node -> Idx.PPos node
ppos a b = Idx.PPos $ Idx.StructureEdge a b


initSection :: Idx.InitOrSection
initSection = Idx.Init

exitSection :: Idx.SectionOrExit
exitSection = Idx.Exit
