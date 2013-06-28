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
type StInSum node   = Idx.ForNode Idx.StInSum node
type StOutSum node  = Idx.ForNode Idx.StOutSum node

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


maxEnergy ::
   (Idx.ToInitOrSection from, Idx.ToSectionOrExit to) =>
   from -> to -> node -> MaxEnergy node

stEnergy ::
   (Idx.ToAugmentedSection from, Idx.ToAugmentedSection to) =>
   from -> to -> node -> StEnergy node

stX ::
   (Idx.ToAugmentedSection from, Idx.ToAugmentedSection to) =>
   from -> to -> node -> StX node

maxEnergy = storageEdge Idx.MaxEnergy
stEnergy  = storageTrans Idx.StEnergy
stX       = storageTrans Idx.StX

storageEdge ::
   (Idx.ToInitOrSection from, Idx.ToSectionOrExit to) =>
   (Idx.StorageEdge node -> idx node) ->
   from -> to -> node -> Idx.ForNode idx node
storageEdge mkIdx a b =
   Idx.storageEdge mkIdx (Idx.initOrSection a) (Idx.sectionOrExit b)

storageTrans ::
   (Idx.ToAugmentedSection from, Idx.ToAugmentedSection to) =>
   (Idx.StorageTrans node -> idx node) ->
   from -> to -> node -> Idx.ForNode idx node
storageTrans mkIdx a b =
   Idx.storageTrans mkIdx (Idx.augmentSection a) (Idx.augmentSection b)


stInSum ::
   (Idx.ToInitOrSection sec) =>
   sec -> node -> StInSum node
stInSum sec =
   Idx.ForNode (Idx.StInSum (fmap Idx.NoExit $ Idx.initOrSection sec))
--   Idx.ForNode (Idx.StInSum (Idx.initOrSection sec))

stOutSum ::
   (Idx.ToSectionOrExit sec) =>
   sec -> node -> StOutSum node
stOutSum sec =
   Idx.ForNode (Idx.StOutSum (Idx.NoInit $ Idx.sectionOrExit sec))
--   Idx.ForNode (Idx.StOutSum (Idx.sectionOrExit sec))


storage :: Idx.Boundary -> node -> Storage node
storage = Idx.ForNode . Idx.Storage

ppos :: node -> node -> Idx.PPos node
ppos a b = Idx.PPos $ Idx.StructureEdge a b


initSection :: Idx.InitOrSection
initSection = Idx.Init

exitSection :: Idx.SectionOrExit
exitSection = Idx.Exit
