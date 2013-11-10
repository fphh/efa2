module EFA.Flow.Sequence.Index where

import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Part.Index as PartIdx
import qualified EFA.Flow.SequenceState.Index as Idx

import Prelude hiding (sum)


type Energy node    = Idx.InSection TopoIdx.Energy node
type Power node     = Idx.InSection TopoIdx.Power node
type Eta node       = Idx.InSection TopoIdx.Eta node
type X node         = Idx.InSection TopoIdx.X node
type DTime node     = Idx.InSection TopoIdx.DTime node
type Sum node       = Idx.InSection TopoIdx.Sum node

type Storage node   = Idx.ForStorage StorageIdx.Content node
type MaxEnergy node = Idx.ForStorage StorageIdx.MaxEnergy node
type StEnergy node  = Idx.ForStorage (StorageIdx.Energy Idx.Section) node
type StX node       = Idx.ForStorage (StorageIdx.X Idx.Section) node
type StInSum node   = Idx.ForStorage (StorageIdx.InSum Idx.Section) node
type StOutSum node  = Idx.ForStorage (StorageIdx.OutSum Idx.Section) node

type CarryEdge = StorageIdx.Edge Idx.Section
type CarryBond = StorageIdx.Bond Idx.Section


energy :: Idx.Section -> node -> node -> Energy node
power :: Idx.Section -> node -> node -> Power node
eta :: Idx.Section -> node -> node -> Eta node
x :: Idx.Section -> node -> node -> X node

energy    = topologyEdge TopoIdx.Energy
power     = topologyEdge TopoIdx.Power
eta       = topologyEdge TopoIdx.Eta
x         = topologyEdge TopoIdx.X

topologyEdge ::
   (TopoIdx.Edge node -> idx node) ->
   Idx.Section -> node -> node -> Idx.InSection idx node
topologyEdge mkIdx s from to =
   Idx.InPart s $ mkIdx $ TopoIdx.Edge from to


dTime :: Idx.Section -> DTime node
dTime sec = Idx.InPart sec TopoIdx.DTime

sum :: Idx.Section -> TopoIdx.Direction -> node -> Sum node
sum sec dir = Idx.InPart sec . TopoIdx.Sum dir

inSum, outSum :: Idx.Section -> node -> Sum node
inSum  = flip sum TopoIdx.In
outSum = flip sum TopoIdx.Out


maxEnergy ::
   (PartIdx.ToInitOrSection from, PartIdx.ToSectionOrExit to) =>
   from -> to -> node -> MaxEnergy node

stEnergy ::
   (PartIdx.ToInitOrSection from, PartIdx.ToSectionOrExit to) =>
   from -> to -> node -> StEnergy node

stX ::
   (PartIdx.ToAugmentedSection from, PartIdx.ToAugmentedSection to) =>
   from -> to -> node -> StX node

maxEnergy = carryEdge StorageIdx.MaxEnergy
stEnergy  = carryEdge StorageIdx.Energy
stX       = carryBond StorageIdx.X

carryEdge ::
   (PartIdx.ToInitOrSection from, PartIdx.ToSectionOrExit to) =>
   (CarryEdge -> idx) ->
   from -> to -> node -> Idx.ForStorage idx node
carryEdge mkIdx a b =
   Idx.carryEdge mkIdx (PartIdx.initOrSection a) (PartIdx.sectionOrExit b)

carryBond ::
   (PartIdx.ToAugmentedSection from, PartIdx.ToAugmentedSection to) =>
   (CarryBond -> idx) ->
   from -> to -> node -> Idx.ForStorage idx node
carryBond mkIdx a b =
   Idx.carryBond mkIdx (PartIdx.augmentSection a) (PartIdx.augmentSection b)


stInSum ::
   (PartIdx.ToSectionOrExit sec) =>
   sec -> node -> StInSum node
stInSum sec =
   Idx.ForStorage (StorageIdx.InSum (PartIdx.sectionOrExit sec))

stOutSum ::
   (PartIdx.ToInitOrSection sec) =>
   sec -> node -> StOutSum node
stOutSum sec =
   Idx.ForStorage (StorageIdx.OutSum (PartIdx.initOrSection sec))


storage :: Idx.Boundary -> node -> Storage node
storage = Idx.ForStorage . StorageIdx.Content

powerFromPosition :: Idx.Section -> TopoIdx.Position node -> Power node
powerFromPosition sec (TopoIdx.Position e) = Idx.InPart sec $ TopoIdx.Power e

energyFromPosition :: Idx.Section -> TopoIdx.Position node -> Energy node
energyFromPosition sec (TopoIdx.Position e) = Idx.InPart sec $ TopoIdx.Energy e


initSection :: PartIdx.InitOrSection
initSection = PartIdx.Init

exitSection :: PartIdx.SectionOrExit
exitSection = PartIdx.Exit
