module EFA.Flow.Sequence.Index where

import qualified EFA.Graph.Topology.Index as Idx

import Prelude hiding (sum)


type Energy node    = Idx.InSection Idx.Energy node
type Power node     = Idx.InSection Idx.Power node
type Eta node       = Idx.InSection Idx.Eta node
type X node         = Idx.InSection Idx.X node
type DTime node     = Idx.InSection Idx.DTime node
type Sum node       = Idx.InSection Idx.Sum node

type Storage node   = Idx.ForStorage Idx.Storage node
type MaxEnergy node = Idx.ForStorage Idx.MaxEnergy node
type StEnergy node  = Idx.ForStorage (Idx.StEnergy Idx.Section) node
type StX node       = Idx.ForStorage (Idx.StX Idx.Section) node
type StInSum node   = Idx.ForStorage (Idx.StInSum Idx.Section) node
type StOutSum node  = Idx.ForStorage (Idx.StOutSum Idx.Section) node

type PPos = Idx.PPos

type CarryEdge = Idx.CarryEdge Idx.Section
type CarryBond = Idx.CarryBond Idx.Section


energy :: Idx.Section -> node -> node -> Energy node
power :: Idx.Section -> node -> node -> Power node
eta :: Idx.Section -> node -> node -> Eta node
x :: Idx.Section -> node -> node -> X node

energy    = topologyEdge Idx.Energy
power     = topologyEdge Idx.Power
eta       = topologyEdge Idx.Eta
x         = topologyEdge Idx.X

topologyEdge ::
   (Idx.TopologyEdge node -> idx node) ->
   Idx.Section -> node -> node -> Idx.InSection idx node
topologyEdge mkIdx s from to =
   Idx.InPart s $ mkIdx $ Idx.TopologyEdge from to


dTime :: Idx.Section -> DTime node
dTime sec = Idx.InPart sec Idx.DTime

sum :: Idx.Section -> Idx.Direction -> node -> Sum node
sum sec dir = Idx.InPart sec . Idx.Sum dir

inSum, outSum :: Idx.Section -> node -> Sum node
inSum  = flip sum Idx.In
outSum = flip sum Idx.Out


maxEnergy ::
   (Idx.ToInitOrSection from, Idx.ToSectionOrExit to) =>
   from -> to -> node -> MaxEnergy node

stEnergy ::
   (Idx.ToInitOrSection from, Idx.ToSectionOrExit to) =>
   from -> to -> node -> StEnergy node

stX ::
   (Idx.ToAugmentedSection from, Idx.ToAugmentedSection to) =>
   from -> to -> node -> StX node

maxEnergy = carryEdge Idx.MaxEnergy
stEnergy  = carryEdge Idx.StEnergy
stX       = carryBond Idx.StX

carryEdge ::
   (Idx.ToInitOrSection from, Idx.ToSectionOrExit to) =>
   (CarryEdge node -> idx node) ->
   from -> to -> node -> Idx.ForStorage idx node
carryEdge mkIdx a b =
   Idx.carryEdge mkIdx (Idx.initOrSection a) (Idx.sectionOrExit b)

carryBond ::
   (Idx.ToAugmentedSection from, Idx.ToAugmentedSection to) =>
   (CarryBond node -> idx node) ->
   from -> to -> node -> Idx.ForStorage idx node
carryBond mkIdx a b =
   Idx.carryBond mkIdx (Idx.augmentSection a) (Idx.augmentSection b)


stInSum ::
   (Idx.ToSectionOrExit sec) =>
   sec -> node -> StInSum node
stInSum sec =
   Idx.ForStorage (Idx.StInSum (Idx.sectionOrExit sec))

stOutSum ::
   (Idx.ToInitOrSection sec) =>
   sec -> node -> StOutSum node
stOutSum sec =
   Idx.ForStorage (Idx.StOutSum (Idx.initOrSection sec))


storage :: Idx.Boundary -> node -> Storage node
storage = Idx.ForStorage . Idx.Storage

ppos :: node -> node -> Idx.PPos node
ppos a b = Idx.PPos $ Idx.TopologyEdge a b

powerFromPPos :: Idx.Section -> Idx.PPos node -> Power node
powerFromPPos sec (Idx.PPos e) = Idx.InPart sec $ Idx.Power e

energyFromPPos :: Idx.Section -> Idx.PPos node -> Energy node
energyFromPPos sec (Idx.PPos e) = Idx.InPart sec $ Idx.Energy e


initSection :: Idx.InitOrSection
initSection = Idx.Init

exitSection :: Idx.SectionOrExit
exitSection = Idx.Exit
