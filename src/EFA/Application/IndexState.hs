module EFA.Application.IndexState where

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


dTime :: Idx.State -> DTime node
dTime sec = Idx.InPart sec Idx.DTime

sum :: Idx.State -> Idx.Direction -> node -> Sum node
sum sec dir = Idx.InPart sec . Idx.Sum dir


{-
maxEnergy ::
   (Idx.ToInitOrState from, Idx.ToStateOrExit to) =>
   from -> to -> node -> MaxEnergy node



stEnergy ::
   (Idx.ToInitOrState from, Idx.ToStateOrExit to) =>
   from -> to -> node -> StEnergy node

stX ::
   (Idx.ToAugmentedState from, Idx.ToAugmentedState to) =>
   from -> to -> node -> StX node
-}

stEnergy ::
  Idx.Init sec ->
  Idx.Exit sec ->
  node ->
  Idx.ForNode (Idx.StEnergy sec) node
stEnergy  = Idx.storageEdge Idx.StEnergy

stX ::
  Idx.Augmented sec ->
  Idx.Augmented sec ->
  node ->
  Idx.ForNode (Idx.StX sec) node
stX       = Idx.storageTrans Idx.StX

maxEnergy ::
  Idx.Init Idx.Section ->
  Idx.Exit Idx.Section ->
  node ->
  Idx.ForNode Idx.MaxEnergy node
maxEnergy = Idx.storageEdge Idx.MaxEnergy

{-
storageEdge ::
   (Idx.ToInitOrState from, Idx.ToStateOrExit to) =>
   (StorageEdge node -> idx node) ->
   from -> to -> node -> Idx.ForNode idx node
storageEdge mkIdx a b =
   Idx.storageEdge mkIdx (Idx.initOrState a) (Idx.sectionOrExit b)

storageTrans ::
   (Idx.ToAugmentedState from, Idx.ToAugmentedState to) =>
   (StorageTrans node -> idx node) ->
   from -> to -> node -> Idx.ForNode idx node
storageTrans mkIdx a b =
   Idx.storageTrans mkIdx (Idx.augmentState a) (Idx.augmentState b)


stInSum ::
   (Idx.ToStateOrExit sec) =>
   state -> node -> StInSum node
stInSum state =
   Idx.ForNode (Idx.StInSum (Idx.stateOrExit sec))

stOutSum ::
   (Idx.ToInitOrState sec) =>
   state -> node -> StOutSum node
stOutSum state =
   Idx.ForNode (Idx.StOutSum (Idx.initOrState sec))
-}

storage :: Idx.Boundary -> node -> Storage node
storage = Idx.ForNode . Idx.Storage

ppos :: node -> node -> Idx.PPos node
ppos a b = Idx.PPos $ Idx.StructureEdge a b

{-
initState :: Idx.InitOrState
initState = Idx.Init

exitState :: Idx.StateOrExit
exitState = Idx.Exit
-}


