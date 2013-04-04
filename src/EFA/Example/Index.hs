module EFA.Example.Index (
   module EFA.Example.Index,

   Idx.Energy,
   Idx.Power,
   Idx.Eta,
   Idx.X,
   Idx.DTime,
   Idx.Sum,

   Idx.MaxEnergy,
   Idx.StEnergy,
   Idx.StX,
   Idx.Storage,
   Idx.StSum,
   ) where

import qualified EFA.Graph.Topology.Index as Idx

import Prelude hiding (sum)


energy :: Idx.Section -> node -> node -> Idx.Energy node
power :: Idx.Section -> node -> node -> Idx.Power node
eta :: Idx.Section -> node -> node -> Idx.Eta node
x :: Idx.Section -> node -> node -> Idx.X node

energy    = Idx.structureEdge Idx.Energy
power     = Idx.structureEdge Idx.Power
eta       = Idx.structureEdge Idx.Eta
x         = Idx.structureEdge Idx.X

dTime :: Idx.Section -> Idx.DTime node
dTime sec = Idx.DTime sec

sum :: Idx.Section -> Idx.Direction -> node -> Idx.Sum node
sum sec dir node = Idx.Sum dir $ Idx.SecNode sec node


maxEnergy :: Idx.Boundary -> Idx.Boundary -> node -> Idx.MaxEnergy node
stEnergy :: Idx.Boundary -> Idx.Boundary -> node -> Idx.StEnergy node
stX :: Idx.Boundary -> Idx.Boundary -> node -> Idx.StX node

maxEnergy = Idx.storageEdge Idx.MaxEnergy
stEnergy  = Idx.storageEdge Idx.StEnergy
stX       = Idx.storageEdge Idx.StX

storage :: Idx.Boundary -> node -> Idx.Storage node
storage bnd node = Idx.Storage $ Idx.BndNode bnd node
