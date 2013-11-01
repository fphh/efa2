module EFA.Flow.Storage.Index where

import qualified EFA.Graph.Topology.Index as Idx

import Prelude hiding (flip)


class Flip edge where
   flip :: edge -> edge


instance Flip idx => Flip (Idx.ForStorage idx node) where
   flip (Idx.ForStorage idx n) = Idx.ForStorage (flip idx) n

instance Flip (X part) where
   flip (X x) = X $ flip x

instance Flip (Idx.CarryBond part) where
   flip (Idx.CarryBond p0 p1) = Idx.CarryBond p1 p0



newtype Content = Content Idx.Boundary deriving (Show, Ord, Eq)

{- |
Energy variables for hypothetical outgoing energies.
At carry edges they describe the maximum energy
that a storage could deliver.
-}
newtype MaxEnergy = MaxEnergy (Idx.CarryEdge Idx.Section) deriving (Show, Ord, Eq)

newtype Energy sec = Energy (Idx.CarryEdge sec) deriving (Show, Ord, Eq)

newtype X sec = X (Idx.CarryBond sec) deriving (Show, Ord, Eq)

data InSum sec = InSum (Idx.Exit sec) deriving (Show, Ord, Eq)

data OutSum sec = OutSum (Idx.Init sec) deriving (Show, Ord, Eq)
