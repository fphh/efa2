module EFA.Flow.Storage.Index where

import qualified EFA.Graph.Topology.Index as Idx

import Prelude hiding (flip)


class Flip edge where
   flip :: edge -> edge


instance Flip idx => Flip (Idx.ForStorage idx node) where
   flip (Idx.ForStorage idx n) = Idx.ForStorage (flip idx) n

instance Flip (Idx.StX part) where
   flip (Idx.StX x) = Idx.StX $ flip x

instance Flip (Idx.CarryBond part) where
   flip (Idx.CarryBond p0 p1) = Idx.CarryBond p1 p0
