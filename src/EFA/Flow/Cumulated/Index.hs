module EFA.Flow.Cumulated.Index (
   Energy(..),
   Power(..),
   X(..),
   Eta(..),
   Sum(..),
   DTime(..),

   Direction(..),
   Edge(..),
   TopoIdx.flip,
   TopoIdx.formatDirection,
   ) where

import qualified EFA.Flow.Topology.Index as TopoIdx
import EFA.Flow.Topology.Index (Eta(..), Sum(..), Direction(..), Edge(..))


data Energy node = Energy Direction (Edge node) deriving (Show, Ord, Eq)

data Power node = Power Direction (Edge node) deriving (Show, Ord, Eq)

data X node = X Direction (Edge node) deriving (Show, Ord, Eq)

newtype DTime node = DTime (Edge node) deriving (Show, Ord, Eq)
