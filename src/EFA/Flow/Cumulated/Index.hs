module EFA.Flow.Cumulated.Index (
   Energy(..),
   Power(..),
   X(..),
   Eta(..),
   Sum(..),
   DTime(..),

   Direction(..),
   TopologyEdge(..),
   ) where

import EFA.Graph.Topology.Index
          (Eta(..), Sum(..), Direction(..), TopologyEdge(..))


data Energy node = Energy Direction (TopologyEdge node) deriving (Show, Ord, Eq)

data Power node = Power Direction (TopologyEdge node) deriving (Show, Ord, Eq)

data X node = X Direction (TopologyEdge node) deriving (Show, Ord, Eq)

newtype DTime node = DTime (TopologyEdge node) deriving (Show, Ord, Eq)
