module EFA.Flow.Cumulated.Index (
   Energy(..),
   Power(..),
   X(..),
   Eta(..),
   Sum(..),
   DTime(..),

   Direction(..),
   StructureEdge(..),
   ) where

import EFA.Graph.Topology.Index
          (Eta(..), Sum(..), Direction(..), StructureEdge(..))


data Energy node = Energy Direction (StructureEdge node) deriving (Show, Ord, Eq)

data Power node = Power Direction (StructureEdge node) deriving (Show, Ord, Eq)

data X node = X Direction (StructureEdge node) deriving (Show, Ord, Eq)

newtype DTime node = DTime (StructureEdge node) deriving (Show, Ord, Eq)
