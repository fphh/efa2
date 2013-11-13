module EFA.Flow.Cumulated.Index (
   Energy(..),
   Power(..),
   X(..),
   Eta(..),
   Sum(..),
   DTime(..),

   Direction(..),
   Graph.DirEdge(..),
   TopoIdx.flip,
   TopoIdx.formatDirection,
   ) where

import qualified EFA.Flow.Topology.Index as TopoIdx
import EFA.Flow.Topology.Index (Sum(..), Direction(..))

import qualified EFA.Graph as Graph


data Energy node = Energy Direction (Graph.DirEdge node) deriving (Show, Ord, Eq)

data Power node = Power Direction (Graph.DirEdge node) deriving (Show, Ord, Eq)

data X node = X Direction (Graph.DirEdge node) deriving (Show, Ord, Eq)

newtype Eta node = Eta (Graph.DirEdge node) deriving (Show, Ord, Eq)

newtype DTime node = DTime (Graph.DirEdge node) deriving (Show, Ord, Eq)
