module EFA.Flow.Cumulated.Index (
   Energy(..),
   Power(..),
   X(..),
   Eta(..),
   Sum(..),
   DTime(..),

   Direction(..),
   Graph.DirEdge(..),
   TopoIdx.formatDirection,

   inEnergy, outEnergy,
   inPower, outPower,
   inX, outX,
   eta, dTime,
   TopoIdx.inSum, TopoIdx.outSum,
   ) where

import qualified EFA.Flow.Topology.Index as TopoIdx
import EFA.Flow.Topology.Index (Sum(..), Direction(..))

import qualified EFA.Graph as Graph


data Energy node = Energy Direction (Graph.DirEdge node) deriving (Show, Ord, Eq)

data Power node = Power Direction (Graph.DirEdge node) deriving (Show, Ord, Eq)

data X node = X Direction (Graph.DirEdge node) deriving (Show, Ord, Eq)

newtype Eta node = Eta (Graph.DirEdge node) deriving (Show, Ord, Eq)

newtype DTime node = DTime (Graph.DirEdge node) deriving (Show, Ord, Eq)


edge ::
   (Graph.DirEdge node -> idx) ->
   node -> node -> idx
edge mkIdx from to = mkIdx $ Graph.DirEdge from to


inEnergy, outEnergy :: node -> node -> Energy node
inEnergy  = edge (Energy In)
outEnergy = edge (Energy Out)

inPower, outPower :: node -> node -> Power node
inPower  = edge (Power In)
outPower = edge (Power Out)

inX, outX :: node -> node -> X node
inX  = edge (X In)
outX = edge (X Out)


eta :: node -> node -> Eta node
eta = edge Eta

dTime :: node -> node -> DTime node
dTime = edge DTime
