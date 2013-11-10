module EFA.Flow.Cumulated.Index (
   Energy(..),
   Power(..),
   X(..),
   Eta(..),
   Sum(..),
   DTime(..),

   Direction(..),
   Position(..),
   TopoIdx.flip,
   TopoIdx.formatDirection,
   ) where

import qualified EFA.Flow.Topology.Index as TopoIdx
import EFA.Flow.Topology.Index (Eta(..), Sum(..), Direction(..), Position(..))


data Energy node = Energy Direction (Position node) deriving (Show, Ord, Eq)

data Power node = Power Direction (Position node) deriving (Show, Ord, Eq)

data X node = X Direction (Position node) deriving (Show, Ord, Eq)

newtype DTime node = DTime (Position node) deriving (Show, Ord, Eq)
