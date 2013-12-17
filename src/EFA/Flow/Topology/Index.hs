module EFA.Flow.Topology.Index where

import qualified EFA.Graph as Graph

import qualified EFA.Utility.TypeConstructor as TC

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)

import qualified Test.QuickCheck as QC
import Control.Monad (liftM2)

import qualified Prelude as P
import Prelude hiding (init, flip, sum)


-- | Energy variables
newtype Energy node = Energy (Position node) deriving (Show, Ord, Eq)

-- | Power variables
newtype Power node = Power (Position node) deriving (Show, Ord, Eq)

-- | Efficiency variables
newtype Eta node = Eta (Position node) deriving (Show, Ord, Eq)

-- | Splitting factors
newtype X node = X (Position node) deriving (Show, Ord, Eq)

data Direction = In | Out deriving (Show, Eq, Ord)

data Sum node = Sum Direction node deriving (Show, Ord, Eq)


-- | Delta time variables
data DTime node = DTime deriving (Show, Ord, Eq)

-- | Sum of split factor signals
data One node = One deriving (Show, Ord, Eq)

-- | Index for Measurement Position
data Position node = Position node node deriving (Show, Read, Eq, Ord)


instance TC.Eq Position where eq = (==)
instance TC.Ord Position where cmp = compare
instance TC.Show Position where showsPrec = showsPrec

instance (QC.Arbitrary node) => QC.Arbitrary (Position node) where
   arbitrary = liftM2 Position QC.arbitrary QC.arbitrary
   shrink (Position from to) =
      map (uncurry Position) $ QC.shrink (from, to)


energy :: node -> node -> Energy node
power :: node -> node -> Power node
eta :: node -> node -> Eta node
x :: node -> node -> X node

energy    = position Energy
power     = position Power
eta       = position Eta
x         = position X

position ::
   (Position node -> idx node) ->
   node -> node -> idx node
position mkIdx from to =
   mkIdx $ Position from to


dTime :: DTime node
dTime = DTime

sum :: Direction -> node -> Sum node
sum dir = Sum dir

inSum, outSum :: node -> Sum node
inSum  = sum In
outSum = sum Out


edge :: node -> node -> Graph.DirEdge node
edge = Graph.DirEdge

ppos :: node -> node -> Position node
ppos = Position

powerFromPosition :: Position node -> Power node
powerFromPosition = Power

energyFromPosition :: Position node -> Energy node
energyFromPosition = Energy


class Identifier idx where
   identifier :: Format output => idx node -> output

instance Identifier Energy where identifier _ = Format.energy
instance Identifier Power where identifier _ = Format.power
instance Identifier Eta where identifier _ = Format.eta
instance Identifier X where identifier _ = Format.xfactor


formatDirection :: Format output => Direction -> output
formatDirection d =
   case d of
      In  -> Format.directionIn
      Out -> Format.directionOut
