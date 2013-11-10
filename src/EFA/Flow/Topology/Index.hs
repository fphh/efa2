module EFA.Flow.Topology.Index where

import qualified EFA.Utility.TypeConstructor as TC

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)

import qualified Test.QuickCheck as QC
import Control.Monad (liftM2)

import qualified Prelude as P
import Prelude hiding (init, flip, sum)


-- | Energy variables
newtype Energy node = Energy (Edge node) deriving (Show, Ord, Eq)

-- | Power variables
newtype Power node = Power (Edge node) deriving (Show, Ord, Eq)

-- | Efficiency variables
newtype Eta node = Eta (Edge node) deriving (Show, Ord, Eq)

-- | Splitting factors
newtype X node = X (Edge node) deriving (Show, Ord, Eq)

data Direction = In | Out deriving (Show, Eq, Ord)

data Sum node = Sum Direction node deriving (Show, Ord, Eq)


-- | Delta time variables
data DTime node = DTime deriving (Show, Ord, Eq)

-- | Indices for Power Position
newtype Position node = Position (Edge node) deriving (Show, Read, Ord, Eq)


data Edge node = Edge node node
   deriving (Show, Read, Eq, Ord)


class Flip edge where
   flip :: edge node -> edge node

instance Flip Edge where
   flip (Edge from to) = Edge to from


instance Flip Power where
   flip (Power p) = Power $ flip p

instance Flip Energy where
   flip (Energy p) = Energy $ flip p

instance Flip Position where
   flip (Position p) = Position $ flip p


instance TC.Eq Edge where eq = (==)
instance TC.Ord Edge where cmp = compare
instance TC.Show Edge where showsPrec = showsPrec

instance (QC.Arbitrary node) => QC.Arbitrary (Edge node) where
   arbitrary = liftM2 Edge QC.arbitrary QC.arbitrary
   shrink (Edge from to) =
      map (uncurry Edge) $ QC.shrink (from, to)

instance (QC.Arbitrary node) => QC.Arbitrary (Position node) where
   arbitrary = fmap Position QC.arbitrary
   shrink (Position p) = map Position $ QC.shrink p


energy :: node -> node -> Energy node
power :: node -> node -> Power node
eta :: node -> node -> Eta node
x :: node -> node -> X node

energy    = edge Energy
power     = edge Power
eta       = edge Eta
x         = edge X

edge ::
   (Edge node -> idx node) ->
   node -> node -> idx node
edge mkIdx from to =
   mkIdx $ Edge from to


dTime :: DTime node
dTime = DTime

sum :: Direction -> node -> Sum node
sum dir = Sum dir

inSum, outSum :: node -> Sum node
inSum  = sum In
outSum = sum Out


ppos :: node -> node -> Position node
ppos a b = Position $ Edge a b

powerFromPosition :: Position node -> Power node
powerFromPosition (Position e) = Power e

energyFromPosition :: Position node -> Energy node
energyFromPosition (Position e) = Energy e


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
