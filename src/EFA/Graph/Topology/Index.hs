module EFA.Graph.Topology.Index where

import Prelude hiding (flip)


newtype Section = Section Int deriving (Show, Eq, Ord)

instance Enum Section where
   fromEnum (Section n) = n
   toEnum n = Section n

initSection :: Section
initSection = Section (-1)



data Absolute = Absolute deriving (Show, Eq, Ord)

data Delta = Delta | Before | After deriving (Show, Eq, Ord)


data Record rec idx = Record rec idx deriving (Show, Eq)

-- this ordering is easier to read than the default one
instance (Ord rec, Ord idx) => Ord (Record rec idx) where
   compare (Record rx ix) (Record ry iy) =
      case compare ix iy of
         EQ -> compare rx ry
         o -> o

instance Functor (Record rec) where
   fmap f (Record rec idx) = Record rec $ f idx

absolute :: idx -> Record Absolute idx
absolute = Record Absolute

delta :: idx -> Record Delta idx
delta = Record Delta

before :: idx -> Record Delta idx
before = Record Before

after :: idx -> Record Delta idx
after = Record After



data SecNode node = SecNode Section node deriving (Show, Eq, Ord)



-- * Edge indices

data StructureEdge node = StructureEdge Section node node
   deriving (Show, Eq, Ord)

data StorageEdge node = StorageEdge Section Section node
   deriving (Show, Eq, Ord)

structureEdge ::
   (StructureEdge node -> idx) ->
   Section -> node -> node -> idx
structureEdge mkIdx s x y =
   mkIdx $ StructureEdge s x y

storageEdge ::
   (StorageEdge node -> idx) ->
   Section -> Section -> node -> idx
storageEdge mkIdx s0 s1 n =
   mkIdx $ StorageEdge s0 s1 n


class Flip edge where
   flip :: edge node -> edge node

instance Flip StructureEdge where
   flip (StructureEdge s x y) = StructureEdge s y x

instance Flip StorageEdge where
   flip (StorageEdge s0 s1 n) = StorageEdge s1 s0 n


-- | Variable types of the solver. The solver, in fact, is
-- ignorant of the provenance of the variables. However, to
-- facilitate life, we introduce variable types, that make
-- it easy to express things needed in energy flow analysis,
-- that is:
--
-- * a data record number
-- * two node identifiers to specify a place in the topology

-- | Energy variables.
data Energy node = Energy (StructureEdge node) deriving (Show, Ord, Eq)

data StEnergy node = StEnergy (StorageEdge node) deriving (Show, Ord, Eq)


-- | Energy variables for hypothetical outgoing energies.
-- At storage edges they describe the maximum energy
-- that a storage could deliver.
data MaxEnergy node = MaxEnergy (StorageEdge node) deriving (Show, Ord, Eq)

-- | Power variables.
data Power node = Power (StructureEdge node) deriving (Show, Ord, Eq)

-- | Eta variables.
data Eta node = Eta (StructureEdge node) deriving (Show, Ord, Eq)

-- | Splitting factors.
data X node = X (StructureEdge node) deriving (Show, Ord, Eq)

data StX node = StX (StorageEdge node) deriving (Show, Ord, Eq)

data Storage node = Storage !(SecNode node) deriving (Show, Ord, Eq)

data Direction = In | Out deriving (Show, Eq, Ord)

data Sum node = Sum !Direction !(SecNode node) deriving (Show, Ord, Eq)


-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime node = DTime !Section deriving (Show, Ord, Eq)
