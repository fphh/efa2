module EFA.Graph.Topology.Index where

import qualified Test.QuickCheck as QC


newtype Section = Section Int deriving (Show, Eq, Ord)

instance Enum Section where
   fromEnum (Section n) = n
   toEnum n = Section n

initSection :: Section
initSection = Section (-1)



data Absolute = Absolute deriving (Eq, Ord)

instance Show Absolute where
         show _ = ""

data Differential = Differential deriving (Show, Eq, Ord)



newtype Record = Record Absolute deriving (Show, Eq, Ord)

newtype Node = Node Int deriving (Show, Eq, Ord)

instance Enum Node where
   fromEnum (Node n) = n
   toEnum n = Node n

rootNode :: Node
rootNode = Node (-1)

instance QC.Arbitrary Node where
   arbitrary = fmap Node $ QC.choose (0,10)
   shrink (Node n) = map Node $ QC.shrink n

data SecNode = SecNode Section Node deriving (Show, Eq, Ord)


-- * Edge indices

-- | Variable types of the solver. The solver, in fact, is
-- ignorant of the provenance of the variables. However, to
-- facilitate life, we introduce variable types, that make
-- it easy to express things needed in energy flow analysis,
-- that is:
--
-- * a data record number
-- * two node identifiers to specify a place in the topology

-- | Energy variables.
data Energy = Energy !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DEnergy = DEnergy !Record !SecNode !SecNode deriving (Show, Ord, Eq)


-- | Energy variables for hypothetical outgoing energies.
-- At intersection edges they describe the maximum energy
-- that a storage could deliver.
data MaxEnergy = MaxEnergy !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DMaxEnergy = DMaxEnergy !Record !SecNode !SecNode deriving (Show, Ord, Eq)

-- | Power variables.
data Power = Power !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DPower = DPower !Record !SecNode !SecNode deriving (Show, Ord, Eq)

-- | Eta variables.
data Eta = Eta !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DEta = DEta !Record !SecNode !SecNode deriving (Show, Ord, Eq)

-- | Splitting factors.
data X = X !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DX = DX !Record !SecNode !SecNode deriving (Show, Ord, Eq)

-- | Strange factors for outgoing intersection edges.
data Y = Y !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DY = DY !Record !SecNode !SecNode deriving (Show, Ord, Eq)

data Storage = Storage !Record !SecNode deriving (Show, Ord, Eq)

data Use = InSum
         | OutSum deriving (Show, Eq, Ord)

data Var = Var !Record Use !SecNode deriving (Show, Ord, Eq)


-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime = DTime !Record !Section deriving (Show, Ord, Eq)


class RecNum a where
   getRecNum :: a -> Record
   setRecNum :: Record -> a -> a

instance RecNum Energy where
   getRecNum (Energy r _ _) = r
   setRecNum rec (Energy _ f t) = Energy rec f t

instance RecNum DEnergy where
   getRecNum (DEnergy r _ _) = r
   setRecNum rec (DEnergy _ f t) = DEnergy rec f t

instance RecNum Power where
   getRecNum (Power r _ _) = r
   setRecNum rec (Power _ f t) = Power rec f t

instance RecNum DPower where
   getRecNum (DPower r _ _) = r
   setRecNum rec (DPower _ f t) = DPower rec f t

instance RecNum Eta where
   getRecNum (Eta r _ _) = r
   setRecNum rec (Eta _ f t) = Eta rec f t

instance RecNum DEta where
   getRecNum (DEta r _ _) = r
   setRecNum rec (DEta _ f t) = DEta rec f t

instance RecNum X where
   getRecNum (X r _ _) = r
   setRecNum rec (X _ f t) = X rec f t

instance RecNum DX where
   getRecNum (DX r _ _) = r
   setRecNum rec (DX _ f t) = DX rec f t

instance RecNum DTime where
   getRecNum (DTime r _) = r
   setRecNum rec (DTime _ s) = DTime rec s

instance RecNum Storage where
   getRecNum (Storage r _) = r
   setRecNum rec (Storage _ n) = Storage rec n

instance RecNum Var where
   getRecNum (Var r _ _) = r
   setRecNum rec (Var _ use t) = Var rec use t
