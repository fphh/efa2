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

{-
newtype Node = Node Int deriving (Show, Eq, Ord)

instance Enum Node where
   fromEnum (Node n) = n
   toEnum n = Node n

rootNode :: Node
rootNode = Node (-1)

instance QC.Arbitrary Node where
   arbitrary = fmap Node $ QC.choose (0,10)
   shrink (Node n) = map Node $ QC.shrink n
-}

data SecNode a = SecNode Section a deriving (Show, Eq, Ord)


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
data Energy a = Energy !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)
data DEnergy a = DEnergy !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)


-- | Energy variables for hypothetical outgoing energies.
-- At intersection edges they describe the maximum energy
-- that a storage could deliver.
data MaxEnergy a = MaxEnergy !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)
data DMaxEnergy a = DMaxEnergy !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Power variables.
data Power a = Power !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)
data DPower a = DPower !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Eta variables.
data Eta a = Eta !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)
data DEta a = DEta !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Splitting factors.
data X a = X !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)
data DX a = DX !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Strange factors for outgoing intersection edges.
data Y a = Y !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)
data DY a = DY !Record !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

data Storage a = Storage !Record !(SecNode a) deriving (Show, Ord, Eq)

data Use = InSum
         | OutSum deriving (Show, Eq, Ord)

data Var a = Var !Record Use !(SecNode a) deriving (Show, Ord, Eq)


-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime a = DTime !Record !Section deriving (Show, Ord, Eq)


class RecNum a where
   getRecNum :: a -> Record
   setRecNum :: Record -> a -> a

instance RecNum (Energy a) where
   getRecNum (Energy r _ _) = r
   setRecNum rec (Energy _ f t) = Energy rec f t

instance RecNum (DEnergy a) where
   getRecNum (DEnergy r _ _) = r
   setRecNum rec (DEnergy _ f t) = DEnergy rec f t

instance RecNum (Power a) where
   getRecNum (Power r _ _) = r
   setRecNum rec (Power _ f t) = Power rec f t

instance RecNum (DPower a) where
   getRecNum (DPower r _ _) = r
   setRecNum rec (DPower _ f t) = DPower rec f t

instance RecNum (Eta a) where
   getRecNum (Eta r _ _) = r
   setRecNum rec (Eta _ f t) = Eta rec f t

instance RecNum (DEta a) where
   getRecNum (DEta r _ _) = r
   setRecNum rec (DEta _ f t) = DEta rec f t

instance RecNum (X a) where
   getRecNum (X r _ _) = r
   setRecNum rec (X _ f t) = X rec f t

instance RecNum (DX a) where
   getRecNum (DX r _ _) = r
   setRecNum rec (DX _ f t) = DX rec f t

instance RecNum (DTime a) where
   getRecNum (DTime r _) = r
   setRecNum rec (DTime _ s) = DTime rec s

instance RecNum (Storage a) where
   getRecNum (Storage r _) = r
   setRecNum rec (Storage _ n) = Storage rec n

instance RecNum (Var a) where
   getRecNum (Var r _ _) = r
   setRecNum rec (Var _ use t) = Var rec use t
