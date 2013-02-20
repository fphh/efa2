module EFA.Graph.Topology.Index where


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

recAbs :: Record
recAbs = Record Absolute


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

data Direction = In | Out deriving (Show, Eq, Ord)

data Sum a = Sum !Record !Direction !(SecNode a) deriving (Show, Ord, Eq)


-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime a = DTime !Record !Section deriving (Show, Ord, Eq)
