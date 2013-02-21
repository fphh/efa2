module EFA.Graph.Topology.Index where


newtype Section = Section Int deriving (Show, Eq, Ord)

instance Enum Section where
   fromEnum (Section n) = n
   toEnum n = Section n

initSection :: Section
initSection = Section (-1)



data Absolute = Absolute deriving (Show, Eq, Ord)

data Delta = Delta | Before | After deriving (Show, Eq, Ord)


class Ord rec => Record rec where recDeflt :: rec
instance Record Absolute where recDeflt = Absolute
instance Record Delta where recDeflt = Before



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
data Energy rec a = Energy rec !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)


-- | Energy variables for hypothetical outgoing energies.
-- At intersection edges they describe the maximum energy
-- that a storage could deliver.
data MaxEnergy rec a = MaxEnergy rec !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Power variables.
data Power rec a = Power rec !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Eta variables.
data Eta rec a = Eta rec !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Splitting factors.
data X rec a = X rec !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Strange factors for outgoing intersection edges.
data Y rec a = Y rec !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

data Storage rec a = Storage rec !(SecNode a) deriving (Show, Ord, Eq)

data Direction = In | Out deriving (Show, Eq, Ord)

data Sum rec a = Sum rec !Direction !(SecNode a) deriving (Show, Ord, Eq)


-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime rec a = DTime rec !Section deriving (Show, Ord, Eq)
