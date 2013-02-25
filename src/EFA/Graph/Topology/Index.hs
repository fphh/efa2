module EFA.Graph.Topology.Index where


newtype Section = Section Int deriving (Show, Eq, Ord)

instance Enum Section where
   fromEnum (Section n) = n
   toEnum n = Section n

initSection :: Section
initSection = Section (-1)



data Absolute = Absolute deriving (Show, Eq, Ord)

data Delta = Delta | Before | After deriving (Show, Eq, Ord)


data Record rec idx = Record rec idx deriving (Show, Eq, Ord)

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
data Energy a = Energy !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)


-- | Energy variables for hypothetical outgoing energies.
-- At intersection edges they describe the maximum energy
-- that a storage could deliver.
data MaxEnergy a = MaxEnergy !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Power variables.
data Power a = Power !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Eta variables.
data Eta a = Eta !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Splitting factors.
data X a = X !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

-- | Strange factors for outgoing intersection edges.
data Y a = Y !(SecNode a) !(SecNode a) deriving (Show, Ord, Eq)

data Storage a = Storage !(SecNode a) deriving (Show, Ord, Eq)

data Direction = In | Out deriving (Show, Eq, Ord)

data Sum a = Sum !Direction !(SecNode a) deriving (Show, Ord, Eq)


-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime a = DTime !Section deriving (Show, Ord, Eq)
