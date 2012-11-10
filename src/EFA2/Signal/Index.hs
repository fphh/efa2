module EFA2.Signal.Index where


newtype Section = Section Int deriving (Show, Eq, Ord)

instance Enum Section where
   fromEnum (Section n) = n
   toEnum n = Section n


newtype Record = Record Int deriving (Show, Eq, Ord)

newtype Store = Store Int deriving (Show, Eq, Ord)


-- * Edge indices

-- | Variable types of the solver. The solver, in fact, is
-- ignorant of the provenance of the variables. However, to
-- facilitate life, we introduce variable types, that make
-- it easy to express things needed in energy flow analysis,
-- that is:
--
-- * a section number
-- * a data record number
-- * two numbers to identify a place in the topology
--   (for equation generation, we use the underlying fgl node ids.

-- | Energy variables.
data Energy = Energy !Section !Record !Int !Int deriving (Show, Ord, Eq)
data DEnergy = DEnergy !Section !Record !Int !Int deriving (Show, Ord, Eq)

-- | Power variables.
data Power = Power !Section !Record !Int !Int deriving (Show, Ord, Eq)
data DPower = DPower !Section !Record !Int !Int deriving (Show, Ord, Eq)

-- | Eta variables.
data FEta = FEta !Section !Record !Int !Int deriving (Show, Ord, Eq)
data DEta = DEta !Section !Record !Int !Int deriving (Show, Ord, Eq)

-- | Splitting factors.
data X = X !Section !Record !Int !Int deriving (Show, Ord, Eq)
data DX = DX !Section !Record !Int !Int deriving (Show, Ord, Eq)


-- * Node indices

-- | Section number, record number, storage number.
data Storage = Storage !Section !Record !Store deriving (Show, Ord, Eq)

-- | This variable type can be used to express arbitrary relations.
-- You can variables also make dependent on section and record.
-- ATTENTION: Some of them are used for equation generation for
-- performance issues. You have to make sure yourself that your
-- variable is unique in the equational system.
--data Var = Var !Section !Record !Int !Int deriving (Show, Ord, Eq)

data Use = InSum
         | OutSum
         | InDiffSum
         | OutDiffSum
         | St deriving (Show, Eq, Ord)

toDiffUse :: Use -> Use
toDiffUse InSum = InDiffSum
toDiffUse OutSum = OutDiffSum

data Var = Var !Section !Record Use !Int deriving (Show, Ord, Eq)


-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime = DTime !Section !Record deriving (Show, Ord, Eq)
