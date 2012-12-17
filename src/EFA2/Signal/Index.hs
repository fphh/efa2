module EFA2.Signal.Index where

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
data FEta = FEta !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DEta = DEta !Record !SecNode !SecNode deriving (Show, Ord, Eq)

-- | Splitting factors.
data X = X !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DX = DX !Record !SecNode !SecNode deriving (Show, Ord, Eq)

-- | Strange factors for outgoing intersection edges.
data Y = Y !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DY = DY !Record !SecNode !SecNode deriving (Show, Ord, Eq)



-- * Node indices

data Storage = Storage !Record !SecNode deriving (Show, Ord, Eq)

-- | This variable type can be used to express arbitrary relations.
-- You can variables also make dependent on section and record.
-- ATTENTION: Some of them are used for equation generation for
-- performance issues. You have to make sure yourself that your
-- variable is unique in the equational system.
--data Var = Var !Section !Record !Int !Int deriving (Show, Ord, Eq)


data Use = InSum
         | OutSum deriving (Show, Eq, Ord)

data DiffUse = InDiffSum
             | OutDiffSum deriving (Show, Eq, Ord)

toDiffUse :: Use -> DiffUse
toDiffUse InSum = InDiffSum
toDiffUse OutSum = OutDiffSum


data Var = Var !Record Use !SecNode deriving (Show, Ord, Eq)


data InSumVar = InSumVar !Record !SecNode deriving (Show, Ord, Eq)
data OutSumVar = OutSumVar !Record !SecNode deriving (Show, Ord, Eq)

-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime = DTime !Record !Section deriving (Show, Ord, Eq)
