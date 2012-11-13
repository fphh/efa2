module EFA2.Signal.Index where

import qualified Test.QuickCheck as QC


newtype Section = Section Int deriving (Show, Eq, Ord)

instance Enum Section where
   fromEnum (Section n) = n
   toEnum n = Section n

initSection :: Section
initSection = Section (-1)


newtype Record = Record Int deriving (Show, Eq, Ord)

newtype Store = Store Int deriving (Show, Eq, Ord)


newtype Node = Node Int deriving (Show, Eq, Ord)

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

-- | Power variables.
data Power = Power !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DPower = DPower !Record !SecNode !SecNode deriving (Show, Ord, Eq)

-- | Eta variables.
data FEta = FEta !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DEta = DEta !Record !SecNode !SecNode deriving (Show, Ord, Eq)

-- | Splitting factors.
data X = X !Record !SecNode !SecNode deriving (Show, Ord, Eq)
data DX = DX !Record !SecNode !SecNode deriving (Show, Ord, Eq)


-- * Node indices

data Storage = Storage !Record !Section !Store deriving (Show, Ord, Eq)

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

data Var = Var !Record Use !SecNode deriving (Show, Ord, Eq)


-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime = DTime !Record !Section deriving (Show, Ord, Eq)
