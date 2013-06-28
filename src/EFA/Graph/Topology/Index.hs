module EFA.Graph.Topology.Index where

import qualified EFA.Utility.TypeConstructor as TC

import qualified Test.QuickCheck as QC
import Control.Monad (liftM2)

import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)
import Data.Word (Word)

import qualified Prelude as P
import Prelude hiding (flip)


newtype Section = Section Word deriving (Show, Eq, Ord)

instance Enum Section where
   toEnum n =
      if n >=0
        then Section $ fromIntegral n
        else error "Section.toEnum: negative number"
   fromEnum (Section n) =
      if n <= fromIntegral (maxBound::Int)
        then fromIntegral n
        else error "Section.fromEnum: number too big"

data Init a = Init | NoInit a deriving (Show, Eq, Ord)
data Exit a = NoExit a | Exit deriving (Show, Eq, Ord)

type InitOrSection = Init Section
type SectionOrExit = Exit Section
type AugmentedSection = Init SectionOrExit


instance Functor Init where
   fmap _ Init = Init
   fmap f (NoInit a) = NoInit $ f a

instance Functor Exit where
   fmap _ Exit = Exit
   fmap f (NoExit a) = NoExit $ f a


section :: MaybeSection sec => Word -> sec
section = fromSection . Section

class MaybeSection sec where
   fromSection :: Section -> sec

instance MaybeSection Section where
   fromSection = id

instance MaybeSection sec => MaybeSection (Init sec) where
   fromSection = NoInit . fromSection

instance MaybeSection sec => MaybeSection (Exit sec) where
   fromSection = NoExit . fromSection


class MaybeSection sec => MaybeInit sec where
   initSection :: sec

instance MaybeSection sec => MaybeInit (Init sec) where
   initSection = Init

instance MaybeInit sec => MaybeInit (Exit sec) where
   initSection = NoExit initSection


class MaybeSection sec => MaybeExit sec where
   exitSection :: sec

instance MaybeSection sec => MaybeExit (Exit sec) where
   exitSection = Exit

instance MaybeExit sec => MaybeExit (Init sec) where
   exitSection = NoInit exitSection


fromAugmentedSection ::
   (MaybeSection sec, MaybeInit sec, MaybeExit sec) =>
   AugmentedSection -> sec
fromAugmentedSection a =
   case a of
      Init -> initSection
      NoInit Exit -> exitSection
      NoInit (NoExit s) -> fromSection s


class ToAugmentedSection sec where
   augmentSection :: sec -> AugmentedSection

instance ToAugmentedSection Section where
   augmentSection = NoInit . NoExit

instance ToSectionOrExit sec => ToAugmentedSection (Init sec) where
   augmentSection = fmap sectionOrExit

instance ToSection sec => ToAugmentedSection (Exit sec) where
   augmentSection = NoInit . fmap toSection


class ToInitOrSection sec where
   initOrSection :: sec -> InitOrSection

instance ToInitOrSection Section where
   initOrSection = NoInit

instance ToSection sec => ToInitOrSection (Init sec) where
   initOrSection = fmap toSection


class ToSectionOrExit sec where
   sectionOrExit :: sec -> SectionOrExit

instance ToSectionOrExit Section where
   sectionOrExit = NoExit

instance ToSection sec => ToSectionOrExit (Exit sec) where
   sectionOrExit = fmap toSection


class ToSection sec where
   toSection :: sec -> Section

instance ToSection Section where
   toSection = id


allowInit :: SectionOrExit -> AugmentedSection
allowInit = NoInit

allowExit :: InitOrSection -> AugmentedSection
allowExit = fmap NoExit



boundaryFromAugSection :: AugmentedSection -> Maybe Boundary
boundaryFromAugSection x =
   fmap Following $
   case x of
      Init -> Just Init
      NoInit Exit -> Nothing
      NoInit (NoExit s) -> Just $ NoInit s

augSectionFromBoundary :: Boundary -> AugmentedSection
augSectionFromBoundary (Following bnd) = allowExit bnd


newtype Boundary = Following (Init Section) deriving (Show, Eq, Ord)

instance Enum Boundary where
   toEnum n =
      if n == -1
        then Following Init
        else Following $ NoInit $ toEnum n
   fromEnum (Following Init) = -1
   fromEnum (Following (NoInit n)) = fromEnum n

initial :: Boundary
initial = Following Init

afterSection :: Section -> Boundary
afterSection = Following . NoInit

beforeSection :: Section -> Boundary
beforeSection s =
   if s == Section 0
     then Following Init
     else Following (NoInit (pred s))


data Absolute = Absolute deriving (Show, Eq, Ord)

data Delta = Before | Delta | After deriving (Show, Eq, Ord)

data ExtDelta rec = ExtDelta Delta rec deriving (Show, Eq, Ord)


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


data TimeNode time node = TimeNode time node deriving (Show, Eq, Ord)

type SecNode = TimeNode Section
type AugNode = TimeNode AugmentedSection
type BndNode = TimeNode Boundary


initSecNode :: node -> AugNode node
initSecNode = TimeNode Init

afterSecNode :: Section -> node -> BndNode node
afterSecNode s = TimeNode (Following (NoInit s))

bndNodeFromSecNode :: SecNode node -> BndNode node
bndNodeFromSecNode (TimeNode sec node) =
   TimeNode (Following (NoInit sec)) node

secNodeFromBndNode :: BndNode node -> Maybe (SecNode node)
secNodeFromBndNode (TimeNode bnd node) =
   case bnd of
      Following Init -> Nothing
      Following (NoInit sec) -> Just (TimeNode sec node)

augNodeFromBndNode :: BndNode node -> AugNode node
augNodeFromBndNode (TimeNode bnd node) =
   TimeNode (augSectionFromBoundary bnd) node

bndNodeFromAugNode :: AugNode node -> Maybe (BndNode node)
bndNodeFromAugNode (TimeNode aug node) =
   fmap (P.flip TimeNode node) $ boundaryFromAugSection aug

secNode :: Section -> node -> SecNode node
secNode = TimeNode



-- * Edge indices

data StructureEdge node = StructureEdge node node
   deriving (Show, Read, Eq, Ord)

{- |
A storage edge is always directed from an early to a later section.
However, a splitting factor exists both in chronological and reversed order.
On the other hand in the future we may use chronological order exclusively
and register two split factors per edge.
-}
data StorageEdge node = StorageEdge InitOrSection SectionOrExit
   deriving (Show, Eq, Ord)

data StorageTrans node = StorageTrans AugmentedSection AugmentedSection
   deriving (Show, Eq, Ord)

instance TC.Eq StructureEdge where eq = (==)
instance TC.Eq StorageEdge   where eq = (==)
instance TC.Eq StorageTrans  where eq = (==)

instance TC.Ord StructureEdge where cmp = compare
instance TC.Ord StorageEdge   where cmp = compare
instance TC.Ord StorageTrans  where cmp = compare

instance TC.Show StructureEdge where showsPrec = showsPrec
instance TC.Show StorageEdge   where showsPrec = showsPrec
instance TC.Show StorageTrans  where showsPrec = showsPrec


storageTransFromEdge :: StorageEdge node -> StorageTrans node
storageTransFromEdge (StorageEdge s0 s1) =
   StorageTrans (allowExit s0) (allowInit s1)


data InSection idx node = InSection Section (idx node)
   deriving (Show, Eq, Ord)

inSection ::
   (node -> idx node) -> SecNode node -> InSection idx node
inSection makeIdx (TimeNode sec edge) =
   InSection sec (makeIdx edge)

liftInSection ::
   (idx0 node -> idx1 node) ->
   InSection idx0 node -> InSection idx1 node
liftInSection f (InSection sec edge) =
   InSection sec $ f edge

data ForNode idx node = ForNode (idx node) node
   deriving (Show, Eq, Ord)

forNode ::
   (time -> idx node) -> TimeNode time node -> ForNode idx node
forNode makeIdx (TimeNode bnd node) =
   ForNode (makeIdx bnd) node

liftForNode ::
   (idx0 node -> idx1 node) ->
   ForNode idx0 node -> ForNode idx1 node
liftForNode f (ForNode edge node) =
   ForNode (f edge) node


wrapInSection :: InSection idx node -> InSection (TC.Wrap idx) node
wrapInSection (InSection s e)  =  InSection s (TC.Wrap e)

wrapForNode :: ForNode idx node -> ForNode (TC.Wrap idx) node
wrapForNode (ForNode e n)  =  ForNode (TC.Wrap e) n

instance TC.Eq idx => TC.Eq (InSection idx) where eq = equating wrapInSection
instance TC.Eq idx => TC.Eq (ForNode   idx) where eq = equating wrapForNode

instance TC.Ord idx => TC.Ord (InSection idx) where cmp = comparing wrapInSection
instance TC.Ord idx => TC.Ord (ForNode   idx) where cmp = comparing wrapForNode

instance TC.Show idx => TC.Show (InSection idx) where showsPrec p = showsPrec p . wrapInSection
instance TC.Show idx => TC.Show (ForNode   idx) where showsPrec p = showsPrec p . wrapForNode


structureEdge ::
   (StructureEdge node -> idx node) ->
   Section -> node -> node -> InSection idx node
structureEdge mkIdx s x y =
   InSection s $ mkIdx $ StructureEdge x y

storageEdge ::
   (StorageEdge node -> idx node) ->
   InitOrSection -> SectionOrExit -> node -> ForNode idx node
storageEdge mkIdx s0 s1 n =
   ForNode (mkIdx $ StorageEdge s0 s1) n

storageTrans ::
   (StorageTrans node -> idx node) ->
   AugmentedSection -> AugmentedSection -> node -> ForNode idx node
storageTrans mkIdx s0 s1 n =
   ForNode (mkIdx $ StorageTrans s0 s1) n


storageEdgeFrom, storageEdgeTo ::
   ForNode StorageEdge node -> AugNode node
storageEdgeFrom (ForNode (StorageEdge sec _) n) = TimeNode (allowExit sec) n
storageEdgeTo   (ForNode (StorageEdge _ sec) n) = TimeNode (allowInit sec) n



class Flip edge where
   flip :: edge node -> edge node


instance Flip idx => Flip (InSection idx) where
   flip (InSection s idx) = InSection s (flip idx)

instance Flip StructureEdge where
   flip (StructureEdge x y) = StructureEdge y x


instance Flip idx => Flip (ForNode idx) where
   flip (ForNode idx n) = ForNode (flip idx) n

instance Flip StorageTrans where
   flip (StorageTrans s0 s1) = StorageTrans s1 s0


instance Flip Power where
   flip (Power x) = Power $ flip x

instance Flip PPos where
   flip (PPos x) = PPos $ flip x


instance (QC.Arbitrary node) => QC.Arbitrary (StructureEdge node) where
   arbitrary = liftM2 StructureEdge QC.arbitrary QC.arbitrary
   shrink (StructureEdge from to) =
      map (uncurry StructureEdge) $ QC.shrink (from, to)

instance (QC.Arbitrary node) => QC.Arbitrary (PPos node) where
   arbitrary = fmap PPos QC.arbitrary
   shrink (PPos x) = map PPos $ QC.shrink x


-- | Variable types of the solver. The solver, in fact, is
-- ignorant of the provenance of the variables. However, to
-- facilitate life, we introduce variable types, that make
-- it easy to express things needed in energy flow analysis,
-- that is:
--
-- * a data record number
-- * two node identifiers to specify a place in the topology

-- | Energy variables.
newtype Energy node = Energy (StructureEdge node) deriving (Show, Ord, Eq)

newtype StEnergy node = StEnergy (StorageTrans node) deriving (Show, Ord, Eq)


-- | Energy variables for hypothetical outgoing energies.
-- At storage edges they describe the maximum energy
-- that a storage could deliver.
newtype MaxEnergy node = MaxEnergy (StorageEdge node) deriving (Show, Ord, Eq)

-- | Power variables.
newtype Power node = Power (StructureEdge node) deriving (Show, Ord, Eq)

-- | Eta variables.
newtype Eta node = Eta (StructureEdge node) deriving (Show, Ord, Eq)

-- | Splitting factors.
newtype X node = X (StructureEdge node) deriving (Show, Ord, Eq)

newtype StX node = StX (StorageTrans node) deriving (Show, Ord, Eq)

newtype Storage node = Storage Boundary deriving (Show, Ord, Eq)

data Direction = In | Out deriving (Show, Eq, Ord)

data Sum node = Sum Direction node deriving (Show, Ord, Eq)

{-
These types would be more precise,
but since we cannot assert the restricted range of sections
statically in many places,
this only leads to a lot of incomplete case analyses.

data StInSum node = StInSum InitOrSection deriving (Show, Ord, Eq)

data StOutSum node = StOutSum SectionOrExit deriving (Show, Ord, Eq)
-}

data StInSum node = StInSum AugmentedSection deriving (Show, Ord, Eq)

data StOutSum node = StOutSum AugmentedSection deriving (Show, Ord, Eq)


-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime node = DTime deriving (Show, Ord, Eq)

-- | Indices for Power Position
newtype PPos node = PPos (StructureEdge node) deriving (Show, Read, Ord, Eq)


