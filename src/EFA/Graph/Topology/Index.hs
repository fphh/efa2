module EFA.Graph.Topology.Index where

import qualified EFA.Utility.TypeConstructor as TC

import qualified Test.QuickCheck as QC
import Control.Monad (liftM2)

import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)
import Data.Word (Word)

import qualified Prelude as P
import Prelude hiding (init, flip)


newtype State = State Word deriving (Show, Eq, Ord)

instance Enum State where
   toEnum n =
      if n >=0
        then State $ fromIntegral n
        else error "State.toEnum: negative number"
   fromEnum (State n) =
      if n <= fromIntegral (maxBound::Int)
        then fromIntegral n
        else error "State.fromEnum: number too big"

type InitOrState = Init State
type StateOrExit = Exit State


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
type Augmented sec = Exit (Init sec)
type AugmentedSection = Augmented Section
type AugmentedState   = Augmented State


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


switchAugmented ::
   a -> a -> (sec -> a) ->
   Augmented sec -> a
switchAugmented init exit secf aug =
   case aug of
      Exit -> exit
      NoExit Init -> init
      NoExit (NoInit s) -> secf s

fromAugmentedSection ::
   (MaybeSection sec, MaybeInit sec, MaybeExit sec) =>
   AugmentedSection -> sec
fromAugmentedSection =
   switchAugmented initSection exitSection fromSection


class ToAugmentedSection sec where
   augmentSection :: sec -> AugmentedSection

instance ToAugmentedSection Section where
   augmentSection = NoExit . NoInit

instance ToSection sec => ToAugmentedSection (Init sec) where
   augmentSection = NoExit . fmap toSection

instance ToInitOrSection sec => ToAugmentedSection (Exit sec) where
   augmentSection = fmap initOrSection


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


allowInit :: Exit sec -> Augmented sec
allowInit = fmap NoInit

allowExit :: Init sec -> Augmented sec
allowExit = NoExit

augment :: sec -> Augmented sec
augment = NoExit . NoInit


maybeInit :: Augmented sec -> Maybe (Exit sec)
maybeInit =
   switchAugmented Nothing (Just Exit) (Just . NoExit)

maybeExit :: Augmented sec -> Maybe (Init sec)
maybeExit aug =
   case aug of
      Exit -> Nothing
      NoExit sec -> Just sec


boundaryFromAugSection :: AugmentedSection -> Maybe Boundary
boundaryFromAugSection =
   fmap Following . maybeExit

augSectionFromBoundary :: Boundary -> AugmentedSection
augSectionFromBoundary (Following bnd) = allowExit bnd

sectionFromBoundary :: Boundary -> Maybe Section
sectionFromBoundary (Following bnd) =
   case bnd of
      Init -> Nothing
      NoInit sec -> Just sec


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


data PartNode part node = PartNode part node deriving (Show, Eq, Ord)

type StateNode = PartNode State
type SecNode = PartNode Section
type AugNode sec = PartNode (Augmented sec)
type BndNode = PartNode Boundary

type AugSecNode = AugNode Section
type AugStateNode = AugNode State


secNode :: Section -> node -> SecNode node
secNode = PartNode

initSecNode :: node -> AugSecNode node
initSecNode = PartNode initSection

exitSecNode :: node -> AugSecNode node
exitSecNode = PartNode exitSection

initAugNode :: node -> AugNode sec node
initAugNode = PartNode (NoExit Init)

exitAugNode :: node -> AugNode sec node
exitAugNode = PartNode Exit

afterSecNode :: Section -> node -> BndNode node
afterSecNode s = PartNode $ afterSection s

bndNodeFromSecNode :: SecNode node -> BndNode node
bndNodeFromSecNode (PartNode sec node) =
   PartNode (Following (NoInit sec)) node

secNodeFromBndNode :: BndNode node -> Maybe (SecNode node)
secNodeFromBndNode (PartNode bnd node) =
   fmap (P.flip PartNode node) $ sectionFromBoundary bnd

augNodeFromBndNode :: BndNode node -> AugSecNode node
augNodeFromBndNode (PartNode bnd node) =
   PartNode (augSectionFromBoundary bnd) node

bndNodeFromAugNode :: AugSecNode node -> Maybe (BndNode node)
bndNodeFromAugNode (PartNode aug node) =
   fmap (P.flip PartNode node) $ boundaryFromAugSection aug


maybeInitNode :: AugNode sec node -> Maybe (PartNode (Exit sec) node)
maybeInitNode (PartNode aug node) =
   fmap (P.flip PartNode node) $ maybeInit aug

maybeExitNode :: AugNode sec node -> Maybe (PartNode (Init sec) node)
maybeExitNode (PartNode aug node) =
   fmap (P.flip PartNode node) $ maybeExit aug



stateNode :: State -> node -> StateNode node
stateNode = PartNode



-- * Edge indices

data TopologyEdge node = TopologyEdge node node
   deriving (Show, Read, Eq, Ord)

{- |
A storage edge is always directed from an early to a later section.
However, a splitting factor exists both in chronological and reversed order.
On the other hand in the future we may use chronological order exclusively
and register two split factors per edge.
-}
data CarryEdge sec node = CarryEdge (Init sec) (Exit sec)
   deriving (Show, Eq, Ord)

data CarryBond sec node = CarryBond (Augmented sec) (Augmented sec)
   deriving (Show, Eq, Ord)

instance TC.Eq TopologyEdge where eq = (==)
instance (Eq sec) => TC.Eq (CarryEdge sec) where eq = (==)
instance (Eq sec) => TC.Eq (CarryBond sec) where eq = (==)

instance TC.Ord TopologyEdge where cmp = compare
instance (Ord sec) => TC.Ord (CarryEdge sec) where cmp = compare
instance (Ord sec) => TC.Ord (CarryBond sec) where cmp = compare

instance TC.Show TopologyEdge where showsPrec = showsPrec
instance (Show sec) => TC.Show (CarryEdge sec) where showsPrec = showsPrec
instance (Show sec) => TC.Show (CarryBond sec) where showsPrec = showsPrec


carryBondFromEdge :: CarryEdge sec node -> CarryBond sec node
carryBondFromEdge (CarryEdge s0 s1) =
   CarryBond (allowExit s0) (allowInit s1)

withCarryEdgeFromBond ::
   Ord part =>
   (CarryEdge part node -> a) ->
   (CarryEdge part node -> a) ->
   CarryBond part node -> a
withCarryEdgeFromBond fIn fOut (CarryBond stFrom stTo) =
   case (stFrom, stTo) of
      (NoExit from, Exit) ->
         fOut $ CarryEdge from Exit
      (NoExit Init, NoExit (NoInit to)) ->
         fOut $ CarryEdge Init (NoExit to)

      (Exit, NoExit from) ->
         fIn $ CarryEdge from Exit
      (NoExit (NoInit to), NoExit Init) ->
         fIn $ CarryEdge Init (NoExit to)

      (NoExit (NoInit x), NoExit (NoInit y)) ->
         case compare x y of
            LT -> fOut $ CarryEdge (NoInit x) (NoExit y)
            GT -> fIn  $ CarryEdge (NoInit y) (NoExit x)
            EQ -> error "storage loop in section"

      (NoExit Init, NoExit Init) ->
         error "storage loop at Init"
      (Exit, Exit) ->
         error "storage loop at Exit"


data InPart part idx node = InPart part (idx node)
   deriving (Show, Eq, Ord)

type InSection = InPart Section
type InState = InPart State


inPart ::
   (node -> idx node) -> PartNode part node -> InPart part idx node
inPart makeIdx (PartNode sec edge) = InPart sec (makeIdx edge)

liftInPart ::
   (idx0 node -> idx1 node) ->
   InPart part idx0 node -> InPart part idx1 node
liftInPart f (InPart sec edge) = InPart sec $ f edge


inState ::
   (node -> idx node) -> StateNode node -> InState idx node
inState = inPart

liftInState ::
   (idx0 node -> idx1 node) ->
   InState idx0 node -> InState idx1 node
liftInState = liftInPart


inSection ::
   (node -> idx node) -> SecNode node -> InSection idx node
inSection = inPart

liftInSection ::
   (idx0 node -> idx1 node) ->
   InSection idx0 node -> InSection idx1 node
liftInSection = liftInPart

data ForStorage idx node = ForStorage (idx node) node
   deriving (Show, Eq, Ord)

forStorage ::
   (part -> idx node) -> PartNode part node -> ForStorage idx node
forStorage makeIdx (PartNode bnd node) =
   ForStorage (makeIdx bnd) node

liftForStorage ::
   (idx0 node -> idx1 node) ->
   ForStorage idx0 node -> ForStorage idx1 node
liftForStorage f (ForStorage edge node) =
   ForStorage (f edge) node


wrapInPart :: InPart part idx node -> InPart part (TC.Wrap idx) node
wrapInPart = liftInPart TC.Wrap

wrapForStorage :: ForStorage idx node -> ForStorage (TC.Wrap idx) node
wrapForStorage = liftForStorage TC.Wrap

instance (Eq part, TC.Eq idx) => TC.Eq (InPart part idx) where
   eq = equating wrapInPart
instance TC.Eq idx => TC.Eq (ForStorage idx) where
   eq = equating wrapForStorage

instance (Ord part, TC.Ord idx) => TC.Ord (InPart part idx) where
   cmp = comparing wrapInPart
instance TC.Ord idx => TC.Ord (ForStorage idx) where
   cmp = comparing wrapForStorage

instance (Show part, TC.Show idx) => TC.Show (InPart part idx) where
   showsPrec p = showsPrec p . wrapInPart
instance TC.Show idx => TC.Show (ForStorage idx) where
   showsPrec p = showsPrec p . wrapForStorage


carryEdge ::
   (CarryEdge sec node -> idx node) ->
   Init sec -> Exit sec -> node -> ForStorage idx node
carryEdge mkIdx s0 s1 n =
   ForStorage (mkIdx $ CarryEdge s0 s1) n

carryBond ::
   (CarryBond sec node -> idx node) ->
   Augmented sec -> Augmented sec -> node -> ForStorage idx node
carryBond mkIdx s0 s1 n =
   ForStorage (mkIdx $ CarryBond s0 s1) n


carryEdgeFrom, carryEdgeTo ::
   ForStorage (CarryEdge sec) node -> AugNode sec node
carryEdgeFrom (ForStorage (CarryEdge sec _) n) = PartNode (allowExit sec) n
carryEdgeTo   (ForStorage (CarryEdge _ sec) n) = PartNode (allowInit sec) n

carryBondFrom, carryBondTo ::
   ForStorage (CarryBond sec) node -> AugNode sec node
carryBondFrom (ForStorage (CarryBond sec _) n) = PartNode sec n
carryBondTo   (ForStorage (CarryBond _ sec) n) = PartNode sec n



class Flip edge where
   flip :: edge node -> edge node


instance Flip idx => Flip (InPart part idx) where
   flip (InPart s idx) = InPart s (flip idx)

instance Flip TopologyEdge where
   flip (TopologyEdge x y) = TopologyEdge y x


instance Flip idx => Flip (ForStorage idx) where
   flip (ForStorage idx n) = ForStorage (flip idx) n

instance Flip (CarryBond sec) where
   flip (CarryBond s0 s1) = CarryBond s1 s0


instance Flip Power where
   flip (Power x) = Power $ flip x

instance Flip Energy where
   flip (Energy x) = Energy $ flip x

instance Flip (StX sec) where
   flip (StX x) = StX $ flip x

instance Flip PPos where
   flip (PPos x) = PPos $ flip x


instance (QC.Arbitrary node) => QC.Arbitrary (TopologyEdge node) where
   arbitrary = liftM2 TopologyEdge QC.arbitrary QC.arbitrary
   shrink (TopologyEdge from to) =
      map (uncurry TopologyEdge) $ QC.shrink (from, to)

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
newtype Energy node = Energy (TopologyEdge node) deriving (Show, Ord, Eq)

newtype StEnergy sec node = StEnergy (CarryEdge sec node) deriving (Show, Ord, Eq)


-- | Energy variables for hypothetical outgoing energies.
-- At storage edges they describe the maximum energy
-- that a storage could deliver.
newtype MaxEnergy node = MaxEnergy (CarryEdge Section node) deriving (Show, Ord, Eq)

-- | Power variables.
newtype Power node = Power (TopologyEdge node) deriving (Show, Ord, Eq)

-- | Eta variables.
newtype Eta node = Eta (TopologyEdge node) deriving (Show, Ord, Eq)

-- | Splitting factors.
newtype X node = X (TopologyEdge node) deriving (Show, Ord, Eq)

newtype StX sec node = StX (CarryBond sec node) deriving (Show, Ord, Eq)

newtype Storage node = Storage Boundary deriving (Show, Ord, Eq)

data Direction = In | Out deriving (Show, Eq, Ord)

data Sum node = Sum Direction node deriving (Show, Ord, Eq)

data StInSum sec node = StInSum (Exit sec) deriving (Show, Ord, Eq)

data StOutSum sec node = StOutSum (Init sec) deriving (Show, Ord, Eq)


-- * Other indices

-- | Delta time variables, depending solely on their section and record number.
data DTime node = DTime deriving (Show, Ord, Eq)

-- | Indices for Power Position
newtype PPos node = PPos (TopologyEdge node) deriving (Show, Read, Ord, Eq)


