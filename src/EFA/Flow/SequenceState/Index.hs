module EFA.Flow.SequenceState.Index (
   module EFA.Flow.SequenceState.Index,
   module EFA.Flow.Part.Index,
   PartIdx.Boundary(PartIdx.Following),
   PartIdx.AugmentedSection,
   PartIdx.AugmentedState,
   PartIdx.InitOrSection, PartIdx.SectionOrExit,
   PartIdx.Section(PartIdx.Section),
   PartIdx.State(PartIdx.State),
   PartIdx.section0, PartIdx.state0,
   PartIdx.initial, PartIdx.afterSection,
   PartIdx.augment,
   PartIdx.switchAugmented,
   PartIdx.boundaryFromAugSection,
   PartIdx.sectionFromBoundary,
   PartIdx.initSection,
   PartIdx.exitSection,
   ) where

import qualified EFA.Flow.Topology.Index as TopoIdx
import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.Part.Index as PartIdx
import EFA.Flow.Part.Index
          (State, Section, Boundary, Augmented,
           Init(Init, NoInit), maybeInit, allowInit,
           Exit(Exit, NoExit), maybeExit, allowExit)

import EFA.Report.Format (Format)

import qualified EFA.Utility.TypeConstructor as TC

import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)

import qualified Prelude as P
import Prelude hiding (init, flip)


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
initSecNode = PartNode PartIdx.initSection

exitSecNode :: node -> AugSecNode node
exitSecNode = PartNode PartIdx.exitSection

initAugNode :: node -> AugNode sec node
initAugNode = PartNode (NoExit Init)

exitAugNode :: node -> AugNode sec node
exitAugNode = PartNode Exit

afterSecNode :: Section -> node -> BndNode node
afterSecNode s = PartNode $ PartIdx.afterSection s

bndNodeFromSecNode :: SecNode node -> BndNode node
bndNodeFromSecNode (PartNode sec node) =
   PartNode (PartIdx.Following (NoInit sec)) node

secNodeFromBndNode :: BndNode node -> Maybe (SecNode node)
secNodeFromBndNode (PartNode bnd node) =
   fmap (P.flip PartNode node) $ PartIdx.sectionFromBoundary bnd

augNodeFromBndNode :: BndNode node -> AugSecNode node
augNodeFromBndNode (PartNode bnd node) =
   PartNode (PartIdx.augSectionFromBoundary bnd) node

bndNodeFromAugNode :: AugSecNode node -> Maybe (BndNode node)
bndNodeFromAugNode (PartNode aug node) =
   fmap (P.flip PartNode node) $ PartIdx.boundaryFromAugSection aug


maybeInitNode :: AugNode sec node -> Maybe (PartNode (Exit sec) node)
maybeInitNode (PartNode aug node) =
   fmap (P.flip PartNode node) $ maybeInit aug

maybeExitNode :: AugNode sec node -> Maybe (PartNode (Init sec) node)
maybeExitNode (PartNode aug node) =
   fmap (P.flip PartNode node) $ maybeExit aug



stateNode :: State -> node -> StateNode node
stateNode = PartNode



-- * Edge indices

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

data ForStorage idx node = ForStorage idx node
   deriving (Show, Eq, Ord)

forStorage ::
   (part -> idx) -> PartNode part node -> ForStorage idx node
forStorage makeIdx (PartNode bnd node) =
   ForStorage (makeIdx bnd) node

liftForStorage ::
   (idx0 -> idx1) ->
   ForStorage idx0 node -> ForStorage idx1 node
liftForStorage f (ForStorage edge node) =
   ForStorage (f edge) node


wrapInPart :: InPart part idx node -> InPart part (TC.Wrap idx) node
wrapInPart = liftInPart TC.Wrap


instance (Eq part, TC.Eq idx) => TC.Eq (InPart part idx) where
   eq = equating wrapInPart
instance Eq idx => TC.Eq (ForStorage idx) where
   eq = (==)

instance (Ord part, TC.Ord idx) => TC.Ord (InPart part idx) where
   cmp = comparing wrapInPart
instance Ord idx => TC.Ord (ForStorage idx) where
   cmp = compare

instance (Show part, TC.Show idx) => TC.Show (InPart part idx) where
   showsPrec p = showsPrec p . wrapInPart
instance Show idx => TC.Show (ForStorage idx) where
   showsPrec p = P.showsPrec p


carryEdge ::
   (StorageIdx.Edge sec -> idx) ->
   Init sec -> Exit sec -> node -> ForStorage idx node
carryEdge mkIdx s0 s1 n =
   ForStorage (mkIdx $ StorageIdx.Edge s0 s1) n

carryPosition ::
   (StorageIdx.Position sec -> idx) ->
   Augmented sec -> Augmented sec -> node -> ForStorage idx node
carryPosition mkIdx s0 s1 n =
   ForStorage (mkIdx $ StorageIdx.Position s0 s1) n


carryEdgeFrom, carryEdgeTo ::
   ForStorage (StorageIdx.Edge sec) node -> AugNode sec node
carryEdgeFrom (ForStorage (StorageIdx.Edge sec _) n) = PartNode (allowExit sec) n
carryEdgeTo   (ForStorage (StorageIdx.Edge _ sec) n) = PartNode (allowInit sec) n

carryPositionFrom, carryPositionTo ::
   ForStorage (StorageIdx.Position sec) node -> AugNode sec node
carryPositionFrom (ForStorage (StorageIdx.Position sec _) n) = PartNode sec n
carryPositionTo   (ForStorage (StorageIdx.Position _ sec) n) = PartNode sec n


instance TopoIdx.Flip idx => TopoIdx.Flip (InPart part idx) where
   flip (InPart s idx) = InPart s (TopoIdx.flip idx)


class Identifier idx where
   identifier :: Format output => idx node -> output

instance TopoIdx.Identifier idx => Identifier (InPart part idx) where
   identifier (InPart _part idx) = TopoIdx.identifier idx

instance StorageIdx.Identifier idx => Identifier (ForStorage idx) where
   identifier (ForStorage idx _node) = StorageIdx.identifier idx
