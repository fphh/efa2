{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module EFA.Flow.Sequence.Quantity (
   Graph, SeqFlow.sequence, SeqFlow.storages,
   Topology, Sequence, Storages,
   Sums(..), Carry(..), Flow(..),

   mapGraph,
   mapStorages,
   mapSequence,

   checkedZipWithGraph,
   checkedZipWithSequence,
   checkedZipWithStorages,

   traverseGraph,
   traverseStorages,
   traverseSequence,

   foldMap,
   fold,

   toAssignMap,

   Unknown(..),
   sequenceGraph,
   graphFromSections,

   mapGraphWithVar,
   mapStoragesWithVar,
   mapSequenceWithVar,

   lookupSums,

   Lookup, lookup,
   LookupScalar,
   Env.TypeOf, Env.Element, Env.switchPart,

   formatAssigns,
   ) where

import qualified EFA.Flow.Sequence.AssignMap as AssignMap
import qualified EFA.Flow.Sequence.Index as SeqIdx
import qualified EFA.Flow.Sequence.Variable as Var
import qualified EFA.Flow.Sequence as SeqFlow
import qualified EFA.Flow.SequenceState.Quantity as Env
import qualified EFA.Flow.SequenceState.Index as Idx
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Storage.Quantity as StorageQuant
import qualified EFA.Flow.Storage.Variable as StorageVar
import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.Storage as Storage
import EFA.Flow.Topology.Quantity (Topology, Sums(..), Flow(..))
import EFA.Flow.SequenceState.Variable ((<#>))
import EFA.Flow.Sequence.AssignMap (AssignMap)
import EFA.Flow.Sequence (sequence, storages)

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph

import qualified EFA.Signal.Sequence as Sequ

import EFA.Equation.Unknown (Unknown(unknown))

import EFA.Report.FormatValue (FormatValue, formatAssign)
import EFA.Report.Format (Format)

import qualified EFA.Utility.Map as MapU
import EFA.Utility.Map (Caller)

import qualified Control.Monad.Trans.Writer as MW
import Control.Monad ((<=<))
import Control.Applicative (Applicative, pure, liftA2, (<*>))

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Foldable as Fold

import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid, (<>))
import Data.Tuple.HT (mapPair, mapSnd)

import Prelude hiding (lookup, init, seq, sequence, sin, sum)


type
   Storages node a = SeqFlow.Storages node a a (Carry a)

type
   Sequence node v =
      SeqFlow.Sequence node Graph.EitherEdge v (Sums v) (Maybe (Flow v))

type
   Graph node a v =
      SeqFlow.Graph node Graph.EitherEdge
         v (Sums v) a a (Maybe (Flow v)) (Carry a)

data Carry a =
   Carry {
      carryMaxEnergy, carryXOut, carryEnergy, carryXIn :: a
   }
   deriving (Eq)


instance Show (Graph a b c) where
         show _ = "<SeqQty.Graph>"

instance StorageQuant.Carry Carry where
   carryEnergy = carryEnergy
   carryXOut   = carryXOut
   carryXIn    = carryXIn
   foldEnergy f e = f (carryMaxEnergy e) <> f (carryEnergy e)

   type CarryPart Carry = Idx.Section
   carryVars =
      Carry {
         carryMaxEnergy = StorageVar.index . StorageIdx.MaxEnergy,
         carryEnergy = StorageVar.index . StorageIdx.Energy,
         carryXOut = StorageVar.index . StorageIdx.X . StorageIdx.outPosFromEdge,
         carryXIn = StorageVar.index . StorageIdx.X . StorageIdx.inPosFromEdge
      }


instance Functor Carry where
   fmap f (Carry me xout e xin) =
      Carry (f me) (f xout) (f e) (f xin)


instance Foldable Carry where
   foldMap = foldMapDefault


instance Traversable Carry where
   traverse f (Carry me xout e xin) =
      pure Carry <*> f me <*> f xout <*> f e <*> f xin


instance Applicative Carry where
   pure a = Carry a a a a
   Carry fme fxout fe fxin <*> Carry me xout e xin =
      Carry (fme me) (fxout xout) (fe e) (fxin xin)


mapGraph ::
   (a0 -> a1) ->
   (v0 -> v1) ->
   Graph node a0 v0 -> Graph node a1 v1
mapGraph f g gr =
   SeqFlow.Graph {
      sequence = mapSequence g $ sequence gr,
      storages = mapStorages f $ storages gr
   }

mapSequence ::
   (v0 -> v1) ->
   Sequence node v0 -> Sequence node v1
mapSequence f =
   fmap (mapSnd $ FlowTopo.mapSection f)

mapStorages ::
   (a0 -> a1) ->
   Storages node a0 -> Storages node a1
mapStorages f =
   fmap (mapPair (StorageQuant.mapGraph f, fmap f))


checkedZipWithGraph ::
   (Ord node) =>
   Caller ->
   (a0 -> a1 -> a2) ->
   (v0 -> v1 -> v2) ->
   Graph node a0 v0 ->
   Graph node a1 v1 ->
   Graph node a2 v2
checkedZipWithGraph caller f g gr0 gr1 =
   SeqFlow.Graph {
      sequence = checkedZipWithSequence caller g (sequence gr0) (sequence gr1),
      storages = checkedZipWithStorages caller f (storages gr0) (storages gr1)
   }

checkedZipWithSequence ::
   (Ord node) =>
   Caller ->
   (v0 -> v1 -> v2) ->
   Sequence node v0 ->
   Sequence node v1 ->
   Sequence node v2
checkedZipWithSequence caller f =
   MapU.checkedZipWith (caller++".checkedZipWithSequence")
      (\(rng0, gr0) (rng1, gr1) ->
         (if rng0==rng1 then rng0 else error (caller++".equalRange"),
          FlowTopo.checkedZipWithSection
             (caller++".checkedZipWithSequence.section")
             f gr0 gr1))

checkedZipWithStorages ::
   (Ord node) =>
   Caller ->
   (a0 -> a1 -> a2) ->
   Storages node a0 ->
   Storages node a1 ->
   Storages node a2
checkedZipWithStorages caller f =
   let name = caller++".checkedZipWithStorages"
   in  MapU.checkedZipWith name
          (\(graph0, storage0)
            (graph1, storage1) ->
               (Storage.checkedZipWith name f (liftA2 f) graph0 graph1,
                MapU.checkedZipWith (name++".storage") f storage0 storage1))


traverseGraph ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Graph node a0 v0 -> f (Graph node a1 v1)
traverseGraph f g (SeqFlow.Graph sts seq) =
   liftA2 SeqFlow.Graph
      (traverseStorages f $ sts)
      (traverseSequence g $ seq)

traverseSequence ::
   (Applicative f, Ord node) =>
   (v0 -> f v1) ->
   Sequence node v0 -> f (Sequence node v1)
traverseSequence f =
   traverse
      (\(rng, gr) -> fmap ((,) rng) $ FlowTopo.traverseSection f gr)

traverseStorages ::
   (Applicative f, Ord node) =>
   (a0 -> f a1) ->
   Storages node a0 -> f (Storages node a1)
traverseStorages f =
   traverse
      (uncurry (liftA2 (,)) .
       mapPair (StorageQuant.traverseGraph f, traverse f))


toAssignMap ::
   (Node.C node) =>
   Graph node a v -> AssignMap node a v
toAssignMap =
   fold .
   mapGraphWithVar AssignMap.scalarSingleton AssignMap.signalSingleton


lookupSums ::
   (Ord node) =>
   Idx.SecNode node -> Graph node a v -> Maybe (Sums v)
lookupSums (Idx.PartNode sec node) =
   Graph.lookupNode node . FlowTopo.topology <=< seqLookup sec


seqLookup ::
   Idx.Section -> Graph node a v -> Maybe (FlowTopo.Section node v)
seqLookup sec = Sequ.lookup sec . sequence



lookupStorage ::
   (Ord node) => SeqIdx.Storage node -> Graph node a v -> Maybe a
lookupStorage (Idx.ForStorage (StorageIdx.Content bnd) node) g = do
   (_,stores) <- Map.lookup node $ storages g
   Map.lookup bnd stores

lookupMaxEnergy ::
   (Ord node) => SeqIdx.MaxEnergy node -> Graph node a v -> Maybe a
lookupMaxEnergy =
   withStorage
      (\(StorageIdx.MaxEnergy se) ->
         fmap carryMaxEnergy . Storage.lookupEdge se)


withStorage ::
   (Ord node) =>
   (idx -> StorageQuant.Graph Carry a -> Maybe a) ->
   Idx.ForStorage idx node -> Graph node a v -> Maybe a
withStorage look (Idx.ForStorage idx node) =
   look idx . fst <=< Map.lookup node . storages


class
   (Env.Type (Env.TypeOf idx), Var.Index idx, Var.FormatIndex idx) =>
      Lookup idx where
   lookup ::
      (Ord node) =>
      idx node -> Graph node a v -> Maybe (Env.Element idx a v)

instance (FlowTopo.Lookup idx) => Lookup (Idx.InSection idx) where
   lookup (Idx.InPart sec idx) g =
      FlowTopo.lookup idx =<< seqLookup sec g

instance (LookupScalar idx) => Lookup (Idx.ForStorage idx) where
   lookup = lookupScalar


class (StorageVar.Index idx) => LookupScalar idx where
   lookupScalar ::
      (Ord node) => Idx.ForStorage idx node -> Graph node a v -> Maybe a

instance LookupScalar StorageIdx.MaxEnergy where
   lookupScalar = lookupMaxEnergy

instance LookupScalar StorageIdx.Content where
   lookupScalar = lookupStorage

instance LookupScalar (StorageIdx.Energy Idx.Section) where
   lookupScalar = withStorage StorageQuant.lookup

instance LookupScalar (StorageIdx.X Idx.Section) where
   lookupScalar = withStorage StorageQuant.lookup

instance LookupScalar (StorageIdx.InSum Idx.Section) where
   lookupScalar = withStorage StorageQuant.lookup

instance LookupScalar (StorageIdx.OutSum Idx.Section) where
   lookupScalar = withStorage StorageQuant.lookup


data Irrelevant = Irrelevant

instance Unknown Irrelevant where
   unknown = Irrelevant


{-
All storages must be present in the collection of storage graphs,
also if the storages are always inactive.
Only this way we can assign values to the initial sections.
-}
sequenceGraph ::
   (Node.C node, Unknown a, Unknown v) =>
   Sequ.List (Topo.FlowTopology node) -> Graph node a v
sequenceGraph =
   graphFromSections . fmap FlowTopo.sectionFromPlain

graphFromSections ::
   (Node.C node, Unknown a) =>
   Sequ.List (FlowTopo.Section node v) -> Graph node a v
graphFromSections sq =
   SeqFlow.Graph {
      storages =
         fmap
            (storageMapFromList (Fold.toList $ Sequ.mapWithSection const sq) .
             StorageQuant.forwardEdgesFromSums) $
         Env.storageSequences $
         Fold.toList $ Sequ.mapWithSection  (,) $
         fmap FlowTopo.topology sq,
      sequence = Sequ.toMap sq
   }

storageMapFromList ::
   (Unknown a) =>
   [Idx.Section] ->
   [SeqIdx.CarryEdge] ->
   (Storage.Graph Idx.Section a (Carry a), Map Idx.Boundary a)
storageMapFromList secs edges =
   (StorageQuant.graphFromList secs edges,
    Map.fromList $ map (flip (,) unknown . Idx.Following) $
    Idx.Init : map Idx.NoInit secs)



mapGraphWithVar ::
   (Ord node) =>
   (Var.Scalar node -> a0 -> a1) ->
   (Var.Signal node -> v0 -> v1) ->
   Graph node a0 v0 ->
   Graph node a1 v1
mapGraphWithVar f g gr =
   SeqFlow.Graph {
      storages = mapStoragesWithVar f gr,
      sequence = mapSequenceWithVar g $ sequence gr
   }

mapStoragesWithVar ::
   (Ord node) =>
   (Var.Scalar node -> a0 -> a1) ->
   Graph node a0 v0 ->
   Storages node a1
mapStoragesWithVar f gr =
   Map.mapWithKey
      (\node (sgr, bnds) ->
         (StorageQuant.mapGraphWithVar (flip lookupSums gr) f node sgr,
          Map.mapWithKey
             (\bnd a -> f (StorageIdx.Content bnd <#> node) a)
             bnds)) $
   storages gr

mapSequenceWithVar ::
   (Ord node) =>
   (Var.Signal node -> v0 -> v1) ->
   Sequence node v0 ->
   Sequence node v1
mapSequenceWithVar f =
   Map.mapWithKey $ \sec ->
      mapSnd $ FlowTopo.mapSectionWithVar (f . Idx.InPart sec)


formatAssigns ::
   (Node.C node, FormatValue a, FormatValue v, Format output) =>
   Graph node a v -> [output]
formatAssigns =
   foldMap (:[]) (:[]) .
   mapGraphWithVar formatAssign formatAssign

foldMap ::
   (Node.C node, Monoid w) =>
   (a -> w) -> (v -> w) -> Graph node a v -> w
foldMap fa fv =
   fold . mapGraph fa fv

fold ::
   (Node.C node, Monoid w) =>
   Graph node w w -> w
fold = MW.execWriter . traverseGraph MW.tell MW.tell

