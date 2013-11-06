-- remove this using functor Ord class
{-# LANGUAGE FlexibleContexts #-}
module EFA.Graph (
   -- * types
   Graph,
   LabeledNode,
   LabeledEdge,
   Edge(from, to),
   DirEdge(DirEdge),
   UnDirEdge(UnDirEdge), unDirEdge,
   EitherEdge(EDirEdge,EUnDirEdge),

   -- * construction
   empty, fromList, fromMap,

   -- * extract large portions of the graph
   graphMap,
   nodeLabels, nodeSet, nodes, nodeEdges,
   edgeLabels, edgeSet, edges,

   -- * queries
   isEmpty,
   lookupNode, lookupEdge,
   adjacentEdges,
   isLoop,
   pathExists,
   isConsistent,

   -- * manipulate labels
   mapNode, mapNodeWithKey,
   mapEdge, mapEdgeWithKey,
   mapNodeWithInOut, InOut,
   filterEdgeWithKey,
   traverseNode, traverseEdge, traverse,

   -- * combine graphs
   checkedZipWith,
   union,

   -- * manipulate indices
   reverse,
   reverseEdge,
   mapKeys,
   mapMaybeEdgeKeys,
   mapEdgeKeys,

   -- * insertion and removal
   deleteNode, deleteNodeSet, deleteEdge,
   insertNode, insertEdge, insertEdgeSet,
   ) where

import qualified EFA.Utility.TypeConstructor as TC
import qualified EFA.Utility.TotalMap as TMap
import qualified EFA.Utility.Map as MapU

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import Control.Monad (liftM2)
import Control.Applicative (Applicative, pure, liftA2, liftA3)
import Data.Foldable (Foldable, foldMap)
import Data.Set (Set)
import Data.Map (Map)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Tuple.HT (fst3, snd3, thd3, mapFst3, mapThd3)

import qualified Test.QuickCheck as QC

import Prelude hiding (reverse)


{-
For all 'Graph's the 'isConsistent' predicate must be 'True'.
-}
newtype Graph node edge nodeLabel edgeLabel =
   Graph {
      graphMap ::
         Map node
            (Map (edge node) edgeLabel,
             nodeLabel,
             Map (edge node) edgeLabel)
   } deriving (Eq, Ord)

instance
   (Edge e, Ord n, Ord (e n),
    Show n, Show (e n), Show nl, Show el) =>
      Show (Graph n e nl el) where
   showsPrec prec g =
      showParen (prec>10) $
         showString "Graph.fromList " .
         shows (Map.toList $ nodeLabels g) .
         showString " " .
         shows (Map.toList $ edgeLabels g)


isConsistent :: (Ord n, Eq el) => Graph n DirEdge nl el -> Bool
isConsistent (Graph ns) =
   foldMap fst3 ns == foldMap thd3 ns
   &&
   Set.isSubsetOf
      (foldMap (foldMap (foldMap Set.singleton) . Map.keys . fst3) ns)
      (Map.keysSet ns)
   &&
   (Fold.and $ flip Map.mapWithKey ns $
      \n (ins,_nl,outs) ->
         Fold.all ((n==) . to) (Map.keysSet ins) &&
         Fold.all ((n==) . from) (Map.keysSet outs))


type LabeledNode n label = (n, label)


class (Foldable edge) => Edge edge where
   from, to :: edge node -> node

instance Edge DirEdge where
   from (DirEdge x _) = x
   to (DirEdge _ x) = x

instance Edge UnDirEdge where
   from (UnDirEdge x _) = x
   to (UnDirEdge _ x) = x

instance Edge EitherEdge where
   from ee =
      case ee of
         EDirEdge   e -> from e
         EUnDirEdge e -> from e
   to ee =
      case ee of
         EDirEdge   e -> to e
         EUnDirEdge e -> to e


{-
class (Edge edge) => ConsEdge edge where
   {- |
   The construction of an edge may fail
   and it is not warranted
   that @x == from (edge x y)@ or @y == to (edge x y)@.
   -}
   edge :: Ord node => node -> node -> Maybe (edge node)

instance ConsEdge DirEdge where
   edge x y = Just $ DirEdge x y

instance ConsEdge UnDirEdge where
   edge x y = Just $ unDirEdge x y
-}



type LabeledEdge edge node label = (edge node, label)


data DirEdge node = DirEdge node node
   deriving (Eq, Ord, Show)

data UnDirEdge node = UnDirEdge node node
   deriving (Eq, Ord, Show)

unDirEdge :: (Ord node) => node -> node -> UnDirEdge node
unDirEdge x y =
   if x<y
     then UnDirEdge x y
     else UnDirEdge y x

data
   EitherEdge node =
        EDirEdge (DirEdge node)
      | EUnDirEdge (UnDirEdge node)
   deriving (Eq, Ord, Show)


instance TC.Eq DirEdge where eq = (==)
instance TC.Ord DirEdge where cmp = compare
instance TC.Show DirEdge where showsPrec = showsPrec

instance TC.Eq UnDirEdge where eq = (==)
instance TC.Ord UnDirEdge where cmp = compare
instance TC.Show UnDirEdge where showsPrec = showsPrec

instance TC.Eq EitherEdge where eq = (==)
instance TC.Ord EitherEdge where cmp = compare
instance TC.Show EitherEdge where showsPrec = showsPrec


instance Functor DirEdge where
   fmap f (DirEdge x y) = DirEdge (f x) (f y)

instance Foldable DirEdge where
   foldMap f (DirEdge x y) = mappend (f x) (f y)

instance Foldable UnDirEdge where
   foldMap f (UnDirEdge x y) = mappend (f x) (f y)

instance Foldable EitherEdge where
   foldMap f ee =
      case ee of
         EDirEdge   e -> foldMap f e
         EUnDirEdge e -> foldMap f e

instance (QC.Arbitrary n) => QC.Arbitrary (DirEdge n) where
   arbitrary = liftM2 DirEdge QC.arbitrary QC.arbitrary
   shrink (DirEdge x y) = map (uncurry DirEdge) $ QC.shrink (x,y)

instance (QC.Arbitrary n, Ord n) => QC.Arbitrary (UnDirEdge n) where
   arbitrary = liftM2 unDirEdge QC.arbitrary QC.arbitrary
   shrink (UnDirEdge x y) =
      Set.toList $ Set.fromList $ map (uncurry unDirEdge) $ QC.shrink (x,y)


nodes ::
   (Edge edge, Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel ->
   [node]
nodes = Map.keys . graphMap

nodeEdges ::
   (Edge edge, Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel ->
   Map node (Set (edge node), nodeLabel, Set (edge node))
nodeEdges =
   fmap (\(ins,n,outs) -> (Map.keysSet ins, n, Map.keysSet outs)) .
   graphMap


edgeLabels ::
   (Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel ->
   Map (edge node) edgeLabel
edgeLabels =
   foldMap fst3 . graphMap

edgeSet ::
   (Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel -> Set (edge node)
edgeSet = foldMap (Map.keysSet . fst3) . graphMap

edges ::
   (Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel -> [edge node]
edges = Map.keys . edgeLabels


reverse ::
   (Reverse e, Ord (e n), Ord n) =>
   Graph n e nl el -> Graph n e nl el
reverse =
   Graph .
   fmap
      (\(ins, nl, outs) ->
         (Map.mapKeys reverseEdge outs, nl, Map.mapKeys reverseEdge ins)) .
   graphMap


class Edge edge => Reverse edge where
   reverseEdge :: edge node -> edge node

instance Reverse DirEdge where
   reverseEdge (DirEdge x y) = DirEdge y x


{- |
The index map must be an injection,
that is, nodes must not collaps.
Also the node and edge index maps must be consistent, i.e.

> from (edgeMap e) == nodeMap (from e)
> to   (edgeMap e) == nodeMap (to   e)

Strictly spoken, we would need the node map only for isolated nodes,
but we use it for all nodes for simplicity.
-}
mapKeys ::
   (Ord (edge1 node1), Ord node0, Ord node1) =>
   (node0 -> node1) ->
   (edge0 node0 -> edge1 node1) ->
   Graph node0 edge0 nodeLabel edgeLabel ->
   Graph node1 edge1 nodeLabel edgeLabel
mapKeys f g =
   Graph .
   fmap
      (\(ins,nl,outs) ->
         (Map.mapKeys g ins, nl, Map.mapKeys g outs)) .
   Map.mapKeysWith (error "Graph.mapKeys: node map is not injective") f .
   graphMap

empty :: Graph node edge nodeLabel edgeLabel
empty = Graph Map.empty

{- |
The node sets must be disjoint.
-}
union ::
   (Edge edge, Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel ->
   Graph node edge nodeLabel edgeLabel ->
   Graph node edge nodeLabel edgeLabel
union (Graph ns0) (Graph ns1) =
   Graph
      (Map.unionWith (error "Graph.union: node sets overlap") ns0 ns1)

instance
   (Edge edge, Ord (edge node), Ord node) =>
      Monoid (Graph node edge nodeLabel edgeLabel) where
   mempty = empty
   mappend = union


{- |
Node and edge sets must be equal.
-}
checkedZipWith ::
   (Edge edge, Ord (edge node), Ord node) =>
   MapU.Caller ->
   (nodeLabel0 -> nodeLabel1 -> nodeLabel2) ->
   (edgeLabel0 -> edgeLabel1 -> edgeLabel2) ->
   Graph node edge nodeLabel0 edgeLabel0 ->
   Graph node edge nodeLabel1 edgeLabel1 ->
   Graph node edge nodeLabel2 edgeLabel2
checkedZipWith caller f g (Graph ns0) (Graph ns1) =
   Graph $
   MapU.checkedZipWith (caller ++ " node")
      (\(ins0, n0, outs0) (ins1, n1, outs1) ->
         (MapU.checkedZipWith (caller ++ " ins") g ins0 ins1,
          f n0 n1,
          MapU.checkedZipWith (caller ++ " outs") g outs0 outs1))
      ns0 ns1


nodeLabels :: (Edge e, Ord (e n), Ord n) => Graph n e nl el -> Map n nl
nodeLabels = fmap snd3 . graphMap

lookupEdge :: (Edge e, Ord (e n), Ord n) => e n -> Graph n e nl el -> Maybe el
lookupEdge e (Graph g) =
   Map.lookup e . thd3 =<< Map.lookup (from e) g

{- |
Alternative implementation for test:
-}
_lookupEdge :: (Edge e, Ord (e n), Ord n) => e n -> Graph n e nl el -> Maybe el
_lookupEdge e (Graph g) =
   Map.lookup e . fst3 =<< Map.lookup (to e) g


isEmpty :: Graph n e nl el -> Bool
isEmpty = Map.null . graphMap

lookupNode :: (Ord n) => n -> Graph n e nl el -> Maybe nl
lookupNode n (Graph g) = fmap snd3 $ Map.lookup n g

_pre, suc ::
   (Edge e, Ord (e n), Ord n) =>
   Graph n e nl el -> n -> [n]
_pre g n =
   Set.toList . Set.map from . Map.keysSet . fst3 .
   Map.findWithDefault (error "pre: unknown node") n . graphMap $ g
suc g n =
   Set.toList . Set.map to . Map.keysSet . thd3 .
   Map.findWithDefault (error "suc: unknown node") n . graphMap $ g

adjacentEdges ::
   (Edge e, Ord (e n), Ord n) =>
   Graph n e nl el -> n -> Set (e n)
adjacentEdges g n =
   (\(ins,_nl,outs) -> Map.keysSet ins `Set.union` Map.keysSet outs) $
   Map.findWithDefault (error "adjacentEdges: unknown node") n $
   graphMap g

{-
In constrast to Map.intersectWith ($), unaffected values are preserved.
-}
applyMap :: (Ord k) => Map k (a -> a) -> Map k a -> Map k a
applyMap f x =
   Map.union (Map.intersectionWith ($) f x) x

{- |
Node to be deleted must be contained in the graph.
-}
deleteNode ::
   (Edge e, Ord (e n), Ord n) =>
   n -> Graph n e nl el -> Graph n e nl el
deleteNode n (Graph ns) =
   case Map.findWithDefault (error "deleteNode: unknown node") n ns of
      (ins, _nl, outs) ->
         Graph $
         applyMap (Map.mapKeys from $ Map.mapWithKey (\e _ -> mapThd3 $ Map.delete e) ins)  $
         applyMap (Map.mapKeys to   $ Map.mapWithKey (\e _ -> mapFst3 $ Map.delete e) outs) $
         Map.delete n ns

{- |
Could be implemented more efficiently.
-}
deleteNodeSet ::
   (Edge e, Ord (e n), Ord n) =>
   Set n -> Graph n e nl el -> Graph n e nl el
deleteNodeSet delNs g = Set.foldl (flip deleteNode) g delNs

deleteEdge ::
   (Edge e, Ord (e n), Ord n) =>
   e n -> Graph n e nl el -> Graph n e nl el
deleteEdge e (Graph ns) =
   Graph $
   Map.adjust (mapThd3 $ Map.delete e) (from e) $
   Map.adjust (mapFst3 $ Map.delete e) (to e) $
   ns

filterEdgeWithKey ::
   (Edge e, Ord (e n), Ord n) =>
   (e n -> el -> Bool) ->
   Graph n e nl el -> Graph n e nl el
filterEdgeWithKey f =
   Graph .
   fmap
      (\(ins, nl, outs) ->
         (Map.filterWithKey f ins, nl,
          Map.filterWithKey f outs)) .
   graphMap

{- |
You may only use this for filtering edges
and use more specialised types as a result.
You must not alter source and target nodes of edges.
-}
mapMaybeEdgeKeys ::
   (Edge e1, Ord (e1 n), Ord n) =>
   (e0 n -> Maybe (e1 n)) ->
   Graph n e0 nl el -> Graph n e1 nl el
mapMaybeEdgeKeys f =
   Graph .
   fmap
      (\(ins, nl, outs) ->
         (MapU.mapMaybeKeys f ins, nl, MapU.mapMaybeKeys f outs)) .
   graphMap

mapEdgeKeys ::
   (Edge e1, Ord (e1 n), Ord n) =>
   (e0 n -> e1 n) ->
   Graph n e0 nl el -> Graph n e1 nl el
mapEdgeKeys f =
   Graph .
   fmap
      (\(ins, nl, outs) ->
         (Map.mapKeys f ins, nl, Map.mapKeys f outs)) .
   graphMap

{- |
In the current implementation
existing nodes are replaced with new labels
and existing edges are maintained.
However, I think we should better have an extra function for this purpose
and you should not rely on this behavior.
-}
insertNode ::
   (Ord n) => n -> nl -> Graph n e nl el -> Graph n e nl el
insertNode n nl =
   Graph .
   Map.insertWith
      (\_ (ins, _, outs) -> (ins, nl, outs))
      n (Map.empty, nl, Map.empty) .
   graphMap

insertEdge ::
   (Edge e, Ord (e n), Ord n) =>
   e n -> el -> Graph n e nl el -> Graph n e nl el
insertEdge e el = insertEdgeSet $ Map.singleton e el

{- |
In the current implementation
existing edges are replaced with new labels.
However, I think we should better have an extra function for this purpose
and you should not rely on this behavior.
-}
insertEdgeSet ::
   (Edge e, Ord (e n), Ord n) =>
   Map (e n) el -> Graph n e nl el -> Graph n e nl el
insertEdgeSet es =
   let ess = Map.mapWithKey Map.singleton es
   in  Graph .
       applyMap (fmap (\new -> mapFst3 (Map.union new)) $ Map.mapKeysWith Map.union to   ess) .
       applyMap (fmap (\new -> mapThd3 (Map.union new)) $ Map.mapKeysWith Map.union from ess) .
       graphMap

fromList ::
   (Edge e, Ord (e n), Ord n) =>
   [LabeledNode n nl] -> [LabeledEdge e n el] -> Graph n e nl el
fromList ns es =
   fromMap (Map.fromList ns) $ Map.fromList es

fromMap ::
   (Edge e, Ord (e n), Ord n) =>
   Map n nl -> Map (e n) el -> Graph n e nl el
fromMap ns es =
   let ess = Map.mapWithKey Map.singleton es
   in  Graph $
       TMap.intersectionPartialWith (\ins (outs, nl) -> (ins,nl,outs))
          (TMap.cons Map.empty $ Map.mapKeysWith Map.union to   ess) $
       TMap.intersectionPartialWith (,)
          (TMap.cons Map.empty $ Map.mapKeysWith Map.union from ess) ns


mapNode :: (nl0 -> nl1) -> Graph n e nl0 el -> Graph n e nl1 el
mapNode f =
   Graph . fmap (\(ins,n,outs) -> (ins, f n, outs)) . graphMap

mapNodeWithKey :: (n -> nl0 -> nl1) -> Graph n e nl0 el -> Graph n e nl1 el
mapNodeWithKey f =
   Graph .
   Map.mapWithKey (\n (ins,nl,outs) -> (ins, f n nl, outs)) .
   graphMap

mapEdge :: (el0 -> el1) -> Graph n e nl el0 -> Graph n e nl el1
mapEdge f =
   Graph . fmap (\(ins,n,outs) -> (fmap f ins, n, fmap f outs)) . graphMap

mapEdgeWithKey :: (e n -> el0 -> el1) -> Graph n e nl el0 -> Graph n e nl el1
mapEdgeWithKey f =
   Graph .
   fmap (\(ins,n,outs) -> (Map.mapWithKey f ins, n, Map.mapWithKey f outs)) .
   graphMap

nodeSet :: Graph n e nl el -> Set n
nodeSet = Map.keysSet . graphMap


type
   InOut n e nl el =
      ([LabeledEdge e n el], LabeledNode n nl, [LabeledEdge e n el])

mapNodeWithInOut ::
   (Edge e, Ord (e n), Ord n) =>
   (InOut n e nl0 el -> nl1) -> Graph n e nl0 el -> Graph n e nl1 el
mapNodeWithInOut f =
   Graph .
   Map.mapWithKey
      (\n (ins,nl,outs) ->
         (ins, f (Map.toList ins, (n,nl), Map.toList outs), outs)) .
   graphMap


{- |
Same restrictions as in 'traverse'.
-}
traverseNode ::
   (Applicative f, Ord n, Ord (e n), Edge e) =>
   (nl0 -> f nl1) -> Graph n e nl0 el -> f (Graph n e nl1 el)
traverseNode f = traverse f pure

{- |
Same restrictions as in 'traverse'.
-}
traverseEdge ::
   (Applicative f, Ord n, Ord (e n), Edge e) =>
   (el0 -> f el1) -> Graph n e nl el0 -> f (Graph n e nl el1)
traverseEdge f = traverse pure f

{- |
Don't rely on a particular order of traversal!
-}
traverse, _traverseNaive ::
   (Applicative f, Ord n, Ord (e n), Edge e) =>
   (nl0 -> f nl1) ->
   (el0 -> f el1) ->
   Graph n e nl0 el0 -> f (Graph n e nl1 el1)
traverse fn fe gr =
   liftA2 fromMap
      (Trav.traverse fn $ nodeLabels gr)
      (Trav.traverse fe $ edgeLabels gr)

{-
Due to the current implementation all edges are accessed twice.
That is, the actions should be commutative and non-destructive.
-}
_traverseNaive fn fe =
   fmap Graph .
   Trav.traverse
      (\(ins,n,outs) ->
         liftA3 (,,) (Trav.traverse fe ins) (fn n) (Trav.traverse fe outs)) .
   graphMap


isLoop :: (Edge edge, Eq node) => edge node -> Bool
isLoop e = from e == to e

pathExists ::
   (Ord node, Ord (edge node), Edge edge) =>
   node -> node -> Graph node edge nodeLabel edgeLabel -> Bool
pathExists src dst =
   let go topo a =
          not (isEmpty topo) &&
          (a==dst ||
           (any (go (deleteNode a topo)) $ suc topo a))
   in  flip go src
