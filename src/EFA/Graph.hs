-- remove this using functor Ord class
{-# LANGUAGE FlexibleContexts #-}
module EFA.Graph (
   Graph,
   isConsistent,
   -- inEdges, outEdges,
   graphMap,
   nodeLabels,
   edgeLabels, edgeSet, edges,

   LNode,
   Edge(from, to),
   DirEdge(DirEdge),
   UnDirEdge(UnDirEdge), unDirEdge,
   EitherEdge(EDirEdge,EUnDirEdge),
   LEdge,

   reverse,
   reverseEdge,
   ixmap,
   mapNode, mapNodeWithInOut, mapNodeWithKey,
   mapEdge, mapEdgeWithKey,
   traverseNode, traverseEdge,
   empty,
   union,
   lookupNode, lookupEdge,
   isEmpty,
   labNodes,
   labEdges,
   adjEdges,
   pre, -- lpre, preEdgeLabels,
   suc, -- lsuc, sucEdgeLabels,
   delNode,
   -- delNodes,
   delNodeSet,
   -- delEdges,
   -- delEdgeSet,
   lefilter,
   mapEdgesMaybe,
   -- elfilter,
   -- propELFilter,
   insNode, insNodes,
   insEdge, insEdges, insEdgeSet,
   fromList, fromMap,
   nodes, nodeSet, nodeEdges,
   InOut,
   ) where

-- import qualified EFA.Utility as MapU
import qualified EFA.Utility.TotalMap as TMap
import qualified EFA.Utility.TypeConstructor as TC

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Foldable as Fold
import Control.Monad (liftM2)
import Control.Applicative (Applicative, liftA2)
import Data.Traversable (traverse)
import Data.Foldable (Foldable, foldMap)
import Data.Set (Set)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Tuple.HT (fst3, snd3, thd3, mapFst3, mapThd3)
-- import Data.Char (toUpper)

import qualified Test.QuickCheck as QC

import Prelude hiding (reverse)


{-
For all Graph's the 'isConsistent' predicate must be 'True'.
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


type LNode n label = (n, label)


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



type LEdge edge node label = (edge node, label)


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
   Map node (Set node, nodeLabel, Set node)
nodes =
   fmap (\(ins,n,outs) -> (Set.map from ins, n, Set.map to outs)) .
   nodeEdges

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
ixmap ::
   (Ord (edge1 node1), Ord node0, Ord node1) =>
   (node0 -> node1) ->
   (edge0 node0 -> edge1 node1) ->
   Graph node0 edge0 nodeLabel edgeLabel ->
   Graph node1 edge1 nodeLabel edgeLabel
ixmap f g =
   Graph .
   fmap
      (\(ins,nl,outs) ->
         (Map.mapKeys g ins, nl, Map.mapKeys g outs)) .
   Map.mapKeysWith (error "Graph.ixmap: node map is not injective") f .
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


{-
inEdges, outEdges :: Graph n e nl el -> Map n (Set n)
inEdges  = fmap fst3 . nodes
outEdges = fmap thd3 . nodes
-}

nodeLabels :: (Edge e, Ord (e n), Ord n) => Graph n e nl el -> Map n nl
nodeLabels = fmap snd3 . nodes

lookupEdge :: (Edge e, Ord (e n), Ord n) => e n -> Graph n e nl el -> Maybe el
lookupEdge e (Graph g) =
   Map.lookup e . fst3 =<< Map.lookup (from e) g

isEmpty :: Graph n e nl el -> Bool
isEmpty = Map.null . graphMap

lookupNode :: (Ord n) => n -> Graph n e nl el -> Maybe nl
lookupNode n (Graph g) = fmap snd3 $ Map.lookup n g

labNodes :: (Edge e, Ord (e n), Ord n) => Graph n e nl el -> [LNode n nl]
labNodes = Map.toList . nodeLabels

labEdges :: (Edge e, Ord (e n), Ord n) => Graph n e nl el -> [LEdge e n el]
labEdges = Map.toList . edgeLabels

pre, suc ::
   (Edge e, Ord (e n), Ord n) =>
   Graph n e nl el -> n -> [n]
pre g n =
   Set.toList . Set.map from . Map.keysSet . fst3 .
   Map.findWithDefault (error "pre: unknown node") n . graphMap $ g
suc g n =
   Set.toList . Set.map to . Map.keysSet . thd3 .
   Map.findWithDefault (error "suc: unknown node") n . graphMap $ g

{-
lpre, lsuc ::
   (ConsEdge e, Ord (e n), Ord n) =>
   Graph n e nl el -> n -> [LNode n el]
lpre g@(Graph ns _els) n =
   preEdgeLabels g n $
   fst3 $ Map.findWithDefault (error "lpre: unknown node") n ns

lsuc g@(Graph ns _els) n =
   sucEdgeLabels g n $
   thd3 $ Map.findWithDefault (error "lsuc: unknown node") n ns

preEdgeLabels, sucEdgeLabels ::
   (ConsEdge e, Ord (e n), Ord n) =>
   Graph n e nl el -> n -> Set n -> [LNode n el]
preEdgeLabels g n = filterLEdges (edgeLabels g) (flip Edge n)
sucEdgeLabels g n = filterLEdges (edgeLabels g) (Edge n)

filterLEdges ::
   (Ord n, Ord e) =>
   Map e el -> (n -> e) -> Set n -> [(n, el)]
filterLEdges els edge =
   Map.elems . Map.intersectionWith (flip (,)) els .
   Map.mapKeys edge . MapU.fromSet id
-}


adjEdges ::
   (Edge e, Ord (e n), Ord n) =>
   Graph n e nl el -> n -> Set (e n)
adjEdges g n =
   (\(ins,_nl,outs) -> Map.keysSet ins `Set.union` Map.keysSet outs) $
   Map.findWithDefault (error "adjEdges: unknown node") n $
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
delNode ::
   (Edge e, Ord (e n), Ord n) =>
   n -> Graph n e nl el -> Graph n e nl el
delNode n (Graph ns) =
   case Map.findWithDefault (error "delNode: unknown node") n ns of
      (ins, _nl, outs) ->
         Graph $
         applyMap (Map.mapKeys from $ Map.mapWithKey (\e _ -> mapThd3 $ Map.delete e) ins)  $
         applyMap (Map.mapKeys to   $ Map.mapWithKey (\e _ -> mapFst3 $ Map.delete e) outs) $
         Map.delete n ns

{- |
Could be implemented more efficiently.
-}
delNodeSet ::
   (Edge e, Ord (e n), Ord n) =>
   Set n -> Graph n e nl el -> Graph n e nl el
delNodeSet delNs g = Set.foldl (flip delNode) g delNs

{-
nodeEdges ::
   (ConsEdge edge, Ord (edge n), Ord n) =>
   n -> (Set n, a, Set n) -> Set (edge n)
nodeEdges n (ins, _, outs) =
   Set.union
      (setMapMaybe (flip edge n) ins)
      (setMapMaybe (edge n) outs)

setMapMaybe ::
   Ord b =>
   (a -> Maybe b) -> Set a -> Set b
setMapMaybe p =
   Set.fromList . mapMaybe p . Set.toList

delNodes ::
   (ConsEdge e, Ord (e n), Ord n) =>
   [n] -> Graph n e nl el -> Graph n e nl el
delNodes nsl = delNodeSet $ Set.fromList nsl

delEdgeSet ::
   (Edge e, Ord (e n), Ord n) =>
   Set (e n) -> Graph n e nl el -> Graph n e nl el
delEdgeSet es g =
   delEdgeHelp g
      (MapU.differenceSet (edgeLabels g) es,
       Set.toList es)

delEdges ::
   (Edge e, Ord (e n), Ord n) =>
   [e n] -> Graph n e nl el -> Graph n e nl el
delEdges es g =
   delEdgeHelp g
      (MapU.differenceSet (edgeLabels g) $ Set.fromList es, es)

elfilter ::
   (Edge e, Ord (e n), Ord n) =>
   (el -> Bool) ->
   Graph n e nl el -> Graph n e nl el
elfilter f g =
   delEdgeHelp g $ mapSnd Map.keys $ Map.partition f $ edgeLabels g
-}

lefilter ::
   (Edge e, Ord (e n), Ord n) =>
   (LEdge e n el -> Bool) ->
   Graph n e nl el -> Graph n e nl el
lefilter f =
   Graph .
   fmap
      (\(ins, nl, outs) ->
         (Map.filterWithKey (curry f) ins, nl,
          Map.filterWithKey (curry f) outs)) .
   graphMap

{- |
You may only use this for filtering edges
and use more specialised types as a result.
You must not alter source and target nodes of edges.
-}
mapEdgesMaybe ::
   (Edge e1, Ord (e1 n), Ord n) =>
   (e0 n -> Maybe (e1 n)) ->
   Graph n e0 nl el -> Graph n e1 nl el
mapEdgesMaybe f =
   let g = Map.fromList . mapMaybe (\(k,a) -> fmap (flip (,) a) $ f k) . Map.toList
   in  Graph .
       fmap (\(ins, nl, outs) -> (g ins, nl, g outs)) .
       graphMap

{-
delEdgeHelp ::
   (Edge e, Ord n) =>
   Graph n e nl el -> (Map (e n) el, [e n]) -> Graph n e nl el
delEdgeHelp (Graph ns _els) (kept, deleted) =
   Graph
      (fmap
         (\(ins, n, outs) delIns delOuts ->
            (Set.difference ins delIns, n, Set.difference outs delOuts)) ns
         $$ makeInMap  ns deleted
         $$ makeOutMap ns deleted)
      kept


propELFilter :: [LEdge DirEdge Char Int] -> Bool
propELFilter =
   uncurry (==) . compareELFilter . Map.fromList

compareELFilter ::
   Map (DirEdge Char) Int ->
   (Graph Char DirEdge String Int, Graph Char DirEdge String Int)
compareELFilter es =
   let ns =
          MapU.fromSet (\n -> [n, toUpper n]) $
          foldMap (foldMap Set.singleton) $ Map.keys es
   in  (elfilter even $ fromMap ns es,
        fromMap ns $ Map.filter even es)
-}

{- |
In the current implementation
existing nodes are replaced with new labels
and existing edges are maintained.
However, I think we should better have an extra function for this purpose
and you should not rely on this behavior.
-}
insNode ::
   (Ord n) => LNode n nl -> Graph n e nl el -> Graph n e nl el
insNode (n,nl) =
   Graph .
   Map.insertWith
      (\_ (ins, _, outs) -> (ins, nl, outs))
      n (Map.empty, nl, Map.empty) .
   graphMap

insNodes ::
   (Ord n) => [LNode n nl] -> Graph n e nl el -> Graph n e nl el
insNodes = flip (foldl (flip insNode))

insEdge ::
   (Edge e, Ord (e n), Ord n) =>
   LEdge e n el -> Graph n e nl el -> Graph n e nl el
insEdge es = insEdges [es]

insEdges ::
   (Edge e, Ord (e n), Ord n) =>
   [LEdge e n el] -> Graph n e nl el -> Graph n e nl el
insEdges es = insEdgeSet (Map.fromList es)

{- |
In the current implementation
existing edges are replaced with new labels.
However, I think we should better have an extra function for this purpose
and you should not rely on this behavior.
-}
insEdgeSet ::
   (Edge e, Ord (e n), Ord n) =>
   Map (e n) el -> Graph n e nl el -> Graph n e nl el
insEdgeSet es =
   let ess = Map.mapWithKey Map.singleton es
   in  Graph .
       applyMap (fmap (\new -> mapFst3 (Map.union new)) $ Map.mapKeysWith Map.union to   ess) .
       applyMap (fmap (\new -> mapThd3 (Map.union new)) $ Map.mapKeysWith Map.union from ess) .
       graphMap

fromList ::
   (Edge e, Ord (e n), Ord n) =>
   [LNode n nl] -> [LEdge e n el] -> Graph n e nl el
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


{-
infixl 0 $$

($$) :: (Ord n) => Map n (a -> b) -> Map n a -> Map n b
($$) = Map.intersectionWith ($)

makeMap :: (Ord n) =>
   (e n -> n, e n -> n) ->
   Map n nl -> [e n] -> Map n (Set n)
makeMap (selFrom, selTo) ns =
   flip Map.union (fmap (const Set.empty) ns) .
   Map.fromListWith Set.union .
   map (\e -> (selFrom e, Set.singleton $ selTo e))

makeOutMap, makeInMap ::
   (Edge e, Ord n) => Map n nl -> [e n] -> Map n (Set n)
makeOutMap = makeMap (from, to)
makeInMap  = makeMap (to, from)
-}

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


type InOut n e nl el = ([LEdge e n el], LNode n nl, [LEdge e n el])

mapNodeWithInOut ::
   (Edge e, Ord (e n), Ord n) =>
   (InOut n e nl0 el -> nl1) -> Graph n e nl0 el -> Graph n e nl1 el
mapNodeWithInOut f =
   Graph .
   Map.mapWithKey
      (\n (ins,nl,outs) ->
         (ins, f (Map.toList ins, (n,nl), Map.toList outs), outs)) .
   graphMap

{-
inOut ::
   (ConsEdge e, Ord (e n), Ord n) =>
   Graph n e nl el ->
   n -> (Set n, t, Set n) ->
   ([LNode n el], (n, t), [LNode n el])
inOut g n (ins,nl,outs) =
   (preEdgeLabels g n ins,
    (n,nl),
    sucEdgeLabels g n outs)

mkInOutGraphFormat ::
   (ConsEdge e, Ord (e n), Ord n) =>
   Graph n e nl el -> [InOut n nl el]
mkInOutGraphFormat g@(Graph ns _els) =
   map (\(n, ios) -> inOut g n ios) $
   Map.toList ns


mapGraph ::
   (ConsEdge e, Ord (e n), Ord n) =>
   (InOut n nl el -> a) -> Graph n e nl el -> [a]
mapGraph f g = map f (mkInOutGraphFormat g)
-}

{- |
Don't rely on a particular order of traversal!
That is, the actions should commute.
-}
traverseNode ::
   (Applicative f) =>
   (nl0 -> f nl1) -> Graph n e nl0 el -> f (Graph n e nl1 el)
traverseNode f =
   fmap Graph .
   traverse (\(ins,n,outs) -> fmap (\fn -> (ins, fn, outs)) (f n)) .
   graphMap

{- |
Don't rely on a particular order of traversal!
Due to the current implementation all edges are accessed twice.
Don't rely on this behaviour!
That is, the actions should be commutative and non-destructive.
-}
traverseEdge ::
   (Applicative f) =>
   (el0 -> f el1) -> Graph n e nl el0 -> f (Graph n e nl el1)
traverseEdge f =
   fmap Graph .
   traverse
      (\(ins,n,outs) ->
         liftA2 (\fi fo -> (fi,n,fo)) (traverse f ins) (traverse f outs)) .
   graphMap
