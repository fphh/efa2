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
   ixmap, nmap, emap, nmapWithInOut,
   empty,
   union,
   -- getLEdge,
   isEmpty,
   -- lab,
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

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Foldable as Fold
import Control.Monad (liftM2)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Foldable (Foldable, foldMap)
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
         M.Map node
            (M.Map (edge node) edgeLabel,
             nodeLabel,
             M.Map (edge node) edgeLabel)
   } deriving (Eq)

instance
   (Edge e, Ord n, Ord (e n),
    Show n, Show (e n), Show nl, Show el) =>
      Show (Graph n e nl el) where
   showsPrec prec g =
      showParen (prec>10) $
         showString "Graph.fromList " .
         shows (M.toList $ nodeLabels g) .
         showString " " .
         shows (M.toList $ edgeLabels g)


isConsistent :: (Ord n, Eq el) => Graph n DirEdge nl el -> Bool
isConsistent (Graph ns) =
   foldMap fst3 ns == foldMap thd3 ns
   &&
   (Fold.and $ flip M.mapWithKey ns $
      \n (ins,_nl,outs) ->
         Fold.all ((n==) . to) (M.keysSet ins) &&
         Fold.all ((n==) . from) (M.keysSet outs))


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
      S.toList $ S.fromList $ map (uncurry unDirEdge) $ QC.shrink (x,y)


nodes ::
   (Edge edge, Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel ->
   M.Map node (S.Set node, nodeLabel, S.Set node)
nodes =
   fmap (\(ins,n,outs) -> (S.map from ins, n, S.map to outs)) .
   nodeEdges

nodeEdges ::
   (Edge edge, Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel ->
   M.Map node (S.Set (edge node), nodeLabel, S.Set (edge node))
nodeEdges =
   fmap (\(ins,n,outs) -> (M.keysSet ins, n, M.keysSet outs)) .
   graphMap


edgeLabels ::
   (Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel ->
   M.Map (edge node) edgeLabel
edgeLabels =
   foldMap fst3 . graphMap

edgeSet ::
   (Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel -> S.Set (edge node)
edgeSet = foldMap (M.keysSet . fst3) . graphMap

edges ::
   (Ord (edge node), Ord node) =>
   Graph node edge nodeLabel edgeLabel -> [edge node]
edges = M.keys . edgeLabels


reverse ::
   (Reverse e, Ord (e n), Ord n) =>
   Graph n e nl el -> Graph n e nl el
reverse =
   Graph .
   fmap
      (\(ins, nl, outs) ->
         (M.mapKeys reverseEdge outs, nl, M.mapKeys reverseEdge ins)) .
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
         (M.mapKeys g ins, nl, M.mapKeys g outs)) .
   M.mapKeysWith (error "Graph.ixmap: node map is not injective") f .
   graphMap

empty :: Graph node edge nodeLabel edgeLabel
empty = Graph M.empty

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
      (M.unionWith (error "Graph.union: node sets overlap") ns0 ns1)

instance
   (Edge edge, Ord (edge node), Ord node) =>
      Monoid (Graph node edge nodeLabel edgeLabel) where
   mempty = empty
   mappend = union


{-
inEdges, outEdges :: Graph n e nl el -> M.Map n (S.Set n)
inEdges  = fmap fst3 . nodes
outEdges = fmap thd3 . nodes
-}

nodeLabels :: (Edge e, Ord (e n), Ord n) => Graph n e nl el -> M.Map n nl
nodeLabels = fmap snd3 . nodes

{-
getLEdge :: (Ord n) => Graph n nl el -> n -> n -> Maybe (LEdge n el)
getLEdge g x y =
   let e = Edge x y
   in  fmap ((,) e) $ M.lookup e (edgeLabels g)
-}

isEmpty :: Graph n e nl el -> Bool
isEmpty = M.null . graphMap

{-
lab :: Ord n => Graph n nl el -> n -> Maybe nl
lab g n = fmap snd3 $ M.lookup n (nodes g)
-}

labNodes :: (Edge e, Ord (e n), Ord n) => Graph n e nl el -> [LNode n nl]
labNodes = M.toList . nodeLabels

labEdges :: (Edge e, Ord (e n), Ord n) => Graph n e nl el -> [LEdge e n el]
labEdges = M.toList . edgeLabels

pre, suc ::
   (Edge e, Ord (e n), Ord n) =>
   Graph n e nl el -> n -> [n]
pre g n =
   S.toList . S.map from . M.keysSet . fst3 .
   M.findWithDefault (error "pre: unknown node") n . graphMap $ g
suc g n =
   S.toList . S.map to . M.keysSet . thd3 .
   M.findWithDefault (error "suc: unknown node") n . graphMap $ g

{-
lpre, lsuc ::
   (ConsEdge e, Ord (e n), Ord n) =>
   Graph n e nl el -> n -> [LNode n el]
lpre g@(Graph ns _els) n =
   preEdgeLabels g n $
   fst3 $ M.findWithDefault (error "lpre: unknown node") n ns

lsuc g@(Graph ns _els) n =
   sucEdgeLabels g n $
   thd3 $ M.findWithDefault (error "lsuc: unknown node") n ns

preEdgeLabels, sucEdgeLabels ::
   (ConsEdge e, Ord (e n), Ord n) =>
   Graph n e nl el -> n -> S.Set n -> [LNode n el]
preEdgeLabels g n = filterLEdges (edgeLabels g) (flip Edge n)
sucEdgeLabels g n = filterLEdges (edgeLabels g) (Edge n)

filterLEdges ::
   (Ord n, Ord e) =>
   M.Map e el -> (n -> e) -> S.Set n -> [(n, el)]
filterLEdges els edge =
   M.elems . M.intersectionWith (flip (,)) els .
   M.mapKeys edge . MapU.fromSet id
-}


adjEdges ::
   (Edge e, Ord (e n), Ord n) =>
   Graph n e nl el -> n -> S.Set (e n)
adjEdges g n =
   (\(ins,_nl,outs) -> M.keysSet ins `S.union` M.keysSet outs) $
   M.findWithDefault (error "adjEdges: unknown node") n $
   graphMap g

{-
In constrast to Map.intersectWith ($), unaffected values are preserved.
-}
applyMap :: (Ord k) => M.Map k (a -> a) -> M.Map k a -> M.Map k a
applyMap f x =
   M.union (M.intersectionWith ($) f x) x

{- |
Node to be deleted must be contained in the graph.
-}
delNode ::
   (Edge e, Ord (e n), Ord n) =>
   n -> Graph n e nl el -> Graph n e nl el
delNode n (Graph ns) =
   case M.findWithDefault (error "delNode: unknown node") n ns of
      (ins, _nl, outs) ->
         Graph $
         applyMap (M.mapKeys from $ M.mapWithKey (\e _ -> mapThd3 $ M.delete e) ins)  $
         applyMap (M.mapKeys to   $ M.mapWithKey (\e _ -> mapFst3 $ M.delete e) outs) $
         M.delete n ns

{- |
Could be implemented more efficiently.
-}
delNodeSet ::
   (Edge e, Ord (e n), Ord n) =>
   S.Set n -> Graph n e nl el -> Graph n e nl el
delNodeSet delNs g = S.foldl (flip delNode) g delNs

{-
nodeEdges ::
   (ConsEdge edge, Ord (edge n), Ord n) =>
   n -> (S.Set n, a, S.Set n) -> S.Set (edge n)
nodeEdges n (ins, _, outs) =
   S.union
      (setMapMaybe (flip edge n) ins)
      (setMapMaybe (edge n) outs)

setMapMaybe ::
   Ord b =>
   (a -> Maybe b) -> S.Set a -> S.Set b
setMapMaybe p =
   S.fromList . mapMaybe p . S.toList

delNodes ::
   (ConsEdge e, Ord (e n), Ord n) =>
   [n] -> Graph n e nl el -> Graph n e nl el
delNodes nsl = delNodeSet $ S.fromList nsl

delEdgeSet ::
   (Edge e, Ord (e n), Ord n) =>
   S.Set (e n) -> Graph n e nl el -> Graph n e nl el
delEdgeSet es g =
   delEdgeHelp g
      (MapU.differenceSet (edgeLabels g) es,
       S.toList es)

delEdges ::
   (Edge e, Ord (e n), Ord n) =>
   [e n] -> Graph n e nl el -> Graph n e nl el
delEdges es g =
   delEdgeHelp g
      (MapU.differenceSet (edgeLabels g) $ S.fromList es, es)

elfilter ::
   (Edge e, Ord (e n), Ord n) =>
   (el -> Bool) ->
   Graph n e nl el -> Graph n e nl el
elfilter f g =
   delEdgeHelp g $ mapSnd M.keys $ M.partition f $ edgeLabels g
-}

lefilter ::
   (Edge e, Ord (e n), Ord n) =>
   (LEdge e n el -> Bool) ->
   Graph n e nl el -> Graph n e nl el
lefilter f =
   Graph .
   fmap
      (\(ins, nl, outs) ->
         (M.filterWithKey (curry f) ins, nl,
          M.filterWithKey (curry f) outs)) .
   graphMap

{-
delEdgeHelp ::
   (Edge e, Ord n) =>
   Graph n e nl el -> (M.Map (e n) el, [e n]) -> Graph n e nl el
delEdgeHelp (Graph ns _els) (kept, deleted) =
   Graph
      (fmap
         (\(ins, n, outs) delIns delOuts ->
            (S.difference ins delIns, n, S.difference outs delOuts)) ns
         $$ makeInMap  ns deleted
         $$ makeOutMap ns deleted)
      kept


propELFilter :: [LEdge DirEdge Char Int] -> Bool
propELFilter =
   uncurry (==) . compareELFilter . M.fromList

compareELFilter ::
   M.Map (DirEdge Char) Int ->
   (Graph Char DirEdge String Int, Graph Char DirEdge String Int)
compareELFilter es =
   let ns =
          MapU.fromSet (\n -> [n, toUpper n]) $
          foldMap (foldMap S.singleton) $ M.keys es
   in  (elfilter even $ fromMap ns es,
        fromMap ns $ M.filter even es)
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
   M.insertWith
      (\_ (ins, _, outs) -> (ins, nl, outs))
      n (M.empty, nl, M.empty) .
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
insEdges es = insEdgeSet (M.fromList es)

{- |
In the current implementation
existing edges are replaced with new labels.
However, I think we should better have an extra function for this purpose
and you should not rely on this behavior.
-}
insEdgeSet ::
   (Edge e, Ord (e n), Ord n) =>
   M.Map (e n) el -> Graph n e nl el -> Graph n e nl el
insEdgeSet es =
   let ess = M.mapWithKey M.singleton es
   in  Graph .
       applyMap (fmap (\new -> mapFst3 (M.union new)) $ M.mapKeysWith M.union to   ess) .
       applyMap (fmap (\new -> mapThd3 (M.union new)) $ M.mapKeysWith M.union from ess) .
       graphMap

fromList ::
   (Edge e, Ord (e n), Ord n) =>
   [LNode n nl] -> [LEdge e n el] -> Graph n e nl el
fromList ns es =
   fromMap (M.fromList ns) $ M.fromList es

fromMap ::
   (Edge e, Ord (e n), Ord n) =>
   M.Map n nl -> M.Map (e n) el -> Graph n e nl el
fromMap ns es =
   let ess = M.mapWithKey M.singleton es
   in  Graph $
       TMap.intersectionPartialWith (\ins (outs, nl) -> (ins,nl,outs))
          (TMap.cons M.empty $ M.mapKeysWith M.union to   ess) $
       TMap.intersectionPartialWith (,)
          (TMap.cons M.empty $ M.mapKeysWith M.union from ess) ns


{-
infixl 0 $$

($$) :: (Ord n) => M.Map n (a -> b) -> M.Map n a -> M.Map n b
($$) = M.intersectionWith ($)

makeMap :: (Ord n) =>
   (e n -> n, e n -> n) ->
   M.Map n nl -> [e n] -> M.Map n (S.Set n)
makeMap (selFrom, selTo) ns =
   flip M.union (fmap (const S.empty) ns) .
   M.fromListWith S.union .
   map (\e -> (selFrom e, S.singleton $ selTo e))

makeOutMap, makeInMap ::
   (Edge e, Ord n) => M.Map n nl -> [e n] -> M.Map n (S.Set n)
makeOutMap = makeMap (from, to)
makeInMap  = makeMap (to, from)
-}

nmap :: (nl0 -> nl1) -> Graph n e nl0 el -> Graph n e nl1 el
nmap f =
   Graph . fmap (\(ins,n,outs) -> (ins, f n, outs)) . graphMap

emap :: (el0 -> el1) -> Graph n e nl el0 -> Graph n e nl el1
emap f =
   Graph . fmap (\(ins,n,outs) -> (fmap f ins, n, fmap f outs)) . graphMap

nodeSet :: Graph n e nl el -> S.Set n
nodeSet = M.keysSet . graphMap


type InOut n e nl el = ([LEdge e n el], LNode n nl, [LEdge e n el])

nmapWithInOut ::
   (Edge e, Ord (e n), Ord n) =>
   (InOut n e nl0 el -> nl1) -> Graph n e nl0 el -> Graph n e nl1 el
nmapWithInOut f =
   Graph .
   M.mapWithKey
      (\n (ins,nl,outs) ->
         (ins, f (M.toList ins, (n,nl), M.toList outs), outs)) .
   graphMap

{-
inOut ::
   (ConsEdge e, Ord (e n), Ord n) =>
   Graph n e nl el ->
   n -> (S.Set n, t, S.Set n) ->
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
   M.toList ns


mapGraph ::
   (ConsEdge e, Ord (e n), Ord n) =>
   (InOut n nl el -> a) -> Graph n e nl el -> [a]
mapGraph f g = map f (mkInOutGraphFormat g)
-}
