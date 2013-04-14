module EFA.Graph (
   Graph,
   isConsistent,
   -- inEdges, outEdges,
   graphMap,
   nodeLabels,
   edgeLabels,

   LNode,
   Edge(Edge),
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

-- import EFA.Utility (mapFromSet, differenceMapSet, intersectionMapSet)

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
newtype Graph node nodeLabel edgeLabel =
   Graph {
      graphMap ::
         M.Map node
            (M.Map (Edge node) edgeLabel,
             nodeLabel,
             M.Map (Edge node) edgeLabel)
   } deriving (Eq)

instance (Show n, Show nl, Show el, Ord n) => Show (Graph n nl el) where
   showsPrec prec g =
      showParen (prec>10) $
         showString "Graph.fromList " .
         shows (M.toList $ nodeLabels g) .
         showString " " .
         shows (M.toList $ edgeLabels g)


isConsistent :: (Ord n, Eq el) => Graph n nl el -> Bool
isConsistent (Graph ns) =
   foldMap fst3 ns == foldMap thd3 ns
   &&
   (Fold.and $ flip M.mapWithKey ns $
      \n (ins,_nl,outs) ->
         Fold.all ((n==) . to) (M.keysSet ins) &&
         Fold.all ((n==) . from) (M.keysSet outs))


type LNode n label = (n, label)

data Edge node = Edge {from, to :: node}
   deriving (Show, Eq, Ord)

instance Functor Edge where
   fmap f (Edge x y) = Edge (f x) (f y)

instance Foldable Edge where
   foldMap f (Edge x y) = mappend (f x) (f y)

instance (QC.Arbitrary n) => QC.Arbitrary (Edge n) where
   arbitrary = liftM2 Edge QC.arbitrary QC.arbitrary
   shrink (Edge x y) = map (uncurry Edge) $ QC.shrink (x,y)


nodes ::
   (Ord node) =>
   Graph node nodeLabel edgeLabel ->
   M.Map node (S.Set node, nodeLabel, S.Set node)
nodes =
   fmap (\(ins,n,outs) -> (S.map from ins, n, S.map to outs)) .
   nodeEdges

nodeEdges ::
   (Ord node) =>
   Graph node nodeLabel edgeLabel ->
   M.Map node (S.Set (Edge node), nodeLabel, S.Set (Edge node))
nodeEdges =
   fmap (\(ins,n,outs) -> (M.keysSet ins, n, M.keysSet outs)) .
   graphMap


edgeLabels ::
   (Ord node) =>
   Graph node nodeLabel edgeLabel ->
   M.Map (Edge node) edgeLabel
edgeLabels =
   foldMap fst3 . graphMap


reverse :: (Ord n) => Graph n nl el -> Graph n nl el
reverse =
   Graph .
   fmap
      (\(ins, nl, outs) ->
         (M.mapKeys reverseEdge outs, nl, M.mapKeys reverseEdge ins)) .
   graphMap

reverseEdge :: Edge node -> Edge node
reverseEdge (Edge x y) = Edge y x


type LEdge n label = (Edge n, label)


{- |
The index map must be an injection,
that is, nodes must not collaps.
-}
ixmap ::
   (Ord node0, Ord node1) =>
   (node0 -> node1) ->
   Graph node0 nodeLabel edgeLabel ->
   Graph node1 nodeLabel edgeLabel
ixmap f =
   Graph .
   fmap
      (\(ins,nl,outs) ->
         (M.mapKeys (fmap f) ins, nl, M.mapKeys (fmap f) outs)) .
   M.mapKeysWith (error "Graph.ixmap: node map is not injective") f .
   graphMap

empty :: Graph node nodeLabel edgeLabel
empty = Graph M.empty

{- |
The node sets must be disjoint.
-}
union ::
   (Ord node) =>
   Graph node nodeLabel edgeLabel ->
   Graph node nodeLabel edgeLabel ->
   Graph node nodeLabel edgeLabel
union (Graph ns0) (Graph ns1) =
   Graph
      (M.unionWith (error "Graph.union: node sets overlap") ns0 ns1)

instance (Ord node) => Monoid (Graph node nodeLabel edgeLabel) where
   mempty = empty
   mappend = union


{-
inEdges, outEdges :: Graph n nl el -> M.Map n (S.Set n)
inEdges  = fmap fst3 . nodes
outEdges = fmap thd3 . nodes
-}

nodeLabels :: (Ord n) => Graph n nl el -> M.Map n nl
nodeLabels = fmap snd3 . nodes

{-
getLEdge :: (Ord n) => Graph n nl el -> n -> n -> Maybe (LEdge n el)
getLEdge g x y =
   let e = Edge x y
   in  fmap ((,) e) $ M.lookup e (edgeLabels g)
-}

isEmpty :: Graph  n nl el -> Bool
isEmpty = M.null . graphMap

{-
lab :: Ord n => Graph n nl el -> n -> Maybe nl
lab g n = fmap snd3 $ M.lookup n (nodes g)
-}

labNodes :: (Ord n) => Graph n nl el -> [LNode n nl]
labNodes = M.toList . nodeLabels

labEdges :: (Ord n) => Graph n nl el -> [LEdge n el]
labEdges = M.toList . edgeLabels

pre, suc :: (Ord n) => Graph n nl el -> n -> [n]
pre g n =
   S.toList . S.map from . M.keysSet . fst3 .
   M.findWithDefault (error "pre: unknown node") n . graphMap $ g
suc g n =
   S.toList . S.map to . M.keysSet . thd3 .
   M.findWithDefault (error "suc: unknown node") n . graphMap $ g

{-
lpre, lsuc :: (Ord n) => Graph n nl el -> n -> [LNode n el]
lpre g@(Graph ns _els) n =
   preEdgeLabels g n $
   fst3 $ M.findWithDefault (error "lpre: unknown node") n ns

lsuc g@(Graph ns _els) n =
   sucEdgeLabels g n $
   thd3 $ M.findWithDefault (error "lsuc: unknown node") n ns

preEdgeLabels, sucEdgeLabels ::
   (Ord n) => Graph n nl el -> n -> S.Set n -> [LNode n el]
preEdgeLabels g n = filterLEdges (edgeLabels g) (flip Edge n)
sucEdgeLabels g n = filterLEdges (edgeLabels g) (Edge n)

filterLEdges ::
   (Ord n, Ord e) =>
   M.Map e el -> (n -> e) -> S.Set n -> [(n, el)]
filterLEdges els edge =
   M.elems . M.intersectionWith (flip (,)) els .
   M.mapKeys edge . mapFromSet id
-}


adjEdges ::
   Ord n => Graph n nl el -> n -> S.Set (Edge n)
adjEdges g n =
   (\(ins,_nl,outs) -> M.keysSet ins `S.union` M.keysSet outs) $
   M.findWithDefault (error "adjEdges: unknown node") n $
   graphMap g

{-
In constrast to Map.intersectWith ($) unaffected are preserved.
-}
applyMap :: (Ord k) => M.Map k (a -> a) -> M.Map k a -> M.Map k a
applyMap f x =
   M.union (M.intersectionWith ($) f x) x

{- |
Node to be deleted must be contained in the graph.
-}
delNode :: (Ord n) => n -> Graph n nl el -> Graph n nl el
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
   Ord n => S.Set n -> Graph n nl el -> Graph n nl el
delNodeSet delNs g = S.foldl (flip delNode) g delNs

{-
nodeEdges ::
   Ord n => n -> (S.Set n, a, S.Set n) -> S.Set (Edge n)
nodeEdges n (ins, _, outs) =
   S.union
      (S.map (flip Edge n) ins)
      (S.map (Edge n) outs)

delNodes ::
   Ord n => [n] -> Graph n nl el -> Graph n nl el
delNodes nsl = delNodeSet $ S.fromList nsl

delEdgeSet ::
   (Ord n) =>
   S.Set (Edge n) -> Graph n nl el -> Graph n nl el
delEdgeSet es g =
   delEdgeHelp g
      (differenceMapSet (edgeLabels g) es,
       S.toList es)

delEdges ::
   (Ord n) =>
   [Edge n] -> Graph n nl el -> Graph n nl el
delEdges es g =
   delEdgeHelp g
      (differenceMapSet (edgeLabels g) $ S.fromList es, es)

elfilter ::
   (Ord n) =>
   (el -> Bool) ->
   Graph n nl el -> Graph n nl el
elfilter f g =
   delEdgeHelp g $ mapSnd M.keys $ M.partition f $ edgeLabels g
-}

lefilter ::
   (Ord n) =>
   (LEdge n el -> Bool) ->
   Graph n nl el -> Graph n nl el
lefilter f =
   Graph .
   fmap
      (\(ins, nl, outs) ->
         (M.filterWithKey (curry f) ins, nl,
          M.filterWithKey (curry f) outs)) .
   graphMap

{-
delEdgeHelp ::
   (Ord n) =>
   Graph n nl el -> (M.Map (Edge n) el, [Edge n]) -> Graph n nl el
delEdgeHelp (Graph ns _els) (kept, deleted) =
   Graph
      (fmap
         (\(ins, n, outs) delIns delOuts ->
            (S.difference ins delIns, n, S.difference outs delOuts)) ns
         $$ makeInMap  ns deleted
         $$ makeOutMap ns deleted)
      kept


propELFilter :: [LEdge Char Int] -> Bool
propELFilter =
   uncurry (==) . compareELFilter . M.fromList

compareELFilter ::
   M.Map (Edge Char) Int ->
   (Graph Char String Int, Graph Char String Int)
compareELFilter es =
   let ns =
          mapFromSet (\n -> [n, toUpper n]) $
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
   (Ord n) => LNode n nl -> Graph n nl el -> Graph n nl el
insNode (n,nl) =
   Graph .
   M.insertWith
      (\_ (ins, _, outs) -> (ins, nl, outs))
      n (M.empty, nl, M.empty) .
   graphMap

insNodes ::
   (Ord n) => [LNode n nl] -> Graph n nl el -> Graph n nl el
insNodes = flip (foldl (flip insNode))

insEdge ::
   (Ord n) => LEdge n el -> Graph n nl el -> Graph n nl el
insEdge es = insEdges [es]

insEdges ::
   (Ord n) => [LEdge n el] -> Graph n nl el -> Graph n nl el
insEdges es = insEdgeSet (M.fromList es)

{- |
In the current implementation
existing edges are replaced with new labels.
However, I think we should better have an extra function for this purpose
and you should not rely on this behavior.
-}
insEdgeSet ::
   (Ord n) => M.Map (Edge n) el -> Graph n nl el -> Graph n nl el
insEdgeSet es =
   let ess = M.mapWithKey M.singleton es
   in  Graph .
       applyMap (fmap (\new -> mapFst3 (M.union new)) $ M.mapKeysWith M.union to   ess) .
       applyMap (fmap (\new -> mapThd3 (M.union new)) $ M.mapKeysWith M.union from ess) .
       graphMap

fromList ::
   (Ord n) =>
   [LNode n nl] -> [LEdge n el] -> Graph n nl el
fromList ns es =
   fromMap (M.fromList ns) $ M.fromList es

fromMap ::
   (Ord n) =>
   M.Map n nl -> M.Map (Edge n) el -> Graph n nl el
fromMap ns es =
   let fill = flip M.union (fmap (const M.empty) ns)
       ess = M.mapWithKey M.singleton es
   in  Graph $
       M.intersectionWith (\nl (ins,outs) -> (ins,nl,outs)) ns $
       M.intersectionWith (,)
          (fill $ M.mapKeysWith M.union to   ess)
          (fill $ M.mapKeysWith M.union from ess)


{-
infixl 0 $$

($$) :: (Ord n) => M.Map n (a -> b) -> M.Map n a -> M.Map n b
($$) = M.intersectionWith ($)

makeOutMap, makeInMap :: (Ord n) => M.Map n nl -> [Edge n] -> M.Map n (S.Set n)
makeOutMap ns =
   flip M.union (fmap (const S.empty) ns) .
   M.fromListWith S.union .
   map (\(Edge x y) -> (x, S.singleton y))

makeInMap ns = makeOutMap ns . map reverseEdge
-}

nmap :: (nl0 -> nl1) -> Graph n nl0 el -> Graph n nl1 el
nmap f =
   Graph . fmap (\(ins,n,outs) -> (ins, f n, outs)) . graphMap

emap :: (el0 -> el1) -> Graph n nl el0 -> Graph n nl el1
emap f =
   Graph . fmap (\(ins,n,outs) -> (fmap f ins, n, fmap f outs)) . graphMap

nodeSet :: Graph n nl el -> S.Set n
nodeSet = M.keysSet . graphMap


type InOut n nl el = ([LNode n el], LNode n nl, [LNode n el])

nmapWithInOut ::
   (Ord n) =>
   (InOut n nl0 el -> nl1) -> Graph n nl0 el -> Graph n nl1 el
nmapWithInOut f =
   Graph .
   M.mapWithKey
      (\n (ins,nl,outs) ->
         (ins,
          f (M.toList $ M.mapKeys to ins, (n,nl), M.toList $ M.mapKeys to outs),
          outs)) .
   graphMap
