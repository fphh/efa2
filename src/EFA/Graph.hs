module EFA.Graph (
   Graph(Graph),
   isConsistent,
   inEdges, outEdges,
   nodeLabels,
   edgeLabels,

   LNode,
   Edge(Edge),
   LEdge,

   reverseEdge,
   ixmap, nmap, emap,
   empty,
   union,
   getIncoming,
   getOutgoing,
   Adj,
   mkOutAdj,
   mkInAdj,
   getLEdge,
   isEmpty,
   lab,
   labNodes,
   labEdges,
   adjEdges,
   pre, lpre, preEdgeLabels,
   suc, lsuc, sucEdgeLabels,
   delNode,
   delNodes,
   delNodeSet,
   delEdges,
   delEdgeSet,
   lefilter,
   elfilter,
   propELFilter,
   insNode, insNodes,
   insEdge, insEdges, insEdgeSet,
   mkGraph, fromList, fromMap,
   nodes, nodeSet,
   InOut,
   mkInOutGraphFormat,
   mapGraph,
   ) where

-- imaport qualified Data.Graph.Inductive as IG
import EFA.Utility (mapFromSet, differenceMapSet, intersectionMapSet)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Foldable as Fold
import Control.Monad (liftM2)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Foldable (Foldable, foldMap, fold)
import Data.Tuple.HT (mapSnd, fst3, snd3, thd3)
import Data.Char (toUpper)

import qualified Test.QuickCheck as QC


{-
For all Graph's the 'isConsistent' predicate must be 'True'.
-}
data Graph node nodeLabel edgeLabel =
   Graph {
      nodes :: M.Map node (S.Set node, nodeLabel, S.Set node),
      edgeLabels :: M.Map (Edge node) edgeLabel
   } deriving (Eq)

instance (Show n, Show nl, Show el) => Show (Graph n nl el) where
   showsPrec prec g =
      showParen (prec>10) $
         showString "Graph.fromList " .
         shows (M.toList $ nodeLabels g) .
         showString " " .
         shows (M.toList $ edgeLabels g)


isConsistent :: Ord n => Graph n nl el -> Bool
isConsistent (Graph ns els) =
   case M.keysSet els of
      es ->
         Fold.all (Fold.all (flip M.member ns)) es
         &&
         es == fold (M.mapWithKey (\n (_, _, outs) -> S.map (Edge n) outs) ns)
         &&
         es == fold (M.mapWithKey (\n (ins, _, _) -> S.map (flip Edge n) ins) ns)


type LNode n label = (n, label)

data Edge node = Edge node node
   deriving (Show, Eq, Ord)

instance Functor Edge where
   fmap f (Edge x y) = Edge (f x) (f y)

instance Foldable Edge where
   foldMap f (Edge x y) = mappend (f x) (f y)

instance (QC.Arbitrary n) => QC.Arbitrary (Edge n) where
   arbitrary = liftM2 Edge QC.arbitrary QC.arbitrary
   shrink (Edge x y) = map (uncurry Edge) $ QC.shrink (x,y)

reverseEdge :: Edge node -> Edge node
reverseEdge (Edge x y) = Edge y x

--edgeFst, edgeSnd :: Edge node -> node
--edgeFst (Edge x _) = x
--edgeSnd (Edge _ y) = y

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
ixmap f (Graph nls els) =
   Graph
      (fmap (\(ins,nl,outs) -> (S.map f ins, nl, S.map f outs)) $
       M.mapKeysWith (error "Graph.ixmap: node map is not injective") f nls)
      (M.mapKeys (fmap f) els)

empty :: Graph node nodeLabel edgeLabel
empty = Graph M.empty M.empty

{- |
The node sets must be disjoint.
-}
union ::
   (Ord node) =>
   Graph node nodeLabel edgeLabel ->
   Graph node nodeLabel edgeLabel ->
   Graph node nodeLabel edgeLabel
union (Graph ns0 els0) (Graph ns1 els1) =
   Graph
      (M.unionWith (error "Graph.union: node sets overlap") ns0 ns1)
      (M.union els0 els1)

instance (Ord node) => Monoid (Graph node nodeLabel edgeLabel) where
   mempty = empty
   mappend = union


inEdges, outEdges :: Graph n nl el -> M.Map n (S.Set n)
inEdges  = fmap fst3 . nodes
outEdges = fmap thd3 . nodes

nodeLabels :: Graph n nl el -> M.Map n nl
nodeLabels = fmap snd3 . nodes


getIncoming :: (Ord n) => Graph n nl el -> n -> [n]
getIncoming g n =
   foldMap S.toList $ M.lookup n $ inEdges g

getOutgoing :: (Ord n) => Graph n nl el -> n -> [n]
getOutgoing g n =
   foldMap S.toList $ M.lookup n $ outEdges g

type Adj n el = [(el, n)]

mkOutAdj :: (Ord n) => Graph n nl el -> n -> Adj n el
mkOutAdj g n = map f es
  where es = zipWith Edge (repeat n) (getOutgoing g n)
        f e@(Edge _ x) = ((edgeLabels g) M.! e, x)

mkInAdj :: (Ord n) => Graph n nl el -> n -> Adj n el
mkInAdj g n = map f es
  where es = zipWith Edge (getIncoming g n) (repeat n)
        f e@(Edge x _) = ((edgeLabels g) M.! e, x)

getLEdge :: (Ord n) => Graph n nl el -> n -> n -> Maybe (LEdge n el)
getLEdge g from to =
   let e = Edge from to
   in  fmap ((,) e) $ M.lookup e (edgeLabels g)

{-
instance (Ord n, Enum n) => IG.Graph (Graph n) where
   empty = Graph M.empty M.empty M.empty M.empty
   isEmpty = isEmpty
   match ni g =
      if isEmpty g
        then (Nothing, g)
        else (Just cont, delNode g n)
     where n = toEnum ni
           cont =
              (map (mapSnd fromEnum) $ mkInAdj g n,
               ni,
               nodeLabels g  M.! n,
               map (mapSnd fromEnum) $ mkOutAdj g n)
   labNodes = map (\(n,l) -> (fromEnum n, l)) . labNodes
   labEdges = map (\(n,m,l) -> (fromEnum n, fromEnum m, l)) . labEdges
   mkGraph ns es =
      mkGraph
         (map (\(n,l) -> (toEnum n, l)) ns)
         (map (\(n,m,l) -> (toEnum n, toEnum m, l)) es)
-}

isEmpty :: Graph  n nl el -> Bool
isEmpty = M.null . nodes

lab :: Ord n => Graph n nl el -> n -> Maybe nl
lab g n = fmap snd3 $ M.lookup n (nodes g)

labNodes :: Graph n nl el -> [LNode n nl]
labNodes = M.toList . nodeLabels

labEdges :: Graph n nl el -> [LEdge n el]
labEdges = M.toList . edgeLabels

pre, suc :: (Ord n) => Graph n nl el -> n -> [n]
pre g n = S.toList . M.findWithDefault (error "pre: unknown node") n . inEdges $ g
suc g n = S.toList . M.findWithDefault (error "suc: unknown node") n . outEdges $ g

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


adjEdges ::
   Ord n => Graph n nl el -> n -> S.Set (Edge n)
adjEdges g n =
   nodeEdges n $
   M.findWithDefault (error "delNode: unknown node") n $
   nodes g

delNode :: (Ord n) => Graph n nl el -> n -> Graph n nl el
delNode g@(Graph nls els) n =
   Graph
      (fmap (\(ins, n0, outs) -> (S.delete n ins, n0, S.delete n outs)) $
       M.delete n nls) $
   differenceMapSet els $ adjEdges g n

delNodeSet ::
   Ord n => S.Set n -> Graph n nl el -> Graph n nl el
delNodeSet delNs (Graph ns els) =
   Graph
      (fmap
         (\(ins, n, outs) ->
            (S.difference ins delNs, n, S.difference outs delNs)) $
         differenceMapSet ns delNs) $
   differenceMapSet els $
   fold $ M.mapWithKey nodeEdges $
   intersectionMapSet ns delNs

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

lefilter ::
   (Ord n) =>
   (LEdge n el -> Bool) ->
   Graph n nl el -> Graph n nl el
lefilter f g =
   delEdgeHelp g $ mapSnd M.keys $ M.partitionWithKey (curry f) $ edgeLabels g

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

insNode ::
   (Ord n) => LNode n nl -> Graph n nl el -> Graph n nl el
insNode (n,nl) g =
   g{nodes =
        M.insertWith
           (\_ (ins, _, outs) -> (ins, nl, outs))
           n (S.empty, nl, S.empty)
           (nodes g)}

insEdge ::
   (Ord n) => LEdge n el -> Graph n nl el -> Graph n nl el
insEdge es = insEdges [es]


insNodes ::
   (Ord n) => [LNode n nl] -> Graph n nl el -> Graph n nl el
insNodes = flip (foldl (flip insNode))

insEdges ::
   (Ord n) => [LEdge n el] -> Graph n nl el -> Graph n nl el
insEdges es = insEdgeSet (M.fromList es)

insEdgeSet ::
   (Ord n) => M.Map (Edge n) el -> Graph n nl el -> Graph n nl el
insEdgeSet es (Graph ns els) =
   Graph
      (fmap
          (\(ins, n, outs) newIns newOuts ->
             (S.union ins newIns, n, S.union outs newOuts)) ns
       $$ (makeInMap  ns $ M.keys es)
       $$ (makeOutMap ns $ M.keys es))
      (M.unionWith (error "insEdgeSet: edge already contained in graph")
         els es)


-- I may deprecate mkGraph in favor of Graph.fromList
fromList, mkGraph ::
   (Ord n) =>
   [LNode n nl] -> [LEdge n el] -> Graph n nl el
mkGraph = fromList
fromList ns es =
   fromMap (M.fromList ns) $ M.fromList es

fromMap ::
   (Ord n) =>
   M.Map n nl -> M.Map (Edge n) el -> Graph n nl el
fromMap ns es =
   case M.keys es of
      esl ->
         Graph (fmap (,,) (makeInMap ns esl) $$ ns $$ (makeOutMap ns esl)) es


infixl 0 $$

($$) :: (Ord n) => M.Map n (a -> b) -> M.Map n a -> M.Map n b
($$) = M.intersectionWith ($)

makeOutMap, makeInMap :: (Ord n) => M.Map n nl -> [Edge n] -> M.Map n (S.Set n)
makeOutMap ns =
   flip M.union (fmap (const S.empty) ns) .
   M.fromListWith S.union .
   map (\(Edge x y) -> (x, S.singleton y))

makeInMap ns = makeOutMap ns . map reverseEdge

{-
instance (Ord n, Enum n) => IG.DynGraph (Graph n) where
         (ins, ni, lab_, outs) & (Graph os is ls es) =
           {- trace (show os ++ "\n" ++ show resOs ++ "\n" ++ show n++ "\n----\n") -} Graph resOs resIs newLs newEs
           where n = toEnum ni
                 insEnum = map (mapSnd toEnum) ins
                 outsEnum = map (mapSnd toEnum) outs
                 newLs = M.insert n lab_ ls
                 newIns = map (\(l, i) -> (Edge i n, l)) insEnum
                 newOuts = map (\(l, o) -> (Edge n o, l)) outsEnum
                 newEs = foldr (uncurry M.insert) es (newIns ++ newOuts)

                 ins' = map snd insEnum
                 -- is' = M.insert n (S.fromList ins') is
                 is' = M.insertWith S.union n (S.fromList ins') is

                 outs' = map snd outsEnum
                 --os' = M.insert n (S.fromList outs') os
                 os' = M.insertWith S.union n (S.fromList outs') os

                 resIs = L.foldl' f is' outs'
                 resOs = L.foldl' f os' ins'

                 f acc x = M.insertWith S.union x (S.singleton n) acc
-}

nmap :: (nl0 -> nl1) -> Graph n nl0 el -> Graph n nl1 el
nmap f (Graph ns els) =
   Graph (fmap (\(ins,n,outs) -> (ins, f n, outs)) ns) els

emap :: (el0 -> el1) -> Graph n nl el0 -> Graph n nl el1
emap f (Graph ns els) =
   Graph ns (fmap f els)

nodeSet :: Graph n nl el -> S.Set n
nodeSet = M.keysSet . nodes


type InOut n nl el = ([LNode n el], LNode n nl, [LNode n el])

mkInOutGraphFormat ::
   (Ord n) => Graph n nl el -> [InOut n nl el]
mkInOutGraphFormat g@(Graph ns _els) =
   map
      (\(n, (ins,nl,outs)) ->
         (preEdgeLabels g n ins,
          (n,nl),
          sucEdgeLabels g n outs)) $
   M.toList ns

filterLEdges ::
   (Ord n, Ord e) =>
   M.Map e el -> (n -> e) -> S.Set n -> [(n, el)]
filterLEdges els edge =
   M.elems . M.intersectionWith (flip (,)) els .
   M.mapKeys edge . mapFromSet id


mapGraph ::
   (Ord n) =>
   (InOut n nl el -> a) -> Graph n nl el -> [a]
mapGraph f g = map f (mkInOutGraphFormat g)
