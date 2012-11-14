module EFA2.Topology.EfaGraph where
--          (EfaGraph, mkGraphFromMap, Edge(..), labEdges_) where

-- import qualified Data.Graph.Inductive as IG
import EFA2.Utils.Utils (mapFromSet)

import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad (liftM2)
import Data.Foldable (foldMap, fold)
import Data.Tuple.HT (mapSnd)
import Data.Char (toUpper)

import qualified Test.QuickCheck as QC


data EfaGraph node nodeLabel edgeLabel =
   EfaGraph {
      outEdges, inEdges :: M.Map node (S.Set node),
      nodeLabels :: M.Map node nodeLabel,
      edgeLabels :: M.Map (Edge node) edgeLabel
   } deriving (Show, Eq)

type LNode n label = (n, label)

data Edge node = Edge node node
   deriving (Show, Eq, Ord)

instance Functor Edge where
   fmap f (Edge x y) = Edge (f x) (f y)

instance (QC.Arbitrary n) => QC.Arbitrary (Edge n) where
   arbitrary = liftM2 Edge QC.arbitrary QC.arbitrary
   shrink (Edge x y) = map (uncurry Edge) $ QC.shrink (x,y)

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
   EfaGraph node0 nodeLabel edgeLabel ->
   EfaGraph node1 nodeLabel edgeLabel
ixmap f (EfaGraph outs ins nls els) =
   EfaGraph
      (fmap (S.map f) $ M.mapKeys f outs)
      (fmap (S.map f) $ M.mapKeys f ins)
      (M.mapKeysWith (error "EfaGraph.ixmap: node map is not injective") f nls)
      (M.mapKeys (fmap f) els)

empty :: EfaGraph node nodeLabel edgeLabel
empty = EfaGraph M.empty M.empty M.empty M.empty

{- |
The node sets must be disjoint.
-}
union ::
   (Ord node) =>
   EfaGraph node nodeLabel edgeLabel ->
   EfaGraph node nodeLabel edgeLabel ->
   EfaGraph node nodeLabel edgeLabel
union (EfaGraph outs0 ins0 nls0 els0) (EfaGraph outs1 ins1 nls1 els1) =
   EfaGraph
      (M.union outs0 outs1)
      (M.union ins0 ins1)
      (M.unionWith (error "EfaGraph.union: node sets overlap") nls0 nls1)
      (M.union els0 els1)


getIncoming :: (Ord n) => EfaGraph n nl el -> n -> [n]
getIncoming g n =
   foldMap S.toList $ M.lookup n $ inEdges g

getOutgoing :: (Ord n) => EfaGraph n nl el -> n -> [n]
getOutgoing g n =
   foldMap S.toList $ M.lookup n $ outEdges g

type Adj n el = [(el, n)]

mkOutAdj :: (Ord n) => EfaGraph n nl el -> n -> Adj n el
mkOutAdj g n = map f es
  where es = zipWith Edge (repeat n) (getOutgoing g n)
        f e@(Edge _ x) = ((edgeLabels g) M.! e, x)

mkInAdj :: (Ord n) => EfaGraph n nl el -> n -> Adj n el
mkInAdj g n = map f es
  where es = zipWith Edge (getIncoming g n) (repeat n)
        f e@(Edge x _) = ((edgeLabels g) M.! e, x)

getLEdge :: (Ord n) => EfaGraph n nl el -> n -> n -> Maybe (LEdge n el)
getLEdge g from to =
   let e = Edge from to
   in  fmap ((,) e) $ M.lookup e (edgeLabels g)

delEfaNode :: (Ord n) => EfaGraph n nl el -> n -> EfaGraph n nl el
delEfaNode g@(EfaGraph outs ins nls els) n =
   let remove = M.map (S.delete n) . M.delete n
   in  EfaGraph (remove outs) (remove ins) (M.delete n nls) $
          M.difference els $ mapFromSet (const ()) $ S.fromList $
             zipWith Edge (getIncoming g n) (repeat n) ++
             zipWith Edge (repeat n) (getOutgoing g n)

{-
instance (Ord n, Enum n) => IG.Graph (EfaGraph n) where
   empty = EfaGraph M.empty M.empty M.empty M.empty
   isEmpty = isEmpty
   match ni g =
      if isEmpty g
        then (Nothing, g)
        else (Just cont, delEfaNode g n)
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

isEmpty :: EfaGraph  n nl el -> Bool
isEmpty = M.null . nodeLabels

lab :: Ord n => EfaGraph n nl el -> n -> Maybe nl
lab g n = M.lookup n (nodeLabels g)

labNodes :: EfaGraph n nl el -> [LNode n nl]
labNodes = M.toList . nodeLabels

labEdges :: EfaGraph n nl el -> [LEdge n el]
labEdges = M.toList . edgeLabels

pre, suc :: (Ord n) => EfaGraph n nl el -> n -> [n]
pre g n = S.toList . M.findWithDefault (error "pre: unknown node") n . inEdges $ g
suc g n = S.toList . M.findWithDefault (error "suc: unknown node") n . outEdges $ g

lpre, lsuc :: (Ord n) => EfaGraph n nl el -> n -> [(n, el)]
lpre g n =
   map (\(Edge x _, l) -> (x,l)) $ M.toList $
   M.intersection (edgeLabels g) $ mapFromSet (const ()) $
   S.map (flip Edge n) $
   M.findWithDefault (error "lpre: unknown node") n $
   inEdges g

lsuc g n =
   map (\(Edge _ x, l) -> (x,l)) $ M.toList $
   M.intersection (edgeLabels g) $ mapFromSet (const ()) $
   S.map (Edge n) $
   M.findWithDefault (error "lsuc: unknown node") n $
   outEdges g


delNodes ::
   Ord n => [n] -> EfaGraph n nl el -> EfaGraph n nl el
delNodes nsl (EfaGraph outs ins nls els) =
   let ns = S.fromList nsl
       nm = mapFromSet (const ()) ns
       remove = M.map (flip S.difference ns) . flip M.difference nm
   in  EfaGraph (remove outs) (remove ins) (M.difference nls nm) $
       M.difference els $ mapFromSet (const ()) $
       S.union
          (fold $
           M.intersectionWithKey (\n inp _ -> S.map (flip Edge n) inp) ins nm)
          (fold $
           M.intersectionWithKey (\n out _ -> S.map (Edge n) out) outs nm)

delEdgeSet ::
   (Ord n) =>
   S.Set (Edge n) -> EfaGraph n nl el -> EfaGraph n nl el
delEdgeSet es g =
   delEdgeHelp g
      (M.difference (edgeLabels g) $
       mapFromSet (const ()) es,
       es)

delEdges ::
   (Ord n) =>
   [Edge n] -> EfaGraph n nl el -> EfaGraph n nl el
delEdges es g =
   case S.fromList es of
      ess ->
         delEdgeHelp g
            (M.difference (edgeLabels g) $
             mapFromSet (const ()) ess,
             ess)

elfilter ::
   (Ord n) =>
   (el -> Bool) ->
   EfaGraph n nl el -> EfaGraph n nl el
elfilter f g =
   delEdgeHelp g $ mapSnd M.keysSet $ M.partition f $ edgeLabels g

delEdgeHelp ::
   (Ord n) =>
   EfaGraph n nl el -> (M.Map (Edge n) el, S.Set (Edge n)) -> EfaGraph n nl el
delEdgeHelp (EfaGraph outs ins nls _els) (kept, deleted) =
   EfaGraph
      (S.foldl (\mp (Edge x y) -> M.adjust (S.delete y) x mp) outs deleted)
      (S.foldl (\mp (Edge x y) -> M.adjust (S.delete x) y mp) ins deleted)
      nls kept


propELFilter :: [LEdge Char Int] -> Bool
propELFilter =
   uncurry (==) . compareELFilter

compareELFilter ::
   [LEdge Char Int] ->
   (EfaGraph Char String Int, EfaGraph Char String Int)
compareELFilter esWithDuplicates =
   let es = M.toList $ M.fromList esWithDuplicates
       ns =
          map (\n -> (n, [n, toUpper n])) $ S.toList $ S.unions $
          map (\(Edge x y, _) -> S.fromList [x,y]) es
   in  (elfilter even $ mkGraph ns es,
        mkGraph ns $ filter (even . snd) es)

insNode ::
   (Ord n) => LNode n nl -> EfaGraph n nl el -> EfaGraph n nl el
insNode n = insNodes [n]

insEdge ::
   (Ord n) => LEdge n el -> EfaGraph n nl el -> EfaGraph n nl el
insEdge es = insEdges [es]


insNodes ::
   (Ord n) => [LNode n nl] -> EfaGraph n nl el -> EfaGraph n nl el
insNodes ns g =
   g{outEdges =
        M.union (outEdges g) $
        M.fromList $ map (mapSnd (const S.empty)) ns,
     inEdges =
        M.union (inEdges g) $
        M.fromList $ map (mapSnd (const S.empty)) ns,
     nodeLabels =
        M.union (M.fromList ns) (nodeLabels g)}

insEdges ::
   (Ord n) => [LEdge n el] -> EfaGraph n nl el -> EfaGraph n nl el
insEdges es g =
   let mapFromList f newEs oldEs =
          M.unionWith S.union oldEs $
          M.fromListWith S.union $
          map (\e -> case f $ fst e of Edge x y -> (x, S.singleton y)) newEs
   in  g{outEdges = mapFromList id es $ outEdges g,
         inEdges  = mapFromList reverseEdge es $ inEdges g,
         edgeLabels =
            M.unionWith (error "insNodes: node already contained in graph")
               (edgeLabels g) (M.fromList es)}

mkGraph ::
   (Ord n) =>
   [LNode n nl] -> [LEdge n el] -> EfaGraph n nl el
mkGraph ns es =
   mkGraphFromMap (M.fromList ns) $ M.fromList es

mkGraphFromMap ::
   (Ord n) =>
   M.Map n nl -> M.Map (Edge n) el -> EfaGraph n nl el
mkGraphFromMap ns es = EfaGraph  outs ins  ns es
  where esl = M.keys es
        mapFromList f =
           flip M.union (fmap (const S.empty) ns) .
           M.fromListWith S.union .
           map (\e -> case f e of Edge x y -> (x, S.singleton y))
        outs = mapFromList id esl
        ins  = mapFromList reverseEdge esl

{-
instance (Ord n, Enum n) => IG.DynGraph (EfaGraph n) where
         (ins, ni, lab_, outs) & (EfaGraph os is ls es) =
           {- trace (show os ++ "\n" ++ show resOs ++ "\n" ++ show n++ "\n----\n") -} EfaGraph resOs resIs newLs newEs
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

nmap :: (nl0 -> nl1) -> EfaGraph n nl0 el -> EfaGraph n nl1 el
nmap f (EfaGraph outs ins nls els) =
   EfaGraph outs ins (fmap f nls) els

emap :: (el0 -> el1) -> EfaGraph n nl el0 -> EfaGraph n nl el1
emap f (EfaGraph outs ins nls els) =
   EfaGraph outs ins nls (fmap f els)

nodeSet :: EfaGraph n nl el -> S.Set n
nodeSet = M.keysSet . nodeLabels


type InOut n nl el = ([LNode n el], LNode n nl, [LNode n el])

mkInOutGraphFormat ::
   (Ord n) => EfaGraph n nl el -> [InOut n nl el]
mkInOutGraphFormat g =
   let filt edge n =
          map (\m -> (m, M.findWithDefault (error "mkInOutGraphFormat: edgeLabel not found") (edge m n) $ edgeLabels g)) .
          S.toList . M.findWithDefault S.empty n
   in  map
          (\nl@(n,_) ->
             (filt Edge n $ inEdges g, nl, filt (flip Edge) n $ outEdges g)) $
       M.toList $ nodeLabels g

mapGraph ::
   (Ord n) =>
   (InOut n nl el -> a) -> EfaGraph n nl el -> [a]
mapGraph f g = map f (mkInOutGraphFormat g)
