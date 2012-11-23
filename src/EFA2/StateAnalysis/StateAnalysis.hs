module EFA2.StateAnalysis.StateAnalysis (
   advanced,
   bruteForce,
   branchAndBound,
   prioritized,
   clustering,

   propBranchAndBound,
   propPrioritized,
   propClustering,

   speedBruteForce,
   speedBranchAndBound,
   speedPrioritized,
   speedClustering,
   ) where

-- This algorithm is made after reading R. Birds "Making a Century" in Pearls of Functional Algorithm Design.

-- Could it be made faster using stream fusion?
-- Probably not, because we keep the hole list. No consumer here.
--import Prelude hiding (map, length, filter, concatMap, all, (++), foldr)
--import Data.List.Stream

--import qualified Data.Vector as V
--import Data.Function (on)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.EfaGraph as Gr
import EFA2.Topology.TopologyData
          (FlowTopology, Topology, NodeType(..),
           FlowDirection(UnDir, Dir), isActive)
import EFA2.Utils.Utils (mapFromSet)

import qualified Data.List.Key as Key
import qualified Data.Foldable as Fold
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.FingerTree.PSQueue as PSQ
import qualified Data.PQueue.Prio.Min as PQ
import Data.FingerTree.PSQueue (PSQ)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.Traversable (sequenceA)
import Data.Monoid (mappend)
import Control.Monad (liftM2, foldM, guard)
import Control.Functor.HT (void)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)

import qualified Test.QuickCheck as QC


-- import Debug.Trace

-- How should it be orderd to be faster?
checkNodeType :: NodeType -> Bool -> Bool -> Bool
checkNodeType Crossing sucActive preActive = sucActive == preActive
checkNodeType NoRestriction _ _ = True
checkNodeType Source _ False = True
checkNodeType AlwaysSource True False = True
checkNodeType Sink False _ = True
checkNodeType AlwaysSink False True = True
checkNodeType DeadNode False False = True
checkNodeType Storage _ _ = True
checkNodeType _ _ _ = False

-- Because of extend, we only do have to deal with Dir edges here!
checkNode :: FlowTopology -> Idx.Node -> Bool
checkNode topo x =
   case M.lookup x $ Gr.nodes topo of
      Nothing -> error "checkNode: node not in graph"
      Just (pre, nty, suc) ->
         checkNodeType nty
            (anyActive $ Gr.sucEdgeLabels topo x suc)
            (anyActive $ Gr.preEdgeLabels topo x pre)


infix 1 `implies`

implies :: Bool -> Bool -> Bool
implies x y = not x || y

checkIncompleteNodeType :: NodeType -> Bool -> Bool -> Bool -> Bool
checkIncompleteNodeType typ complete sucActive preActive =
   case typ of
      Crossing -> complete `implies` sucActive == preActive
      Source -> not preActive
      AlwaysSource -> not preActive && (complete `implies` sucActive)
      Sink -> not sucActive
      AlwaysSink -> not sucActive && (complete `implies` preActive)
      Storage -> True
      NoRestriction -> True
      DeadNode -> not sucActive && not preActive

checkCountNode :: CountTopology -> Idx.Node -> Bool
checkCountNode topo x =
   case M.lookup x $ Gr.nodes topo of
      Nothing -> error "checkNode: node not in graph"
      Just (pre, (nty, nadj), suc) ->
         checkIncompleteNodeType nty
            (S.size pre + S.size suc == nadj)
            (anyActive $ Gr.sucEdgeLabels topo x suc)
            (anyActive $ Gr.preEdgeLabels topo x pre)

anyActive :: [(n, FlowDirection)] -> Bool
anyActive = any (isActive . snd)

admissibleCountTopology :: CountTopology -> Bool
admissibleCountTopology topo =
   Fold.all (checkCountNode topo) $ Gr.nodeSet topo


type NumberOfAdj = Int
type CountTopology = Gr.EfaGraph Idx.Node (NodeType, NumberOfAdj) FlowDirection


{-
-- Surprisingly, the Vector version is not faster than the IntMap version.
-- Does IntMap have constant access times for lookups?

type GraphInfo = V.Vector (Maybe (NodeType, NumberOfAdj))

buildInfo :: FlowTopology -> GraphInfo
buildInfo topo = V.fromList xs
  where ns = labNodes topo
        xs = map snd $ makeContigous $ map f ns
        f (n, l) = (n, Just (nodetypeNLabel l, length (pre topo n) + length (suc topo n)))

makeContigous :: [(Int, Maybe a)] -> [(Int, Maybe a)]
makeContigous xs = reverse ys
  where xs' = L.sortBy (compare `on` fst) xs
        ys = L.foldl' f [] xs'
        f [] (0, d) = [(0, d)]
        f [] x = x:map (, Nothing) [(fst x - 1), (fst x-2)..0]
        f (a:acc) x = x:(map (, Nothing) [(fst x - 1), (fst x - 2)..(fst a +1)] ++ a:acc)
-}

edgeOrients :: Gr.Edge node -> [(Gr.Edge node, FlowDirection)]
edgeOrients (Gr.Edge x y) =
   (Gr.Edge x y, Dir) :
   (Gr.Edge y x, Dir) : -- x and y inversed!
   (Gr.Edge x y, UnDir) :
   []

admissibleEdges ::
   LNEdge -> CountTopology ->
   [((Gr.Edge Idx.Node, FlowDirection), CountTopology)]
admissibleEdges e0 g0 = do
   e1 <- edgeOrients e0
   let g1 = Gr.insEdge e1 g0
   guard $ Fold.all (checkCountNode g1) e0
   return (e1, g1)

expand :: LNEdge -> CountTopology -> [CountTopology]
expand e g = map snd $ admissibleEdges e g

splitNodesEdges :: Topology -> (CountTopology, [Gr.Edge Idx.Node])
splitNodesEdges topo =
   (Gr.fromMap
       (M.map (\(pre,l,suc) -> (l, S.size pre + S.size suc)) $ Gr.nodes topo)
       M.empty,
    map fst $ Gr.labEdges topo)


newtype
   Alternatives =
      Alternatives {getAlternatives :: [(Gr.Edge Idx.Node, FlowDirection)]}

instance Eq  Alternatives where (==)     =  equating  (void . getAlternatives)
instance Ord Alternatives where compare  =  comparing (void . getAlternatives)

alternatives :: LNEdge -> CountTopology -> Alternatives
alternatives e g =
   Alternatives $ map fst $ admissibleEdges e g

recoursePrioEdge ::
   Topology ->
   (CountTopology, PSQ LNEdge Alternatives) ->
   [(CountTopology, PSQ LNEdge Alternatives)]
recoursePrioEdge origTopo =
   let recourse tq@(topo, queue) =
          case PSQ.minView queue of
             Nothing -> [tq]
             Just (bestEdge PSQ.:-> Alternatives edges, remQueue) -> do
                newTopo <- map (flip Gr.insEdge topo) edges
                recourse
                   (newTopo,
                    S.foldl
                       (\q e -> PSQ.adjust (const $ alternatives e newTopo) e q)
                       remQueue $
                    Fold.foldMap (Gr.adjEdges origTopo) bestEdge)
   in  recourse


-- move to Utils?
untilLeft :: (a -> Either b a) -> a -> b
untilLeft f =
   let go a0 =
          case f a0 of
             Left b -> b
             Right a1 -> go a1
   in  go

{-
The edge set in all list elements must be equal if neglecting edge orientation.

We maintain the set of nodes only for reasons of efficiency.
For @Cluster ns ess@ it must hold
@ns == (foldMap (foldMap S.singleton) $ M.keys $ head ess)@.
-}
data
   Cluster =
      Cluster {
         clusterNodes :: S.Set Idx.Node,
         clusterEdges :: [M.Map (Gr.Edge Idx.Node) FlowDirection]
      }


emptyCluster ::
   CountTopology -> Cluster
emptyCluster g =
   Cluster S.empty
      (guard (admissibleCountTopology g) >> [M.empty])

singletonCluster ::
   CountTopology -> Gr.Edge Idx.Node -> Cluster
singletonCluster g e =
   Cluster
      (Fold.foldMap S.singleton e)
      (map (uncurry M.singleton . fst) $ admissibleEdges e g)

mergeCluster ::
   CountTopology ->
   Cluster -> Cluster -> Cluster
mergeCluster topo c0 c1 =
   let nodes = S.union (clusterNodes c0) (clusterNodes c1)
   in  Cluster nodes $ do
          es0 <- clusterEdges c0
          es1 <- clusterEdges c1
          let es2 = M.union es0 es1
              g = Gr.insEdgeSet es2 topo
          guard $ Fold.all (checkCountNode g) nodes
          return es2

{- |
Merge the two clusters with the least numbers of possibilities.
-}
mergeQueuedCluster ::
   CountTopology ->
   MinPQueue Int Cluster ->
   Either
      [FlowTopology]
      (MinPQueue Int Cluster)
mergeQueuedCluster topo queue0 =
   case PQ.minView queue0 of
      Nothing -> error "empty queue"
      Just (c0, queue1) ->
         case PQ.minView queue1 of
            Nothing ->
               Left $
               map (\es -> Gr.nmap fst $ Gr.insEdgeSet es topo) $
               clusterEdges c0
            Just (c1, queue2) -> Right $
               let c2 = mergeCluster topo c0 c1
               in  PQ.insert (length $ clusterEdges c2) c2 queue2



type LNEdge = Gr.Edge Idx.Node

-- * various algorithms

bruteForce :: Topology -> [FlowTopology]
bruteForce topo =
   filter (\g -> Fold.all (checkNode g) $ Gr.nodeSet g) .
   map (Gr.fromMap (Gr.nodeLabels topo) . M.fromList) $
   mapM (edgeOrients . fst) $ Gr.labEdges topo

branchAndBound :: Topology -> [FlowTopology]
branchAndBound topo =
   map (Gr.nmap fst) $
   uncurry (foldM (flip expand)) $
   splitNodesEdges topo

prioritized :: Topology -> [FlowTopology]
prioritized topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  guard (admissibleCountTopology cleanTopo)
       >>
       (map (Gr.nmap fst . fst) $
        recoursePrioEdge topo $
        (cleanTopo,
         PSQ.fromList $ map (\e -> e PSQ.:-> alternatives e cleanTopo) es))

clustering :: Topology -> [FlowTopology]
clustering topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  untilLeft (mergeQueuedCluster cleanTopo) $
       PQ.fromList $
       map (\c -> (length $ clusterEdges c, c)) $
       (emptyCluster cleanTopo :) $
       map (singletonCluster cleanTopo) es

advanced :: Topology -> [FlowTopology]
advanced = clustering


-- * tests

data UndirEdge n = UndirEdge n n
   deriving (Eq, Ord, Show)

undirEdge :: Ord n => n -> n -> UndirEdge n
undirEdge x y =
   if x<y then UndirEdge x y else UndirEdge y x

instance (QC.Arbitrary n, Ord n) => QC.Arbitrary (UndirEdge n) where
   arbitrary = liftM2 undirEdge QC.arbitrary QC.arbitrary
   shrink (UndirEdge x y) =
      S.toList $ S.fromList $ map (uncurry undirEdge) $ QC.shrink (x,y)

instance Fold.Foldable UndirEdge where
   foldMap f (UndirEdge x y) = mappend (f x) (f y)


maxArbEdges :: Int
maxArbEdges = 6

newtype ArbTopology = ArbTopology Topology
   deriving (Show)

instance QC.Arbitrary ArbTopology where
   shrink (ArbTopology g) =
      case Gr.nodeSet g of
         ns ->
            map (ArbTopology . flip Gr.delNodeSet g .
                 S.difference ns . S.fromList) $
            QC.shrink $ S.toList ns
   arbitrary = do
      edges <-
         fmap (M.fromList . take maxArbEdges) QC.arbitrary
      nodes <-
         sequenceA $ mapFromSet (const QC.arbitrary) $
         Fold.foldMap (Fold.foldMap S.singleton) $
         M.keys edges
      return $ ArbTopology $
         Gr.fromMap nodes $
         M.mapKeys (\(UndirEdge x y) -> Gr.Edge x y) edges

propBranchAndBound :: ArbTopology -> Bool
propBranchAndBound (ArbTopology g) =
   bruteForce g == branchAndBound g


{- |
I could declare an Ord instance for EfaGraph,
but I think that @graph0 < graph1@ should be a static error.
Instead I use this function locally for 'Key.sort'.
-}
graphIdent ::
   Gr.EfaGraph node nodeLabel edgeLabel ->
   (M.Map node nodeLabel,
    M.Map (Gr.Edge node) edgeLabel)
graphIdent g = (Gr.nodeLabels g, Gr.edgeLabels g)

{-
I do not convert to Set, but use 'sort' in order to check for duplicates.
-}
propPrioritized :: ArbTopology -> Bool
propPrioritized (ArbTopology g) =
   Key.sort graphIdent (branchAndBound g)
   ==
   Key.sort graphIdent (prioritized g)

propClustering :: ArbTopology -> Bool
propClustering (ArbTopology g) =
   Key.sort graphIdent (branchAndBound g)
   ==
   Key.sort graphIdent (clustering g)


speedBruteForce :: ArbTopology -> Bool
speedBruteForce (ArbTopology g) =
   bruteForce g == bruteForce g

speedBranchAndBound :: ArbTopology -> Bool
speedBranchAndBound (ArbTopology g) =
   branchAndBound g == branchAndBound g

speedPrioritized :: ArbTopology -> Bool
speedPrioritized (ArbTopology g) =
   prioritized g == prioritized g

speedClustering :: ArbTopology -> Bool
speedClustering (ArbTopology g) =
   clustering g == clustering g
