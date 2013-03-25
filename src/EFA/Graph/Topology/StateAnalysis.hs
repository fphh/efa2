module EFA.Graph.Topology.StateAnalysis (
   advanced,
   bruteForce,
   branchAndBound,
   prioritized,
   clustering, clusteringGreedy, clusteringMinimizing,

   propBranchAndBound,
   propPrioritized,
   propClustering, propClusteringGreedy, propClusteringMinimizing,

   speedBruteForce,
   speedBranchAndBound,
   speedPrioritized,
   speedClustering,
   ) where

import qualified EFA.Graph as Gr
import EFA.Graph.Topology
          (FlowTopology, Topology, NodeType(..),
           FlowDirection(UnDir, Dir), isActive)
import EFA.Utility (mapFromSet)

import qualified Data.List.Key as Key
import qualified Data.Foldable as Fold
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.FingerTree.PSQueue as PSQ
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.FingerTree.PSQueue (PSQ)
import Data.PriorityQueue.FingerTree (PQueue)
import Data.Traversable (sequenceA)
import Data.Monoid (mappend)
import Data.NonEmpty ((!:))
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
checkNode :: (Ord node) => FlowTopology node -> node -> Bool
checkNode topo x =
   case M.lookup x $ Gr.nodes topo of
      Nothing -> error "checkNode: node not in graph"
      Just (pre, node, suc) ->
         checkNodeType node
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

checkCountNode :: (Ord node) => CountTopology node -> node -> Bool
checkCountNode topo x =
   case M.lookup x $ Gr.nodes topo of
      Nothing -> error "checkNode: node not in graph"
      Just (pre, (node, nadj), suc) ->
         checkIncompleteNodeType node
            (S.size pre + S.size suc == nadj)
            (anyActive $ Gr.sucEdgeLabels topo x suc)
            (anyActive $ Gr.preEdgeLabels topo x pre)

anyActive :: [(n, FlowDirection)] -> Bool
anyActive = any (isActive . snd)

admissibleCountTopology :: (Ord node) => CountTopology node -> Bool
admissibleCountTopology topo =
   Fold.all (checkCountNode topo) $ Gr.nodeSet topo


type NumberOfAdj = Int
type CountTopology node = Gr.Graph node (NodeType, NumberOfAdj) FlowDirection


edgeOrients :: Gr.Edge node -> [(Gr.Edge node, FlowDirection)]
edgeOrients (Gr.Edge x y) =
   (Gr.Edge x y, Dir) :
   (Gr.Edge y x, Dir) : -- x and y inversed!
   (Gr.Edge x y, UnDir) :
   []

admissibleEdges ::
   (Ord node) =>
   LNEdge node -> CountTopology node ->
   [((Gr.Edge node, FlowDirection), CountTopology node)]
admissibleEdges e0 g0 = do
   e1 <- edgeOrients e0
   let g1 = Gr.insEdge e1 g0
   guard $ Fold.all (checkCountNode g1) e0
   return (e1, g1)

expand :: (Ord node) => LNEdge node -> CountTopology node -> [CountTopology node]
expand e g = map snd $ admissibleEdges e g

splitNodesEdges :: (Ord node) => Topology node -> (CountTopology node, [Gr.Edge node])
splitNodesEdges topo =
   (Gr.fromMap
       (M.map (\(pre,l,suc) -> (l, S.size pre + S.size suc)) $ Gr.nodes topo)
       M.empty,
    map fst $ Gr.labEdges topo)


newtype
   Alternatives node =
      Alternatives {getAlternatives :: [(Gr.Edge node, FlowDirection)]}

instance Eq  (Alternatives a) where (==)     =  equating  (void . getAlternatives)
instance Ord (Alternatives a) where compare  =  comparing (void . getAlternatives)

alternatives :: (Ord node) => LNEdge node -> CountTopology node -> Alternatives node
alternatives e g =
   Alternatives $ map fst $ admissibleEdges e g

recoursePrioEdge ::
   (Ord node) =>
   Topology node ->
   (CountTopology node, PSQ (LNEdge node) (Alternatives node)) ->
   [(CountTopology node, PSQ (LNEdge node) (Alternatives node))]
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
   Cluster node =
      Cluster {
         clusterNodes :: S.Set node,
         clusterEdges :: [M.Map (Gr.Edge node) FlowDirection]
      }


emptyCluster ::
   (Ord node) =>
   CountTopology node -> Cluster node
emptyCluster g =
   Cluster S.empty
      (guard (admissibleCountTopology g) >> [M.empty])

singletonCluster ::
   (Ord node) =>
   CountTopology node -> Gr.Edge node -> Cluster node
singletonCluster g e =
   Cluster
      (Fold.foldMap S.singleton e)
      (map (uncurry M.singleton . fst) $ admissibleEdges e g)

mergeCluster ::
   (Ord node) =>
   CountTopology node ->
   Cluster node -> Cluster node -> Cluster node
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
mergeSmallestClusters ::
   (Ord node) =>
   CountTopology node ->
   PQueue Int (Cluster node) ->
   Either
      [FlowTopology node]
      (PQueue Int (Cluster node))
mergeSmallestClusters topo queue0 =
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



data ShortestList a b =
   ShortestList {
      _shortestListKey :: [a],
      shortestListValue :: b
   }

{- |
The result list may not equal one of the input lists.
We should replace this by Peano numbers.
-}
shorterList :: ShortestList a b -> ShortestList a b -> ShortestList a b
shorterList (ShortestList x0 xv) (ShortestList y0 yv) =
   let go (x:xs) (_:ys) =
          let ShortestList zs zv = go xs ys
          in  ShortestList (x:zs) zv
       go [] _ = ShortestList [] xv
       go _  _ = ShortestList [] yv
   in  go x0 y0

{- |
Return the value that is associated with the shortest list.
Lists are only evaluated as far as necessary
for finding the list with minimum length.
-}
shortestList :: NonEmpty.T [] (ShortestList a b) -> b
shortestList = shortestListValue . NonEmpty.foldl1 shorterList


{- |
Merge the two clusters
that give the minimal number of possibilities when merged.
-}
mergeMinimizingClusterPairs ::
   (Ord node) =>
   CountTopology node ->
   NonEmpty.T [] (Cluster node) ->
   Either [FlowTopology node] (NonEmpty.T [] (Cluster node))
mergeMinimizingClusterPairs topo (NonEmpty.Cons p ps) =
   case NonEmpty.fetch ps of
      Nothing ->
         Left $
         map (\es -> Gr.nmap fst $ Gr.insEdgeSet es topo) $
         clusterEdges p
      Just partition0 ->
         Right $
         shortestList $ do
            (c0, partition1) <-
               NonEmpty.flatten $ NonEmpty.removeEach $ p !: partition0
            (c1, partition2) <- NonEmpty.removeEach partition1
            let c = mergeCluster topo c0 c1
            return $ ShortestList (clusterEdges c) (c !: partition2)

{- |
Merge the cluster with the minimal number of possibilities
with the cluster that minimizes the number of possibilities
when merged with the first one.

Usually, when the merged cluster is connected
then there are less possibilities than for non-connected clusters.
That is, our selection strategy tends to produce connected clusters.
-}
mergeMinimizingCluster ::
   (Ord node) =>
   CountTopology node ->
   NonEmpty.T [] (Cluster node) ->
   Either [FlowTopology node] (NonEmpty.T [] (Cluster node))
mergeMinimizingCluster topo (NonEmpty.Cons p ps) =
   case NonEmpty.fetch ps of
      Nothing ->
         Left $
         map (\es -> Gr.nmap fst $ Gr.insEdgeSet es topo) $
         clusterEdges p
      Just partition0 ->
         let (c0,partition1) =
                shortestList $
                fmap (\(c,cs) -> ShortestList (clusterEdges c) (c,cs)) $
                NonEmpty.flatten $
                NonEmpty.removeEach $ p !: partition0
         in  Right $
             shortestList $
             fmap
                (\(c,cs) ->
                   let cm = mergeCluster topo c0 c
                   in  ShortestList (clusterEdges cm) (cm!:cs)) $
             NonEmpty.removeEach partition1


type LNEdge node = Gr.Edge node

-- * various algorithms

bruteForce :: (Ord node) => Topology node -> [FlowTopology node]
bruteForce topo =
   filter (\g -> Fold.all (checkNode g) $ Gr.nodeSet g) .
   map (Gr.fromMap (Gr.nodeLabels topo) . M.fromList) $
   mapM (edgeOrients . fst) $ Gr.labEdges topo

{-
This algorithm is made after reading R. Birds "Making a Century"
in Pearls of Functional Algorithm Design.
-}
branchAndBound :: (Ord node) => Topology node -> [FlowTopology node]
branchAndBound topo =
   map (Gr.nmap fst) $
   uncurry (foldM (flip expand)) $
   splitNodesEdges topo

prioritized :: (Ord node) => Topology node -> [FlowTopology node]
prioritized topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  guard (admissibleCountTopology cleanTopo)
       >>
       (map (Gr.nmap fst . fst) $
        recoursePrioEdge topo $
        (cleanTopo,
         PSQ.fromList $ map (\e -> e PSQ.:-> alternatives e cleanTopo) es))

clusteringGreedy :: (Ord node) => Topology node -> [FlowTopology node]
clusteringGreedy topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  untilLeft (mergeSmallestClusters cleanTopo) $
       PQ.fromList $
       map (\c -> (length $ clusterEdges c, c)) $
       emptyCluster cleanTopo :
          map (singletonCluster cleanTopo) es

clusteringMinimizing :: (Ord node) => Topology node -> [FlowTopology node]
clusteringMinimizing topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  untilLeft (mergeMinimizingClusterPairs cleanTopo) $
       emptyCluster cleanTopo !:
          map (singletonCluster cleanTopo) es

clustering :: (Ord node) => Topology node -> [FlowTopology node]
clustering topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  untilLeft (mergeMinimizingCluster cleanTopo) $
       emptyCluster cleanTopo !:
          map (singletonCluster cleanTopo) es

advanced :: (Ord node) => Topology node -> [FlowTopology node]
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

newtype ArbTopology node = ArbTopology (Topology node)
   deriving (Show)

instance (QC.Arbitrary node, Ord node) => QC.Arbitrary (ArbTopology node) where
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

propBranchAndBound :: (Eq node, Ord node) => ArbTopology node -> Bool
propBranchAndBound (ArbTopology g) =
   bruteForce g == branchAndBound g


{- |
I could declare an Ord instance for Graph,
but I think that @graph0 < graph1@ should be a static error.
Instead I use this function locally for 'Key.sort'.
-}
graphIdent ::
   Gr.Graph node nodeLabel edgeLabel ->
   (M.Map node nodeLabel,
    M.Map (Gr.Edge node) edgeLabel)
graphIdent g = (Gr.nodeLabels g, Gr.edgeLabels g)

{-
I do not convert to Set, but use 'sort' in order to check for duplicates.
-}
propPrioritized :: (Eq node, Ord node) => ArbTopology node -> Bool
propPrioritized (ArbTopology g) =
   Key.sort graphIdent (branchAndBound g)
   ==
   Key.sort graphIdent (prioritized g)

propClustering :: (Eq node, Ord node) => ArbTopology node -> Bool
propClustering (ArbTopology g) =
   Key.sort graphIdent (branchAndBound g)
   ==
   Key.sort graphIdent (clustering g)

propClusteringGreedy :: (Eq node, Ord node) => ArbTopology node -> Bool
propClusteringGreedy (ArbTopology g) =
   Key.sort graphIdent (branchAndBound g)
   ==
   Key.sort graphIdent (clusteringGreedy g)

propClusteringMinimizing :: (Eq node, Ord node) => ArbTopology node -> Bool
propClusteringMinimizing (ArbTopology g) =
   Key.sort graphIdent (branchAndBound g)
   ==
   Key.sort graphIdent (clusteringMinimizing g)


speedBruteForce :: (Eq node, Ord node) => ArbTopology node -> Bool
speedBruteForce (ArbTopology g) =
   bruteForce g == bruteForce g

speedBranchAndBound :: (Eq node, Ord node) => ArbTopology node -> Bool
speedBranchAndBound (ArbTopology g) =
   branchAndBound g == branchAndBound g

speedPrioritized :: (Eq node, Ord node) => ArbTopology node -> Bool
speedPrioritized (ArbTopology g) =
   prioritized g == prioritized g

speedClustering :: (Eq node, Ord node) => ArbTopology node -> Bool
speedClustering (ArbTopology g) =
   clustering g == clustering g
