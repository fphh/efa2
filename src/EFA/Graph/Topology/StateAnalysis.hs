module EFA.Graph.Topology.StateAnalysis (
   advanced,
   bruteForce,
   branchAndBound,
   prioritized,
   clustering, clusteringGreedy, clusteringMinimizing,
   ) where

import qualified EFA.Graph as Gr
import qualified EFA.Graph.Topology as Topo
import EFA.Graph.Topology (FlowTopology, Topology)

import qualified EFA.Utility.Map as MapU

import qualified Data.Foldable as Fold
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.FingerTree.PSQueue as PSQ
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.FingerTree.PSQueue (PSQ)
import Data.PriorityQueue.FingerTree (PQueue)
import Data.NonEmpty ((!:))
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad (foldM, guard)
import Control.Functor.HT (void)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)


type NodeType = Topo.NodeType ()

-- How should it be ordered to be faster?
checkNodeType :: NodeType -> Bool -> Bool -> Bool
checkNodeType Topo.Crossing sucActive preActive = sucActive == preActive
checkNodeType Topo.NoRestriction _ _ = True
checkNodeType Topo.Source _ False = True
checkNodeType Topo.AlwaysSource True False = True
checkNodeType Topo.Sink False _ = True
checkNodeType Topo.AlwaysSink False True = True
checkNodeType Topo.DeadNode False False = True
checkNodeType (Topo.Storage _) _ _ = True
checkNodeType _ _ _ = False

-- Because of extend, we only have to deal with Dir edges here!
checkNode :: (Ord node) => FlowTopology node -> node -> Bool
checkNode topo x =
   case Map.lookup x $ Gr.graphMap topo of
      Nothing -> error "checkNode: node not in graph"
      Just (pre, node, suc) ->
         checkNodeType node
            (anyActive suc)
            (anyActive pre)


infix 1 `implies`

implies :: Bool -> Bool -> Bool
implies x y = not x || y

checkIncompleteNodeType :: NodeType -> Bool -> Bool -> Bool -> Bool
checkIncompleteNodeType typ complete sucActive preActive =
   case typ of
      Topo.Crossing -> complete `implies` sucActive == preActive
      Topo.Source -> not preActive
      Topo.AlwaysSource -> not preActive && (complete `implies` sucActive)
      Topo.Sink -> not sucActive
      Topo.AlwaysSink -> not sucActive && (complete `implies` preActive)
      Topo.Storage _ -> True
      Topo.NoRestriction -> True
      Topo.DeadNode -> not sucActive && not preActive

checkCountNode :: (Ord node) => CountTopology node -> node -> Bool
checkCountNode topo x =
   case Map.lookup x $ Gr.graphMap topo of
      Nothing -> error "checkNode: node not in graph"
      Just (pre, (node, nadj), suc) ->
         checkIncompleteNodeType node
            (Map.size pre + Map.size suc == nadj)
            (anyActive suc)
            (anyActive pre)

anyActive :: Map (Gr.EitherEdge node) () -> Bool
anyActive = Fold.any Topo.isActive . Map.keysSet

admissibleCountTopology :: (Ord node) => CountTopology node -> Bool
admissibleCountTopology topo =
   Fold.all (checkCountNode topo) $ Gr.nodeSet topo


type NumberOfAdj = Int
type CountTopology node =
        Gr.Graph node Gr.EitherEdge (NodeType, NumberOfAdj) ()

insEdge ::
   Ord node =>
   Gr.EitherEdge node -> CountTopology node -> CountTopology node
insEdge e = Gr.insEdge (e, ())

insEdgeSet ::
   Ord node =>
   Set (Gr.EitherEdge node) -> CountTopology node -> CountTopology node
insEdgeSet e = Gr.insEdgeSet (MapU.fromSet (const ()) e)

graphFromMap ::
   (Gr.Edge e, Ord (e n), Ord n) =>
   Map n nl -> Set (e n) -> Gr.Graph n e nl ()
graphFromMap ns es =
   Gr.fromMap ns (MapU.fromSet (const ()) es)


edgeOrients :: Ord node => Gr.DirEdge node -> [Gr.EitherEdge node]
edgeOrients (Gr.DirEdge x y) =
   (Gr.EDirEdge $ Gr.DirEdge x y) :
   (Gr.EDirEdge $ Gr.DirEdge y x) : -- x and y swapped!
   (Gr.EUnDirEdge $ Gr.unDirEdge x y) :
   []

admissibleEdges ::
   (Ord node) =>
   LNEdge node -> CountTopology node ->
   [(Gr.EitherEdge node, CountTopology node)]
admissibleEdges e0 g0 = do
   e1 <- edgeOrients e0
   let g1 = insEdge e1 g0
   guard $ Fold.all (checkCountNode g1) e0
   return (e1, g1)

expand :: (Ord node) => LNEdge node -> CountTopology node -> [CountTopology node]
expand e g = map snd $ admissibleEdges e g

splitNodesEdges :: (Ord node) => Topology node -> (CountTopology node, [Gr.DirEdge node])
splitNodesEdges topo =
   (Gr.fromMap
       (Map.map (\(pre,l,suc) -> (l, Set.size pre + Set.size suc)) $ Gr.nodes topo)
       Map.empty,
    Gr.edges topo)


newtype
   Alternatives node =
      Alternatives {getAlternatives :: [Gr.EitherEdge node]}

instance Eq  (Alternatives a) where (==)     =  equating  (void . getAlternatives)
instance Ord (Alternatives a) where compare  =  comparing (void . getAlternatives)

alternatives ::
   (Ord node) => LNEdge node -> CountTopology node -> Alternatives node
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
                newTopo <- map (flip insEdge topo) edges
                recourse
                   (newTopo,
                    Set.foldl
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
@ns == (foldMap (foldMap Set.singleton) $ Map.keys $ head ess)@.
-}
data
   Cluster node =
      Cluster {
         clusterNodes :: Set node,
         clusterEdges :: [Set (Gr.EitherEdge node)]
      }


emptyCluster ::
   (Ord node) =>
   CountTopology node -> Cluster node
emptyCluster g =
   Cluster Set.empty
      (guard (admissibleCountTopology g) >> [Set.empty])

singletonCluster ::
   (Ord node) =>
   CountTopology node -> Gr.DirEdge node -> Cluster node
singletonCluster g e =
   Cluster
      (Fold.foldMap Set.singleton e)
      (map (Set.singleton . fst) $ admissibleEdges e g)

mergeCluster ::
   (Ord node) =>
   CountTopology node ->
   Cluster node -> Cluster node -> Cluster node
mergeCluster topo c0 c1 =
   let nodes = Set.union (clusterNodes c0) (clusterNodes c1)
   in  Cluster nodes $ do
          es0 <- clusterEdges c0
          es1 <- clusterEdges c1
          let es2 = Set.union es0 es1
              g = insEdgeSet es2 topo
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
               map (\es -> Gr.nmap fst $ insEdgeSet es topo) $
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
         map (\es -> Gr.nmap fst $ insEdgeSet es topo) $
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
         map (\es -> Gr.nmap fst $ insEdgeSet es topo) $
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


type LNEdge node = Gr.DirEdge node

-- * various algorithms

bruteForce :: (Ord node) => Topology node -> [FlowTopology node]
bruteForce topo =
   filter (\g -> Fold.all (checkNode g) $ Gr.nodeSet g) .
   map (graphFromMap (Gr.nodeLabels topo) . Set.fromList) $
   mapM edgeOrients $ Gr.edges topo

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
