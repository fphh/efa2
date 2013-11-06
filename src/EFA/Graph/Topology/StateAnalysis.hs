module EFA.Graph.Topology.StateAnalysis (
   advanced,
   bruteForce,
   branchAndBound,
   prioritized,
   clustering, clusteringGreedy, clusteringMinimizing,
   setCover,

   admissibleTopology,
   identify,
   minimalGiven,

   minimalGivenDuplicate,
   checkNodeType,
   ) where

import qualified EFA.Graph.Topology.Count as Count
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph; import EFA.Graph (Graph)
import EFA.Graph.Topology.Count
          (CountTopology, splitNodesEdges, nodeDegrees, removeCounts)
import EFA.Graph.Topology (FlowTopology, Topology)

import qualified EFA.Utility.Map as MapU

import qualified Math.SetCover.Exact as SetCover

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import qualified Data.FingerTree.PSQueue as PSQ
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.FingerTree.PSQueue (PSQ)
import Data.PriorityQueue.FingerTree (PQueue)
import Data.NonEmpty ((!:))
import Data.List (unfoldr)
import Control.Monad (foldM, guard)
import Control.Functor.HT (void)
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (mapSnd)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)


type NodeType = Node.Type

-- How should it be ordered to be faster?
{- |
We export this function only for testing.
Do not use it outside of the module.
-}
checkNodeType :: NodeType -> Bool -> Bool -> Bool
checkNodeType Node.Crossing sucActive preActive = sucActive == preActive
checkNodeType Node.NoRestriction _ _ = True
checkNodeType Node.Source _ False = True
checkNodeType Node.AlwaysSource True False = True
checkNodeType Node.Sink False _ = True
checkNodeType Node.AlwaysSink False True = True
checkNodeType Node.DeadNode False False = True
checkNodeType Node.Storage _ _ = True
checkNodeType _ _ _ = False


checkInOut ::
   (Node.C node) =>
   NodeType -> Topo.InOut node () -> Bool
checkInOut nodeType (pre, (), suc) =
   checkNodeType nodeType
      (Topo.anyActive suc)
      (Topo.anyActive pre)


admissibleTopology :: (Node.C node) => FlowTopology node -> Bool
admissibleTopology =
   Fold.and . Map.mapWithKey (checkInOut . Node.typ) . Graph.graphMap


graphFromMap ::
   (Graph.Edge e, Ord (e n), Ord n) =>
   Map n nl -> Set (e n) -> Graph n e nl ()
graphFromMap ns es =
   Graph.fromMap ns (MapU.fromSet (const ()) es)

replaceEdges ::
   (Ord n, Ord (e0 n), Ord (e1 n), Graph.Edge e0, Graph.Edge e1) =>
   Graph n e0 nl el -> [e1 n] -> Graph n e1 nl ()
replaceEdges topo edges =
   graphFromMap (Graph.nodeLabels topo) $ Set.fromList edges


newtype
   Alternatives node =
      Alternatives {getAlternatives :: [Graph.EitherEdge node]}

instance Eq  (Alternatives a) where (==)     =  equating  (void . getAlternatives)
instance Ord (Alternatives a) where compare  =  comparing (void . getAlternatives)

alternatives ::
   (Node.C node) =>
   Graph.DirEdge node -> CountTopology node -> Alternatives node
alternatives e g =
   Alternatives $ map fst $ Count.admissibleEdges e g

recoursePrioEdge ::
   (Node.C node) =>
   Topology node ->
   (CountTopology node, PSQ (Graph.DirEdge node) (Alternatives node)) ->
   [(CountTopology node, PSQ (Graph.DirEdge node) (Alternatives node))]
recoursePrioEdge origTopo =
   let recourse tq@(topo, queue) =
          case PSQ.minView queue of
             Nothing -> [tq]
             Just (bestEdge PSQ.:-> Alternatives edges, remQueue) -> do
                newTopo <- map (flip Count.insertEdge topo) edges
                recourse
                   (newTopo,
                    Set.foldl
                       (\q e -> PSQ.adjust (const $ alternatives e newTopo) e q)
                       remQueue $
                    Fold.foldMap (Graph.adjacentEdges origTopo) bestEdge)
   in  recourse


-- move to Utils?
untilLeft :: (a -> Either b a) -> a -> b
untilLeft f =
   let go a0 =
          case f a0 of
             Left b -> b
             Right a1 -> go a1
   in  go


-- * clustering

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
         clusterEdges :: [Set (Graph.EitherEdge node)]
      }


emptyCluster ::
   (Node.C node) =>
   CountTopology node -> Cluster node
emptyCluster g =
   Cluster Set.empty
      (guard (Count.admissibleTopology g) >> [Set.empty])

singletonCluster ::
   (Node.C node) =>
   CountTopology node -> Graph.DirEdge node -> Cluster node
singletonCluster g e =
   Cluster
      (Fold.foldMap Set.singleton e)
      (map (Set.singleton . fst) $ Count.admissibleEdges e g)

mergeCluster ::
   (Node.C node) =>
   CountTopology node ->
   Cluster node -> Cluster node -> Cluster node
mergeCluster topo c0 c1 =
   let nodes = Set.union (clusterNodes c0) (clusterNodes c1)
   in  Cluster nodes $ do
          es0 <- clusterEdges c0
          es1 <- clusterEdges c1
          let es2 = Set.union es0 es1
              g = Count.insertEdgeSet es2 topo
          guard $ Fold.all (Count.checkNode g) nodes
          return es2

{- |
Merge the two clusters with the least numbers of possibilities.
-}
mergeSmallestClusters ::
   (Node.C node) =>
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
               map (\es -> removeCounts $ Count.insertEdgeSet es topo) $
               clusterEdges c0
            Just (c1, queue2) -> Right $
               let c2 = mergeCluster topo c0 c1
               in  PQ.insert (length $ clusterEdges c2) c2 queue2



data ShortestList a b =
   ShortestList {
      _shortestListKey :: [a],
      shortestListValue :: b
   }

smallestCluster ::
   Cluster node -> b ->
   ShortestList (Set (Graph.EitherEdge node)) b
smallestCluster = ShortestList . clusterEdges

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
   (Node.C node) =>
   CountTopology node ->
   NonEmpty.T [] (Cluster node) ->
   Either [FlowTopology node] (NonEmpty.T [] (Cluster node))
mergeMinimizingClusterPairs topo (NonEmpty.Cons p ps) =
   case NonEmpty.fetch ps of
      Nothing ->
         Left $
         map (\es -> removeCounts $ Count.insertEdgeSet es topo) $
         clusterEdges p
      Just partition0 ->
         Right $
         shortestList $ do
            (c0, partition1) <-
               NonEmpty.flatten $ NonEmpty.removeEach $ p !: partition0
            (c1, partition2) <- NonEmpty.removeEach partition1
            let c = mergeCluster topo c0 c1
            return $ smallestCluster c (c !: partition2)

{- |
Merge the cluster with the minimal number of possibilities
with the cluster that minimizes the number of possibilities
when merged with the first one.

Usually, when the merged cluster is connected
then there are less possibilities than for non-connected clusters.
That is, our selection strategy tends to produce connected clusters.
-}
mergeMinimizingCluster ::
   (Node.C node) =>
   CountTopology node ->
   NonEmpty.T [] (Cluster node) ->
   Either [FlowTopology node] (NonEmpty.T [] (Cluster node))
mergeMinimizingCluster topo (NonEmpty.Cons p ps) =
   case NonEmpty.fetch ps of
      Nothing ->
         Left $
         map (\es -> removeCounts $ Count.insertEdgeSet es topo) $
         clusterEdges p
      Just partition0 ->
         let (c0,partition1) =
                shortestList $
                fmap (\(c,cs) -> smallestCluster c (c,cs)) $
                NonEmpty.flatten $
                NonEmpty.removeEach $ p !: partition0
         in  Right $
             shortestList $
             fmap
                (\(c,cs) ->
                   let cm = mergeCluster topo c0 c
                   in  smallestCluster cm (cm!:cs)) $
             NonEmpty.removeEach partition1



-- * set covering

{- |
Formulate the problem as exact set cover problem.
Every node can choose the tail (Topo.Out) or head (Topo.In) of an edge.
This is expressed by 'setCoverDirEdges'.
The set cover algorithm finds a solution
where for each edge a tail and a head is chosen by the connected nodes.
If both tail and head are not assigned to nodes
then the undirected edges jump in.
This is expressed by 'setCoverUnDirEdges'.
Additionally, 'setCoverDirEdges' needs to add a node identification
in order to make sure that every node is considered only once.
-}
data SetCoverItem node =
     SetCoverNode node
   | SetCoverEdge (Graph.DirEdge node) Topo.StoreDir
   deriving (Eq, Ord)

setCoverUnDirEdges, setCoverDirEdges ::
   (Node.C node) =>
   Topology node ->
   [SetCover.Assign [Graph.EitherEdge node] (Set (SetCoverItem node))]
setCoverUnDirEdges topo =
   Map.elems $
   Map.mapWithKey
      (\e _ ->
         SetCover.assign
            [Graph.EUnDirEdge $ Graph.unDirEdge (Graph.from e) (Graph.to e)]
            (Set.fromList [SetCoverEdge e Topo.In, SetCoverEdge e Topo.Out])) $
   Graph.edgeLabels topo

setCoverDirEdges topo =
   Fold.fold $
   Map.mapWithKey
      (\node (pre, (), suc) ->
         map
            (\edges ->
               SetCover.assign
                  (map (Graph.EDirEdge . fst) $ Map.elems edges)
                  (Set.fromList $
                   SetCoverNode node :
                      (map (uncurry SetCoverEdge) $
                       Map.toList $ fmap snd edges))) $
         filter
            (\edges ->
               checkNodeType (Node.typ node)
                  (not $ Map.null $ Map.filter ((Topo.Out ==) . snd) edges)
                  (not $ Map.null $ Map.filter ((Topo.In  ==) . snd) edges)) $
         map (Map.mapMaybe id) $
         Trav.sequenceA $
         Map.unionWith (error "setCover: duplicate edge")
            (Map.mapWithKey
               (\e _ ->
                  [Nothing, Just (e, Topo.Out),
                   Just (Graph.reverseEdge e, Topo.In)])
               suc)
            (Map.mapWithKey
               (\e _ ->
                  [Nothing, Just (e, Topo.In),
                   Just (Graph.reverseEdge e, Topo.Out)])
               pre)
         ) $
   Graph.graphMap topo


-- * state completion

{-
This algorithm is not optimized.
It is inspired by 'branchAndBound'.
Actually each of our flow state enumeration algorithms
could be turned into a completion algorithm.
The full enumeration could be obtained by completing an empty topology.
-}
complement ::
   (Node.C node, Graph.Edge edge) =>
   CountTopology node ->
   [edge node] ->
   [FlowTopology node]
complement topo freeEdges =
   map removeCounts $
   foldM (flip Count.expand) topo freeEdges

{- |
@identify topo givenEdges@ starts with a flow topology
where the edges of @topo@ are all removed and replaced by @givenEdges@.
Then it computes all ways to fill the missing edges of @topo@
in an admissible way.

It is an checked error if one of the given edges
is not contained in the topology.
-}
identify ::
   (Node.C node) =>
   Topology node -> [Graph.EitherEdge node] -> [FlowTopology node]
identify topo givenEdges =
   let edges = Graph.edges topo
       unDirEdge edge = Graph.unDirEdge (Graph.from edge) (Graph.to edge)
       givenEdgeSet = Set.fromList $ map unDirEdge givenEdges
   in  if Set.isSubsetOf givenEdgeSet
             (Set.fromList $ map unDirEdge edges)
         then
            complement
               (Graph.fromMap (nodeDegrees topo)
                  (Map.fromList $ map (flip (,) ()) givenEdges)) $
            filter (\edge -> not $ Set.member (unDirEdge edge) givenEdgeSet) $
            edges
         else error "StateAnalysis.identify: given edge is not contained in topology"


isSingleton :: [a] -> Bool
isSingleton xs =
   case xs of
      [] -> error "StateAnalysis.minimalGiven: topology can't be reproduced"
      [_] -> True
      _ -> False


reducePattern ::
   (Node.C node) =>
   CountTopology node -> [Graph.EitherEdge node] ->
   [(CountTopology node, [Graph.EitherEdge node])]
reducePattern reducedTopo freeEdges =
   filter (isSingleton . uncurry complement) $
   map (\e -> (Graph.deleteEdge e reducedTopo, e:freeEdges)) $
   Graph.edges reducedTopo

reducePatterns ::
   (Node.C node) =>
   [(CountTopology node, [Graph.EitherEdge node])] ->
   ([[Graph.EitherEdge node]], [(CountTopology node, [Graph.EitherEdge node])])
reducePatterns =
   mapSnd (Map.toList . Map.fromList . concat) .
   ListHT.unzipEithers .
   map
      (\(topo,freeEdges) ->
         let reductions = reducePattern topo freeEdges
         in  if null reductions
               then Left $ Graph.edges topo
               else Right reductions)

{- |
Find all minimal sets of state identifying edges.
For every minimal edge set @es@ it holds

1. @identify topo es@ is a singleton containing the flow topology.

2. @es@ is empty or removing one edge from @es@
   makes @identify topo es@ returning more than one possible topology.


This algorithm basically finds candidate keys.
This is a common problem in relational database theory.
-}
{-
This algorithm is not optimized.
If it is necessary there are certainly many ways to make it more efficient.
-}
minimalGiven ::
   (Node.C node) =>
   FlowTopology node -> [[Graph.EitherEdge node]]
minimalGiven fullTopo =
   concat $ reverse $
   unfoldr
      (\topoEdges ->
         toMaybe (not $ null topoEdges) $ reducePatterns topoEdges)
      [(Graph.fromMap (nodeDegrees fullTopo) $ Graph.edgeLabels fullTopo,
        [])]


{-
topology in building/src/Modules/System causes duplicates:

*Modules.System> Data.Foldable.mapM_ (print . StateAnalysis.minimalGiven) flowStates
-}
{- |
Don't call that function, we only need it for testing.
-}
minimalGivenDuplicate ::
   (Node.C node) =>
   FlowTopology node -> [[Graph.EitherEdge node]]
minimalGivenDuplicate topo =
   let go reducedTopo freeEdges =
          let reduced = reducePattern reducedTopo freeEdges
          in  if null reduced
                then [Graph.edges reducedTopo]
                else concatMap (uncurry go) reduced
   in  go (Graph.fromMap (nodeDegrees topo) $ Graph.edgeLabels topo) []


-- * various algorithms

bruteForce :: (Node.C node) => Topology node -> [FlowTopology node]
bruteForce topo =
   filter admissibleTopology .
   map (replaceEdges topo) $
   mapM Count.edgeOrients $ Graph.edges topo

{-
This algorithm is made after reading R. Birds "Making a Century"
in Pearls of Functional Algorithm Design.
-}
branchAndBound :: (Node.C node) => Topology node -> [FlowTopology node]
branchAndBound topo =
   map removeCounts $
   uncurry (foldM (flip Count.expand)) $
   splitNodesEdges topo

prioritized :: (Node.C node) => Topology node -> [FlowTopology node]
prioritized topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  guard (Count.admissibleTopology cleanTopo)
       >>
       (map (removeCounts . fst) $
        recoursePrioEdge topo $
        (cleanTopo,
         PSQ.fromList $ map (\e -> e PSQ.:-> alternatives e cleanTopo) es))

clusteringGreedy :: (Node.C node) => Topology node -> [FlowTopology node]
clusteringGreedy topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  untilLeft (mergeSmallestClusters cleanTopo) $
       PQ.fromList $
       map (\c -> (length $ clusterEdges c, c)) $
       emptyCluster cleanTopo :
          map (singletonCluster cleanTopo) es

clusteringMinimizing :: (Node.C node) => Topology node -> [FlowTopology node]
clusteringMinimizing topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  untilLeft (mergeMinimizingClusterPairs cleanTopo) $
       emptyCluster cleanTopo !:
          map (singletonCluster cleanTopo) es

clustering :: (Node.C node) => Topology node -> [FlowTopology node]
clustering topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  untilLeft (mergeMinimizingCluster cleanTopo) $
       emptyCluster cleanTopo !:
          map (singletonCluster cleanTopo) es

setCover :: (Node.C node) => Topology node -> [FlowTopology node]
setCover topo =
   map (replaceEdges topo . concat) $
   SetCover.partitions $ setCoverUnDirEdges topo ++ setCoverDirEdges topo


advanced :: (Node.C node) => Topology node -> [FlowTopology node]
advanced = clustering
