module EFA.Graph.Topology.StateAnalysis (
   advanced,
   bruteForce,
   branchAndBound,
   prioritized,
   clustering, clusteringGreedy, clusteringMinimizing,
   setCover,
   ) where

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph; import EFA.Graph (Graph)
import EFA.Graph.Topology (FlowTopology, Topology)

import qualified EFA.Utility.Map as MapU

import qualified Math.SetCover.Exact as SetCover

import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.Set as Set; import Data.Set (Set)
import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.NonEmpty as NonEmpty
import qualified Data.FingerTree.PSQueue as PSQ
import qualified Data.PriorityQueue.FingerTree as PQ
import Data.FingerTree.PSQueue (PSQ)
import Data.PriorityQueue.FingerTree (PQueue)
import Data.NonEmpty ((!:))
import Control.Monad (foldM, guard)
import Control.Functor.HT (void)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)


type NodeType = Node.Type ()

-- How should it be ordered to be faster?
checkNodeType :: NodeType -> Bool -> Bool -> Bool
checkNodeType Node.Crossing sucActive preActive = sucActive == preActive
checkNodeType Node.NoRestriction _ _ = True
checkNodeType Node.Source _ False = True
checkNodeType Node.AlwaysSource True False = True
checkNodeType Node.Sink False _ = True
checkNodeType Node.AlwaysSink False True = True
checkNodeType Node.DeadNode False False = True
checkNodeType (Node.Storage _) _ _ = True
checkNodeType _ _ _ = False


type InOut node nodeLabel =
        (Map (Graph.EitherEdge node) (),
         nodeLabel,
         Map (Graph.EitherEdge node) ())

-- Because of extend, we only have to deal with Dir edges here!
checkInOut ::
   (Ord node) =>
   InOut node (Node.Type ()) -> Bool
checkInOut (pre, node, suc) =
   checkNodeType node
      (anyActive suc)
      (anyActive pre)


infix 1 `implies`

implies :: Bool -> Bool -> Bool
implies x y = not x || y

checkIncompleteNodeType :: NodeType -> Bool -> Bool -> Bool -> Bool
checkIncompleteNodeType typ complete sucActive preActive =
   case typ of
      Node.Crossing -> complete `implies` sucActive == preActive
      Node.Source -> not preActive
      Node.AlwaysSource -> not preActive && (complete `implies` sucActive)
      Node.Sink -> not sucActive
      Node.AlwaysSink -> not sucActive && (complete `implies` preActive)
      Node.Storage _ -> True
      Node.NoRestriction -> True
      Node.DeadNode -> not sucActive && not preActive

checkCountInOut ::
   (Ord node) =>
   InOut node (NodeType, NumberOfAdj) -> Bool
checkCountInOut (pre, (node, nadj), suc) =
   checkIncompleteNodeType node
      (Map.size pre + Map.size suc == nadj)
      (anyActive suc)
      (anyActive pre)

checkCountNode :: (Ord node) => CountTopology node -> node -> Bool
checkCountNode topo x =
   case Map.lookup x $ Graph.graphMap topo of
      Nothing -> error "checkCountNode: node not in graph"
      Just inOut -> checkCountInOut inOut

anyActive :: Map (Graph.EitherEdge node) () -> Bool
anyActive = Fold.any Topo.isActive . Map.keysSet

admissibleCountTopology :: (Ord node) => CountTopology node -> Bool
admissibleCountTopology topo =
   Fold.all checkCountInOut $ Graph.graphMap topo


type NumberOfAdj = Int
type CountTopology node =
        Graph node Graph.EitherEdge (NodeType, NumberOfAdj) ()

insEdge ::
   Ord node =>
   Graph.EitherEdge node -> CountTopology node -> CountTopology node
insEdge e = Graph.insEdge (e, ())

insEdgeSet ::
   Ord node =>
   Set (Graph.EitherEdge node) -> CountTopology node -> CountTopology node
insEdgeSet e = Graph.insEdgeSet (MapU.fromSet (const ()) e)

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


edgeOrients :: Ord node => Graph.DirEdge node -> [Graph.EitherEdge node]
edgeOrients (Graph.DirEdge x y) =
   (Graph.EDirEdge $ Graph.DirEdge x y) :
   (Graph.EDirEdge $ Graph.DirEdge y x) : -- x and y swapped!
   (Graph.EUnDirEdge $ Graph.unDirEdge x y) :
   []

admissibleEdges ::
   (Ord node) =>
   LNEdge node -> CountTopology node ->
   [(Graph.EitherEdge node, CountTopology node)]
admissibleEdges e0 g0 = do
   e1 <- edgeOrients e0
   let g1 = insEdge e1 g0
   guard $ Fold.all (checkCountNode g1) e0
   return (e1, g1)

expand ::
   (Ord node) =>
   LNEdge node -> CountTopology node -> [CountTopology node]
expand e g = map snd $ admissibleEdges e g

splitNodesEdges ::
   (Ord node) =>
   Topology node -> (CountTopology node, [Graph.DirEdge node])
splitNodesEdges topo =
   (Graph.fromMap
       (Map.map (\(pre,l,suc) -> (l, Set.size pre + Set.size suc)) $ Graph.nodes topo)
       Map.empty,
    Graph.edges topo)


newtype
   Alternatives node =
      Alternatives {getAlternatives :: [Graph.EitherEdge node]}

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
                    Fold.foldMap (Graph.adjEdges origTopo) bestEdge)
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
   (Ord node) =>
   CountTopology node -> Cluster node
emptyCluster g =
   Cluster Set.empty
      (guard (admissibleCountTopology g) >> [Set.empty])

singletonCluster ::
   (Ord node) =>
   CountTopology node -> Graph.DirEdge node -> Cluster node
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
               map (\es -> Graph.mapNode fst $ insEdgeSet es topo) $
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
   (Ord node) =>
   CountTopology node ->
   NonEmpty.T [] (Cluster node) ->
   Either [FlowTopology node] (NonEmpty.T [] (Cluster node))
mergeMinimizingClusterPairs topo (NonEmpty.Cons p ps) =
   case NonEmpty.fetch ps of
      Nothing ->
         Left $
         map (\es -> Graph.mapNode fst $ insEdgeSet es topo) $
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
   (Ord node) =>
   CountTopology node ->
   NonEmpty.T [] (Cluster node) ->
   Either [FlowTopology node] (NonEmpty.T [] (Cluster node))
mergeMinimizingCluster topo (NonEmpty.Cons p ps) =
   case NonEmpty.fetch ps of
      Nothing ->
         Left $
         map (\es -> Graph.mapNode fst $ insEdgeSet es topo) $
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


type LNEdge node = Graph.DirEdge node


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
   (Ord node) =>
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
      (\node (pre, nt, suc) ->
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
               checkNodeType nt
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



-- * various algorithms

bruteForce :: (Ord node) => Topology node -> [FlowTopology node]
bruteForce topo =
   filter (Fold.all checkInOut . Graph.graphMap) .
   map (replaceEdges topo) $
   mapM edgeOrients $ Graph.edges topo

{-
This algorithm is made after reading R. Birds "Making a Century"
in Pearls of Functional Algorithm Design.
-}
branchAndBound :: (Ord node) => Topology node -> [FlowTopology node]
branchAndBound topo =
   map (Graph.mapNode fst) $
   uncurry (foldM (flip expand)) $
   splitNodesEdges topo

prioritized :: (Ord node) => Topology node -> [FlowTopology node]
prioritized topo =
   let (cleanTopo, es) = splitNodesEdges topo
   in  guard (admissibleCountTopology cleanTopo)
       >>
       (map (Graph.mapNode fst . fst) $
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

setCover :: (Ord node) => Topology node -> [FlowTopology node]
setCover topo =
   map (replaceEdges topo . concat) $
   SetCover.partitions $ setCoverUnDirEdges topo ++ setCoverDirEdges topo


advanced :: (Ord node) => Topology node -> [FlowTopology node]
advanced = clustering
