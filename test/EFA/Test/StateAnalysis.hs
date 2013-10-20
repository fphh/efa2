{-# LANGUAGE TemplateHaskell #-}
module EFA.Test.StateAnalysis where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Count as Count
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph; import EFA.Graph (Graph)
import EFA.Graph.Topology (Topology, FlowTopology)

import qualified EFA.Utility.Map as MapU

import qualified Data.List.Key as Key
import qualified Data.List.HT as ListHT
import qualified Data.Foldable as Fold
import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Set as Set
import Data.Traversable (sequenceA)

import qualified Test.QuickCheck as QC
import Test.QuickCheck.All (quickCheckAll)


maxArbEdges :: Int
maxArbEdges = 6

newtype ArbTopology node = ArbTopology (Topology node)
   deriving (Show)

instance (QC.Arbitrary node, Ord node) => QC.Arbitrary (ArbTopology node) where
   shrink (ArbTopology g) =
      case Graph.nodeSet g of
         ns ->
            map (ArbTopology . flip Graph.delNodeSet g .
                 Set.difference ns . Set.fromList) $
            QC.shrink $ Set.toList ns
   arbitrary = do
      edges <-
         fmap
            (Map.fromList . take maxArbEdges . filter (not . Graph.isLoop . fst))
            QC.arbitrary
      nodes <-
         sequenceA $ MapU.fromSet (const QC.arbitrary) $
         Fold.foldMap (Fold.foldMap Set.singleton) $
         Map.keys edges
      return $ ArbTopology $
         Graph.fromMap nodes $
         Map.mapKeys (\(Graph.UnDirEdge x y) -> Graph.DirEdge x y) edges


type Node = Node.Int

prop_branchAndBound :: ArbTopology Node -> Bool
prop_branchAndBound (ArbTopology g) =
   StateAnalysis.bruteForce g == StateAnalysis.branchAndBound g


{- |
I could declare an Ord instance for Graph,
but I think that @graph0 < graph1@ should be a static error.
Instead I use this function locally for 'Key.sort'.
-}
graphIdent ::
   (Ord node) =>
   Graph node Graph.EitherEdge nodeLabel edgeLabel ->
   (Map node nodeLabel,
    Map (Graph.EitherEdge node) edgeLabel)
graphIdent g = (Graph.nodeLabels g, Graph.edgeLabels g)


prop_admissible :: ArbTopology Node -> Bool
prop_admissible (ArbTopology g) =
   Fold.all StateAnalysis.admissibleTopology $
   StateAnalysis.branchAndBound g

prop_checkNode :: Node.Type -> Bool -> Bool -> Bool
prop_checkNode nt sucActive preActive =
   StateAnalysis.checkNodeType nt sucActive preActive
   ==
   Count.checkNodeType nt True sucActive preActive

{-
I do not convert to Set, but use 'sort' in order to check for duplicates.
-}
prop_prioritized :: ArbTopology Node -> Bool
prop_prioritized (ArbTopology g) =
   Key.sort graphIdent (StateAnalysis.branchAndBound g)
   ==
   Key.sort graphIdent (StateAnalysis.prioritized g)

prop_clustering :: ArbTopology Node -> Bool
prop_clustering (ArbTopology g) =
   Key.sort graphIdent (StateAnalysis.branchAndBound g)
   ==
   Key.sort graphIdent (StateAnalysis.clustering g)

prop_clusteringGreedy :: ArbTopology Node -> Bool
prop_clusteringGreedy (ArbTopology g) =
   Key.sort graphIdent (StateAnalysis.branchAndBound g)
   ==
   Key.sort graphIdent (StateAnalysis.clusteringGreedy g)

prop_clusteringMinimizing :: ArbTopology Node -> Bool
prop_clusteringMinimizing (ArbTopology g) =
   Key.sort graphIdent (StateAnalysis.branchAndBound g)
   ==
   Key.sort graphIdent (StateAnalysis.clusteringMinimizing g)

prop_setCover :: ArbTopology Node -> Bool
prop_setCover (ArbTopology g) =
   Key.sort graphIdent (StateAnalysis.branchAndBound g)
   ==
   Key.sort graphIdent (StateAnalysis.setCover g)


speed_bruteForce :: ArbTopology Node -> Bool
speed_bruteForce (ArbTopology g) =
   StateAnalysis.bruteForce g == StateAnalysis.bruteForce g

speed_branchAndBound :: ArbTopology Node -> Bool
speed_branchAndBound (ArbTopology g) =
   StateAnalysis.branchAndBound g == StateAnalysis.branchAndBound g

speed_prioritized :: ArbTopology Node -> Bool
speed_prioritized (ArbTopology g) =
   StateAnalysis.prioritized g == StateAnalysis.prioritized g

speed_clustering :: ArbTopology Node -> Bool
speed_clustering (ArbTopology g) =
   StateAnalysis.clustering g == StateAnalysis.clustering g

speed_setCover :: ArbTopology Node -> Bool
speed_setCover (ArbTopology g) =
   StateAnalysis.setCover g == StateAnalysis.setCover g



newtype ArbFlowTopology node = ArbFlowTopology (FlowTopology node)
   deriving (Show)

instance
   (QC.Arbitrary node, Node.C node) =>
      QC.Arbitrary (ArbFlowTopology node) where
   arbitrary =
      let try = do
             ArbTopology topo <- QC.arbitrary
             let flowTopos = StateAnalysis.advanced topo
             if null flowTopos
               then try
               else fmap ArbFlowTopology $ QC.elements flowTopos
      in  try

prop_identifyProjective :: ArbFlowTopology Node -> Bool
prop_identifyProjective (ArbFlowTopology g) =
   [g] ==
      (StateAnalysis.identify (Topo.plainFromFlow g) $
       Graph.edges g)

prop_identifySelf :: Int -> ArbFlowTopology Node -> Bool
prop_identifySelf n (ArbFlowTopology g) =
   elem g $
   StateAnalysis.identify (Topo.plainFromFlow g) $
   take n $ Graph.edges g

prop_minimalGivenUnique :: ArbFlowTopology Node -> Bool
prop_minimalGivenUnique (ArbFlowTopology g) =
   Fold.and $ Map.fromListWith (\_ _ -> False) $
   map (flip (,) True) $ StateAnalysis.minimalGiven g

prop_minimalGivenDuplicate :: ArbFlowTopology Node -> Bool
prop_minimalGivenDuplicate (ArbFlowTopology g) =
   Set.fromList (StateAnalysis.minimalGiven g)
   ==
   Set.fromList (StateAnalysis.minimalGivenDuplicate g)

prop_minimalGivenIdentifies :: ArbFlowTopology Node -> Bool
prop_minimalGivenIdentifies (ArbFlowTopology g) =
   Fold.all (\es -> StateAnalysis.identify (Topo.plainFromFlow g) es == [g]) $
   StateAnalysis.minimalGiven g

prop_minimalGivenMinimal :: ArbFlowTopology Node -> Bool
prop_minimalGivenMinimal (ArbFlowTopology g) =
   Fold.all
      (Fold.all
         (ListHT.lengthAtLeast 2 .
          StateAnalysis.identify (Topo.plainFromFlow g) . snd) .
       ListHT.removeEach) $
   StateAnalysis.minimalGiven g


runTests :: IO Bool
runTests = $quickCheckAll
