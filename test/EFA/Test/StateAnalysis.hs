{-# LANGUAGE TemplateHaskell #-}
module EFA.Test.StateAnalysis where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Graph as Graph; import EFA.Graph (Graph)
import EFA.Graph.Topology (Topology)
import qualified EFA.Utility.Map as MapU

import qualified Data.List.Key as Key
import qualified Data.Foldable as Fold
import qualified Data.Map as M
import qualified Data.Set as S
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
                 S.difference ns . S.fromList) $
            QC.shrink $ S.toList ns
   arbitrary = do
      edges <-
         fmap
            (M.fromList . take maxArbEdges . filter (not . Graph.isLoop . fst))
            QC.arbitrary
      nodes <-
         sequenceA $ MapU.fromSet (const QC.arbitrary) $
         Fold.foldMap (Fold.foldMap S.singleton) $
         M.keys edges
      return $ ArbTopology $
         Graph.fromMap nodes $
         M.mapKeys (\(Graph.UnDirEdge x y) -> Graph.DirEdge x y) edges


type Node = Int

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
   (M.Map node nodeLabel,
    M.Map (Graph.EitherEdge node) edgeLabel)
graphIdent g = (Graph.nodeLabels g, Graph.edgeLabels g)

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


runTests :: IO Bool
runTests = $quickCheckAll
