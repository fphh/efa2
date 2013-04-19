{-# LANGUAGE TemplateHaskell #-}
module EFA.Test.StateAnalysis where

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified EFA.Graph as Gr
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
      case Gr.nodeSet g of
         ns ->
            map (ArbTopology . flip Gr.delNodeSet g .
                 S.difference ns . S.fromList) $
            QC.shrink $ S.toList ns
   arbitrary = do
      edges <-
         fmap (M.fromList . take maxArbEdges) QC.arbitrary
      nodes <-
         sequenceA $ MapU.fromSet (const QC.arbitrary) $
         Fold.foldMap (Fold.foldMap S.singleton) $
         M.keys edges
      return $ ArbTopology $
         Gr.fromMap nodes $
         M.mapKeys (\(Gr.UnDirEdge x y) -> Gr.DirEdge x y) edges


prop_branchAndBound :: (Eq node, Ord node) => ArbTopology node -> Bool
prop_branchAndBound (ArbTopology g) =
   StateAnalysis.bruteForce g == StateAnalysis.branchAndBound g


{- |
I could declare an Ord instance for Graph,
but I think that @graph0 < graph1@ should be a static error.
Instead I use this function locally for 'Key.sort'.
-}
graphIdent ::
   (Ord node) =>
   Gr.Graph node Gr.DirEdge nodeLabel edgeLabel ->
   (M.Map node nodeLabel,
    M.Map (Gr.DirEdge node) edgeLabel)
graphIdent g = (Gr.nodeLabels g, Gr.edgeLabels g)

{-
I do not convert to Set, but use 'sort' in order to check for duplicates.
-}
prop_prioritized :: (Eq node, Ord node) => ArbTopology node -> Bool
prop_prioritized (ArbTopology g) =
   Key.sort graphIdent (StateAnalysis.branchAndBound g)
   ==
   Key.sort graphIdent (StateAnalysis.prioritized g)

prop_clustering :: (Eq node, Ord node) => ArbTopology node -> Bool
prop_clustering (ArbTopology g) =
   Key.sort graphIdent (StateAnalysis.branchAndBound g)
   ==
   Key.sort graphIdent (StateAnalysis.clustering g)

prop_clusteringGreedy :: (Eq node, Ord node) => ArbTopology node -> Bool
prop_clusteringGreedy (ArbTopology g) =
   Key.sort graphIdent (StateAnalysis.branchAndBound g)
   ==
   Key.sort graphIdent (StateAnalysis.clusteringGreedy g)

prop_clusteringMinimizing :: (Eq node, Ord node) => ArbTopology node -> Bool
prop_clusteringMinimizing (ArbTopology g) =
   Key.sort graphIdent (StateAnalysis.branchAndBound g)
   ==
   Key.sort graphIdent (StateAnalysis.clusteringMinimizing g)


speed_bruteForce :: (Eq node, Ord node) => ArbTopology node -> Bool
speed_bruteForce (ArbTopology g) =
   StateAnalysis.bruteForce g == StateAnalysis.bruteForce g

speed_branchAndBound :: (Eq node, Ord node) => ArbTopology node -> Bool
speed_branchAndBound (ArbTopology g) =
   StateAnalysis.branchAndBound g == StateAnalysis.branchAndBound g

speed_prioritized :: (Eq node, Ord node) => ArbTopology node -> Bool
speed_prioritized (ArbTopology g) =
   StateAnalysis.prioritized g == StateAnalysis.prioritized g

speed_clustering :: (Eq node, Ord node) => ArbTopology node -> Bool
speed_clustering (ArbTopology g) =
   StateAnalysis.clustering g == StateAnalysis.clustering g


runTests :: IO Bool
runTests = $quickCheckAll
