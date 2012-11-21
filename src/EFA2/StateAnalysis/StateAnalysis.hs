module EFA2.StateAnalysis.StateAnalysis (
   advanced,
   bruteForce,
   propAdvanced,
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

import qualified Data.Foldable as Fold
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Traversable (sequenceA)
import Data.Monoid (mappend)
import Control.Monad (liftM2, foldM, guard)

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

checkCountNode :: CountTopology -> Idx.Node -> Bool
checkCountNode topo x =
   case M.lookup x $ Gr.nodes topo of
      Nothing -> error "checkNode: node not in graph"
      Just (pre, (nty, nadj), suc) ->
         (S.size pre + S.size suc < nadj) ||
         checkNodeType nty
            (anyActive $ Gr.sucEdgeLabels topo x suc)
            (anyActive $ Gr.preEdgeLabels topo x pre)

anyActive :: [(n, FlowDirection)] -> Bool
anyActive = any (isActive . snd)

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

expand :: LNEdge -> CountTopology -> [CountTopology]
expand e g0 = do
   g1 <- map (flip Gr.insEdge g0) $ edgeOrients e
   guard $ Fold.all (checkCountNode g1) e
   return g1

nodesOnly :: Topology -> CountTopology
nodesOnly topo =
   Gr.mkGraphFromMap
      (M.map (\(pre,l,suc) -> (l, S.size pre + S.size suc)) $ Gr.nodes topo)
      M.empty

type LNEdge = Gr.Edge Idx.Node

bruteForce :: Topology -> [FlowTopology]
bruteForce topo =
   filter (\g -> Fold.all (checkNode g) $ Gr.nodeSet g) .
   map (Gr.mkGraphFromMap (Gr.nodeLabels topo) . M.fromList) $
   mapM (edgeOrients . fst) $ Gr.labEdges topo

advanced :: Topology -> [FlowTopology]
advanced topo =
   map (Gr.nmap fst) $
   foldM (flip expand) (nodesOnly topo) $
   map fst $ Gr.labEdges topo


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
         Gr.mkGraphFromMap nodes $
         M.mapKeys (\(UndirEdge x y) -> Gr.Edge x y) edges

propAdvanced :: ArbTopology -> Bool
propAdvanced (ArbTopology g) =
   bruteForce g == advanced g
