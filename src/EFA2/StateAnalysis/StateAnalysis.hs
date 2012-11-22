module EFA2.StateAnalysis.StateAnalysis (
   advanced,
   bruteForce,
   branchAndBound,
   prioritized,
   propBranchAndBound,
   propPrioritized,
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
import Data.FingerTree.PSQueue (PSQ)
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
   Gr.fromMap
      (M.map (\(pre,l,suc) -> (l, S.size pre + S.size suc)) $ Gr.nodes topo)
      M.empty

recoursePrioEdge ::
   Topology ->
   (CountTopology, PSQ LNEdge Int) ->
   [(CountTopology, PSQ LNEdge Int)]
recoursePrioEdge origTopo =
   let recourse tq@(topo, queue) =
          case PSQ.minView queue of
             Nothing -> [tq]
             Just (bestEdge PSQ.:-> _p, remQueue) -> do
                newTopo <- expand bestEdge topo
                recourse
                   (newTopo,
                    Fold.foldl (\q e -> PSQ.adjust (const $ length $ expand e newTopo) e q) remQueue $
                    Fold.foldMap (Gr.adjEdges origTopo) bestEdge)
   in  recourse


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
   foldM (flip expand) (nodesOnly topo) $
   map fst $ Gr.labEdges topo

prioritized :: Topology -> [FlowTopology]
prioritized topo =
   let cleanTopo = nodesOnly topo
   in  guard (Fold.all (checkCountNode cleanTopo) $ Gr.nodeSet cleanTopo)
       >>
       (map (Gr.nmap fst . fst) $
        recoursePrioEdge topo $
        (cleanTopo,
         PSQ.fromList $ map (uncurry (PSQ.:->)) $ M.toList $
         M.mapWithKey (\e _ -> length $ expand e cleanTopo) $
         Gr.edgeLabels topo))

advanced :: Topology -> [FlowTopology]
advanced = prioritized


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
   Key.sort graphIdent (bruteForce g)
   ==
   Key.sort graphIdent (prioritized g)
