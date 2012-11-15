module EFA2.StateAnalysis.StateAnalysis where

-- This algorithm is made after reading R. Birds "Making a Century" in Pearls of Functional Algorithm Design.

-- Could it be made faster using stream fusion?
-- Probably not, because we keep the hole list. No consumer here.
--import Prelude hiding (map, length, filter, concatMap, all, (++), foldr)
--import Data.List.Stream

import qualified Data.Map as M
--import qualified Data.Vector as V
--import Data.Function (on)

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Topology.EfaGraph as Gr
import EFA2.Topology.TopologyData
          (FlowTopology, NodeType(..),
           FlowDirection, isActive, isInactive,
           FlowDirection(UnDir, WithDir), flipFlowDirection)
import EFA2.Topology.EfaGraph
          (Edge(Edge), LNode, lab, labNodes, labEdges,
           insNode, insEdge, pre, suc, lpre, lsuc, mkGraph)
import EFA2.Utils.Utils (checkJust)


-- import Debug.Trace

-- How should it be orderd to be faster?
checkNodeType :: NodeType -> [FlowDirection] -> [FlowDirection] -> Bool
checkNodeType Crossing xsuc xpre =
   (not (null xsuc) && not (null xpre)) || all isInactive (xsuc ++ xpre)
checkNodeType NoRestriction _ _ = True
checkNodeType Source _ [] = True
checkNodeType AlwaysSource (_:_) [] = True
checkNodeType Sink [] _ = True
checkNodeType AlwaysSink [] (_:_) = True
checkNodeType DeadNode [] [] = True
checkNodeType (Storage _) _ _ = True
checkNodeType _ _ _ = False

-- Because of extend, we only do have to deal with WithDir edges here!
checkNode :: GraphInfo -> Idx.Node -> FlowTopology -> Bool
checkNode gf x topo =
    (nadj /= length xsuc + length xpre) || res
  where res = checkNodeType nty xsuc' xpre'

        xsuc = lsuc topo x
        xsuc' = filter isActive (map snd xsuc)

        xpre = lpre topo x
        xpre' = filter isActive (map snd xpre)

        (nty, nadj) = checkJust "checkNode" $ M.lookup x gf
        --Just (nty, nadj) = gf V.! x  -- Vector Version

ok :: GraphInfo -> LNEdge -> FlowTopology -> Bool
ok gf (x, y) t = checkNode gf (fst x) t && checkNode gf (fst y) t

extend :: LNEdge -> [FlowTopology] -> [FlowTopology]
extend ((x, xl), (y, yl)) [] = [a, b, c]
  where ns = [(x, xl), (y, yl)]
        a = mkGraph ns [(Edge x y, WithDir)]
        b = mkGraph ns [(Edge y x, WithDir)] -- x and y inversed!
        c = mkGraph ns [(Edge x y, UnDir)]
extend ((x, xl), (y, yl)) gs = concatMap f gs'
  where gs' = map (insNode (y, yl) . insNode (x, xl)) gs
        f g = [a g, b g, c g]
        a g = insEdge (Edge x y, WithDir) g
        b g = insEdge (Edge y x, WithDir) g -- x and y inversed!
        c g = insEdge (Edge x y, UnDir) g

type NumberOfAdj = Int
type GraphInfo = M.Map Idx.Node (NodeType, NumberOfAdj)


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


buildInfo :: FlowTopology -> GraphInfo
buildInfo topo = M.mapWithKey f $ Gr.nodeLabels topo
  where f n l = (l, length (pre topo n) + length (suc topo n))


expand :: GraphInfo -> LNEdge -> [FlowTopology] -> [FlowTopology]
expand gf x = filter (ok gf x) . extend x

solutions :: FlowTopology -> [LNEdge] -> [FlowTopology]
solutions origTopo = foldr (expand gf) []
  where gf = buildInfo origTopo

type LNEdge = (LNode Idx.Node NodeType, LNode Idx.Node NodeType)

buildLNEdges :: FlowTopology -> [LNEdge]
buildLNEdges g = map f $ M.keys $ Gr.edgeLabels g
  where f (Edge x y) =
           ((x, checkJust "buildLNEdges" $ lab g x),
            (y, checkJust "buildLNEdges" $ lab g y))

stateAnalysis :: FlowTopology -> [FlowTopology]
stateAnalysis topo = solutions topo $ buildLNEdges topo

-- | Auxiliary function that can be mapped over
-- the results of 'stateAnalysis'. The nodes of the
-- topologies will then be ordered the same way,
-- which looks nice when drawing it with 'drawTopologyXs''
reorderEdges :: FlowTopology -> FlowTopology -- This function should go in an auxiliary module.
reorderEdges topo = mkGraph ns es
  where ns = labNodes topo
        es = map f $ labEdges topo
        f z@(Edge x y, l)
          | x < y = z
          | otherwise = (Edge y x, flipFlowDirection l)
