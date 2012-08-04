{-# LANGUAGE BangPatterns, TupleSections #-}

module EFA2.StateAnalysis.StateAnalysis where

-- This algorithm is made after reading R. Birds "Making a Century" in Pearls of Functional Algorithm Design.

-- Could it be made faster using stream fusion?
-- Probably not, because we keep the hole list. No consumer here.
--import Prelude hiding (map, length, filter, concatMap, all, (++), foldr)
--import Data.List.Stream

import qualified Data.List as L
import qualified Data.IntMap as IM
--import qualified Data.Vector as V
--import Data.Function (on)

import EFA2.Topology.Topology
import EFA2.Topology.TopologyData
import EFA2.Utils.Utils

-- import Debug.Trace

-- How should it be orderd to be faster?
checkNodeType :: NodeType -> [ELabel] -> [ELabel] -> Bool
checkNodeType Crossing xsuc xpre = 
  (length xsuc > 0 && length xpre > 0) || all isInactiveEdge (xsuc ++ xpre)
checkNodeType NoRestriction _ _ = True
checkNodeType Source _ [] = True
checkNodeType AlwaysSource (_:_) [] = True
checkNodeType Sink [] _ = True
checkNodeType AlwaysSink [] (_:_) = True
checkNodeType DeadNode [] [] = True
checkNodeType (Storage _) _ _ = True
checkNodeType (InitStorage _) _ _ = error "Not supposed to meet InitStorage in checkNodeType!"
checkNodeType _ _ _ = False

-- Because of extend, we only do have to deal with WithDir edges here!
checkNode :: GraphInfo -> Node -> Topology -> Bool
checkNode gf x topo
  | nadj == length xsuc + length xpre = res
  | otherwise = True
  where res = checkNodeType nty xsuc' xpre'

        xsuc = lsuc topo x
        xsuc' = filter isActiveEdge (map snd xsuc)

        xpre = lpre topo x
        xpre' = filter isActiveEdge (map snd xpre)

        (nty, nadj) = checkJust "checkNode" $ IM.lookup x gf
        --Just (nty, nadj) = gf V.! x  -- Vector Version

ok :: GraphInfo -> LNEdge -> Topology -> Bool
ok gf (x, y, _) t = checkNode gf (fst x) t && checkNode gf (fst y) t

extend :: LNEdge -> [Topology] -> [Topology]
extend ((x, xl), (y, yl), l) [] = [a, b, c]
  where ns = [(x, xl), (y, yl)]
        a = mkGraph ns [(x, y, l { flowDirection = WithDir })]
        b = mkGraph ns [(y, x, l { flowDirection = WithDir })] -- x and y inversed!
        c = mkGraph ns [(x, y, l { flowDirection = UnDir })]
extend ((x, xl), (y, yl), l) gs = concatMap f gs'
  where gs' = map (insNode (y, yl) . insNode (x, xl)) gs
        f g = [a g, b g, c g]
        a g = insEdge (x, y, l { flowDirection = WithDir }) g
        b g = insEdge (y, x, l { flowDirection = WithDir }) g -- x and y inversed!
        c g = insEdge (x, y, l { flowDirection = UnDir }) g

type NumberOfAdj = Int
type GraphInfo = IM.IntMap (NodeType, NumberOfAdj)


{-
-- Surprisingly, the Vector version is not faster than the IntMap version.
-- Does IntMap have constant access times for lookups?

type GraphInfo = V.Vector (Maybe (NodeType, NumberOfAdj))

buildInfo :: Topology -> GraphInfo
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


buildInfo :: Topology -> GraphInfo
buildInfo topo = IM.fromList xs
  where ns = labNodes topo
        xs = map f ns
        f (n, l) = (n, (nodetypeNLabel l, length (pre topo n) + length (suc topo n)))


expand :: GraphInfo -> LNEdge -> [Topology] -> [Topology]
expand gf x = filter (ok gf x) . extend x

solutions :: Topology -> [LNEdge] -> [Topology]
solutions origTopo = foldr (expand gf) []
  where gf = buildInfo origTopo

type LNEdge = (LNode NLabel, LNode NLabel, ELabel)

buildLNEdges :: Topology -> [LNEdge]
buildLNEdges g = lnes
  where les = labEdges g
        lnes = map f les
        f (x, y, l) = ((x, checkJust "buildLNEdges" $ lab g x), (y, checkJust "buildLNEdges" $ lab g y), l)

stateAnalysis :: Topology -> [Topology]
stateAnalysis topo = sol
  where es = buildLNEdges topo
        sol = solutions topo es

-- | Auxiliary function that can be mapped over
-- the results of 'stateAnalysis'. The nodes of the
-- topologies will then be ordered the same way,
-- which looks nice when drawing it with 'drawTopologyXs''
reorderEdges :: Topology -> Topology -- This function should go in an auxiliary module.
reorderEdges topo = mkGraph ns es
  where ns = labNodes topo
        es = map f $ labEdges topo  
        f z@(x, y, l)
          | x < y = z
          | otherwise = (y, x, l { flowDirection = flipFlowDirection (flowDirection l) })
