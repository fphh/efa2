{-# LANGUAGE TupleSections #-}

module Main where

import Prelude hiding (map, length, filter, concatMap, all, (++), foldr)

import Data.List.Stream
--import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Graph.Inductive


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData
import EFA2.Utils.Utils
import EFA2.Display.DrawGraph

import Debug.Trace


topoDreibein :: Topology
topoDreibein = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, NoRestriction), (1, NoRestriction), (2, Crossing), (3, NoRestriction)]
        edges = [(0, 2, defaultELabel), (1, 2, defaultELabel), (2, 3, defaultELabel)]


topoLoop :: Topology
topoLoop = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, NoRestriction), (1, Crossing), (2, Crossing), (3, Crossing), (4, NoRestriction), (5, NoRestriction) ]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel),
                 (2, 3, defaultELabel), (2, 4, defaultELabel), (3, 5, defaultELabel) ]

topoDoubleLoop :: Topology
topoDoubleLoop = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, NoRestriction), (1, Crossing), (2, Crossing), (3, Crossing), (4, NoRestriction), (5, NoRestriction),
                 (6, NoRestriction), (7, Crossing), (8, Crossing), (9, Crossing), (10, NoRestriction), (11, NoRestriction) ]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel),
                 (2, 3, defaultELabel), (2, 4, defaultELabel), (3, 5, defaultELabel),
                 (6, 7, defaultELabel), (7, 8, defaultELabel), (7, 9, defaultELabel),
                 (8, 9, defaultELabel), (8, 10, defaultELabel), (9, 11, defaultELabel) ]


solutions :: Topology -> [LNEdge] -> [Topology]
solutions origTopo = foldr (expand gf) []
  where gf = buildInfo origTopo

expand :: GraphInfo -> LNEdge -> [Topology] -> [Topology]
expand gf x = filter (ok gf x) . extend x

type NumberOfAdj = Int
type GraphInfo = IM.IntMap (NodeType, NumberOfAdj)

buildInfo :: Topology -> GraphInfo
buildInfo topo = IM.fromList xs
  where ns = labNodes topo
        xs = map f ns
        f (n, l) = (n, (nodetypeNLabel l, length (pre topo n) + length (suc topo n)))

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

checkNode :: GraphInfo -> Node -> Topology -> Bool
checkNode gf x topo
  | nadj == length xsuc + length xpre = {- trace (show x ++ show xsuc' ++ show xpre' ++ show res) -} res
  | otherwise = True
  where res = checkNodeType nty xsuc' xpre'

        xsuc = lsuc topo x
        xsuc' = filter isActiveEdge (map snd xsuc)

        xpre = lpre topo x
        xpre' = filter isActiveEdge (map snd xpre)

        (nty, nadj) = checkJust "checkNode" $ IM.lookup x gf

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

data InsertInfo = InsertInfo Node Node deriving (Show)

type LNEdge = (LNode NLabel, LNode NLabel, ELabel)

buildLNEdges :: Topology -> [LNEdge]
buildLNEdges g = lnes
  where les = labEdges g
        lnes = map f les
        f (x, y, l) = ((x, checkJust "buildLNEdges" $ lab g x), (y, checkJust "buildLNEdges" $ lab g y), l)

reorderEdges :: Topology -> Topology
reorderEdges topo = mkGraph ns es
  where ns = labNodes topo
        es = map f $ labEdges topo  
        f z@(x, y, l)
          | x < y = z
          | otherwise = (y, x, l { flowDirection = flipFlowDirection (flowDirection l) })

main :: IO ()
main = do
  let topo = topoDoubleLoop
      es = buildLNEdges topo
      sol = solutions topo es
  --print (length sol)
  --drawTopologyXs' (map reorderEdges sol)
  print sol
  print (length sol)

