{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Data.Graph.Inductive


import EFA2.Topology.Topology
import EFA2.Topology.TopologyData
import EFA2.Utils.Utils
import EFA2.Display.DrawGraph

import Debug.Trace

{-
topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, NoRestriction), (1, NoRestriction), (2, Crossing), (3, NoRestriction)]
        edges = [(0, 2, defaultELabel), (1, 2, defaultELabel), (2, 3, defaultELabel)]

-}
topo :: Topology
topo = mkGraph (makeNodes nodes) (makeEdges edges)
  where nodes = [(0, NoRestriction), (1, Crossing), (2, Crossing), (3, Crossing), (4, NoRestriction), (5, NoRestriction),
                 (6, NoRestriction), (7, Crossing), (8, Crossing), (9, Crossing), (10, NoRestriction), (11, NoRestriction) ]
        edges = [(0, 1, defaultELabel), (1, 2, defaultELabel), (1, 3, defaultELabel),
                 (2, 3, defaultELabel), (2, 4, defaultELabel), (3, 5, defaultELabel),
                 (6, 7, defaultELabel), (7, 8, defaultELabel), (7, 9, defaultELabel),
                 (8, 9, defaultELabel), (8, 10, defaultELabel), (9, 11, defaultELabel) ]


solutions :: Topology -> [LNEdge] -> [Topology]
solutions origTopo = map fst . filter (good . snd) . foldr (expand gf) []
  where gf = buildInfo origTopo

expand :: GraphInfo -> LNEdge -> [(Topology, InsertInfo)] -> [(Topology, InsertInfo)]
expand gf x = filter (ok gf) . uncurry zip . cross (extend x, modify x) . unzip


type NumberOfAdj = Int
type GraphInfo = IM.IntMap (NodeType, NumberOfAdj)

buildInfo :: Topology -> GraphInfo
buildInfo topo = IM.fromList xs
  where ns = labNodes topo
        xs = map f ns
        f (n, l) = (n, (nodetypeNLabel l, length (pre topo n) + length (suc topo n)))

cross :: (a -> x, b -> y) -> (a, b) -> (x, y)
cross (f, g) (x, y) = (f x, g y)

good :: a -> Bool
good x = True

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


ok :: GraphInfo -> (Topology, InsertInfo) -> Bool
ok gf (t, InsertInfo x y) = checkNode gf x t && checkNode gf y t


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


modify :: LNEdge -> [InsertInfo] -> [InsertInfo]
modify (x, y, _) xs
  | null xs = [z, z, z]
  | otherwise = replicate (3 * length xs) z
  where z = InsertInfo (fst x) (fst y)

type LNEdge = (LNode NLabel, LNode NLabel, ELabel)

buildLNEdges :: Topology -> [LNEdge]
buildLNEdges g = lnes
  where les = labEdges topo
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
  let es = buildLNEdges topo
      sol = solutions topo es
  --print (length sol)
  --drawTopologyXs' (map reorderEdges sol)
  print (length sol)

