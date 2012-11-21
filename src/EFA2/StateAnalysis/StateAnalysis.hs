module EFA2.StateAnalysis.StateAnalysis where

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
          (FlowTopology, NodeType(..),
           FlowDirection(UnDir, Dir), isActive, isInactive)
import EFA2.Topology.EfaGraph (lpre, lsuc)
import EFA2.Utils.Utils (checkJust)

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad (foldM)


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
checkNodeType Storage _ _ = True
checkNodeType _ _ _ = False

-- Because of extend, we only do have to deal with Dir edges here!
checkNode :: Idx.Node -> CountTopology -> Bool
checkNode x topo =
    (nadj /= length xsuc + length xpre) || res
  where res = checkNodeType nty xsuc' xpre'

        xsuc = lsuc topo x
        xsuc' = filter isActive (map snd xsuc)

        xpre = lpre topo x
        xpre' = filter isActive (map snd xpre)

        (nty, nadj) = checkJust "checkNode" $ Gr.lab topo x
        --Just (nty, nadj) = gf V.! x  -- Vector Version

ok :: LNEdge -> CountTopology -> Bool
ok (Gr.Edge x y) t = checkNode x t && checkNode y t



edgeOrients :: LNEdge -> [Gr.LEdge Idx.Node FlowDirection]
edgeOrients (Gr.Edge x y) =
   (Gr.Edge x y, Dir) :
   (Gr.Edge y x, Dir) : -- x and y inversed!
   (Gr.Edge x y, UnDir) :
   []

extend :: LNEdge -> CountTopology -> [CountTopology]
extend e g =
   map (flip Gr.insEdge g) $ edgeOrients e

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


expand :: LNEdge -> CountTopology -> [CountTopology]
expand x = filter (ok x) . extend x

solutions :: CountTopology -> [LNEdge] -> [CountTopology]
solutions = foldM (flip expand)

type LNEdge = Gr.Edge Idx.Node

stateAnalysis :: FlowTopology -> [FlowTopology]
stateAnalysis topo =
   map (Gr.nmap fst) $
   solutions
      (Gr.mkGraphFromMap
         (M.map (\(pre,l,suc) -> (l, S.size pre + S.size suc)) $ Gr.nodes topo)
         M.empty) $
   map fst $ Gr.labEdges topo
