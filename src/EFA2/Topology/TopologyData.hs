{-# LANGUAGE TupleSections #-}

module EFA2.Topology.TopologyData (
       --EfaGraph,
       NLabel (..),
       ELabel (..),
       NodeType (..),
       FlowDirection (..),
       EdgeType (..),
       Topology,
       unTopology,
       FlowTopology,
       SecTopology,
       isStorage,
       isActiveEdge,
       isInactiveEdge,
       isOtherSection,
       isOriginalEdge,
       isInnerStorageEdge,
       isIntersectionEdge,
       flipFlowDirection,
       getStorageNumber,
       defaultELabel,
       defaultNLabel,
       unlabelEdge,
       fromFlowToSecTopology,
       getActiveStores,
       topoToFlowTopo,
       partitionInOutStatic) where

import qualified EFA2.Signal.Index as Idx
import EFA2.Topology.EfaGraph
          (EfaGraph, InOutGraphFormat, mkInOutGraphFormat, getLEdge)
import Data.Graph.Inductive (Node, LNode, Edge, LEdge)

import qualified Data.Map as M
import qualified Data.List as L
import Data.Ord (comparing)
import Data.Maybe (mapMaybe, fromJust)
import Data.Tuple.HT (thd3)


data NodeType = Storage Int
              | InitStorage Int
              | Sink
              | AlwaysSink
              | Source
              | AlwaysSource
              | Crossing
              | DeadNode
              | NoRestriction deriving (Show, Ord, Eq)

isStorage :: NodeType -> Bool
isStorage (Storage _) = True
isStorage (InitStorage _) = True
isStorage _ = False

getStorageNumber :: NodeType -> Int
getStorageNumber (Storage x) = x
getStorageNumber (InitStorage x) = x
getStorageNumber x = error $ "getStorageNumber: " ++ show x ++ " is not a storage"

data NLabel = NLabel { sectionNLabel :: Idx.Section,
                       nodeNLabel :: Int,
                       nodetypeNLabel :: NodeType } deriving (Show, Eq, Ord)

defaultNLabel :: NLabel
defaultNLabel = NLabel (Idx.Section 0) 0 NoRestriction


data FlowDirection = WithDir
                   | AgainstDir
                   | UnDir deriving (Show, Eq, Ord)

isOtherSection :: LNode NLabel -> LNode NLabel -> Bool
isOtherSection (_, l1) (_, l2) = sectionNLabel l1 /= sectionNLabel l2

isInactive :: FlowDirection -> Bool
isInactive UnDir = True
isInactive _ = False

isActive :: FlowDirection -> Bool
isActive = not . isInactive

data EdgeType = OriginalEdge
              | InnerStorageEdge
              | IntersectionEdge deriving (Eq, Ord, Show)



data ELabel = ELabel { edgeType :: EdgeType,
                       flowDirection :: FlowDirection } deriving (Eq, Ord, Show)

defaultELabel :: ELabel
defaultELabel = ELabel OriginalEdge WithDir

isActiveEdge :: ELabel -> Bool
isActiveEdge = isActive . flowDirection

isInactiveEdge :: ELabel -> Bool
isInactiveEdge = isInactive . flowDirection


isOriginalEdge :: ELabel -> Bool
isOriginalEdge = (OriginalEdge ==) . edgeType

isInnerStorageEdge :: ELabel -> Bool
isInnerStorageEdge = (InnerStorageEdge ==) . edgeType

isIntersectionEdge :: ELabel -> Bool
isIntersectionEdge = (IntersectionEdge ==) . edgeType

flipFlowDirection :: FlowDirection -> FlowDirection
flipFlowDirection WithDir = AgainstDir
flipFlowDirection AgainstDir = WithDir
flipFlowDirection UnDir = UnDir



unlabelEdge :: LEdge a -> Edge
unlabelEdge (x, y, _) = (x, y)

unTopology :: Topology -> EfaGraph Node NLabel ELabel
unTopology = id

type Topology = EfaGraph Node NLabel ELabel

topoToFlowTopo :: Topology -> FlowTopology
topoToFlowTopo = id

type FlowTopology = EfaGraph Node NLabel ELabel

type SecTopology = EfaGraph Node NLabel ELabel


fromFlowToSecTopology :: FlowTopology -> SecTopology
fromFlowToSecTopology = id

-- | Active storages, grouped by storage number, sorted by section number.
getActiveStores :: Topology -> [[InOutGraphFormat (LNode NLabel)]]
getActiveStores topo = map (sectionSort . filter (isActiveSt topo)) groupedIof
  where groupedIof =
           M.elems $ M.fromListWith (++) $
           mapMaybe (\n -> fmap (,[n]) $ stNum n) $
           mkInOutGraphFormat id topo
        stNum (_, (_, l), _) =
           case nodetypeNLabel l of
              Storage x -> Just x
              InitStorage x -> Just x
              _ -> Nothing
        sectionSort = L.sortBy (comparing sec)
        sec (_, (_, l), _) = sectionNLabel l

isActiveSt :: Topology -> InOutGraphFormat (LNode NLabel) -> Bool
isActiveSt topo (ins, (nid, _), outs) = res
  where inEs = map ((,nid) . fst) ins
        outEs = map ((nid,) . fst) outs
        es = mapMaybe (uncurry (getLEdge topo)) (inEs ++ outEs)
        res = any isActiveEdge (map thd3 es)

-- | Partition the storages in in and out storages, looking only at edges, not at values.
-- This means that nodes with in AND out edges cannot be treated.
partitionInOutStatic ::
  Topology -> [InOutGraphFormat (LNode NLabel)] -> ([InOutGraphFormat (LNode NLabel)], [InOutGraphFormat (LNode NLabel)])
partitionInOutStatic topo iof = L.partition p iof
  where p (ins, (nid, _), outs)  =  null (filter q ins) /= null (filter r outs)
          where q (n, _) = flowDirection e == WithDir && (isOriginalEdge e || isInnerStorageEdge e)
                  where e = thd3 $ fromJust (getLEdge topo n nid)
                r (n, _) = flowDirection e == AgainstDir && (isOriginalEdge e || isInnerStorageEdge e)
                  where e = thd3 $ fromJust (getLEdge topo nid n)
