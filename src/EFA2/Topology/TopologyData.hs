{-# LANGUAGE TupleSections #-}

module EFA2.Topology.TopologyData (
       --EfaGraph,
       NLabel (..), LNode,
       ELabel (..), LEdge,
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
import qualified EFA2.Topology.EfaGraph as Gr
import EFA2.Topology.EfaGraph (EfaGraph, mkInOutGraphFormat)
import Data.Graph.Inductive (Node)

import qualified Data.Map as M
import qualified Data.List as L
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)


type LNode = Gr.LNode Node NLabel
type LEdge = Gr.LEdge Node ELabel

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

isOtherSection :: LNode -> LNode -> Bool
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



unlabelEdge :: LEdge -> Gr.Edge Node
unlabelEdge = fst

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
getActiveStores :: Topology -> [[Gr.InOut Node NLabel ELabel]]
getActiveStores topo = map (sectionSort . filter isActiveSt) groupedIof
  where groupedIof =
           M.elems $ M.fromListWith (++) $
           mapMaybe (\n -> fmap (,[n]) $ stNum n) $
           mkInOutGraphFormat topo
        stNum (_, (_, l), _) =
           case nodetypeNLabel l of
              Storage x -> Just x
              InitStorage x -> Just x
              _ -> Nothing
        sectionSort = L.sortBy (comparing sec)
        sec (_, (_, l), _) = sectionNLabel l

isActiveSt :: Gr.InOut n nl ELabel -> Bool
isActiveSt (ins, _, outs) =
   any isActiveEdge $ map snd $ ins ++ outs

-- | Partition the storages in in and out storages, looking only at edges, not at values.
-- This means that nodes with in AND out edges cannot be treated.
partitionInOutStatic ::
  [Gr.InOut n nl ELabel] ->
  ([Gr.InOut n nl ELabel], [Gr.InOut n nl ELabel])
partitionInOutStatic = L.partition p
  where p (ins, _, outs)  =  null (filter q ins) /= null (filter r outs)
          where q (_, e) = flowDirection e == WithDir && (isOriginalEdge e || isInnerStorageEdge e)
                r (_, e) = flowDirection e == AgainstDir && (isOriginalEdge e || isInnerStorageEdge e)
