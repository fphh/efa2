{-# LANGUAGE TupleSections #-}

module EFA2.Topology.TopologyData (
       --EfaGraph,
       NLabel (..),
       ELabel (..),
       NodeType (..),
       FlowDirection (..),
       EdgeType (..),
       Topology,
       Topology' (..),
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
import EFA2.Topology.EfaGraph (EfaGraph)
import EFA2.Utils.Graph (InOutGraphFormat, getLEdge, mkInOutGraphFormat)

import qualified Data.Map as M
import qualified Data.List as L
import Data.Graph.Inductive
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


newtype Topology' a b = Topology { unTopology :: EfaGraph Node a b } deriving (Show)
type Topology = Topology' NLabel ELabel

instance Graph Topology' where
         empty = Topology empty
         isEmpty (Topology topo) = isEmpty topo
         match n (Topology topo) = (mcont, Topology g)
           where (mcont, g) = match n topo
         mkGraph ns es = Topology (mkGraph ns es)
         labNodes (Topology topo) = labNodes topo
         labEdges (Topology topo) = labEdges topo

instance DynGraph Topology' where
         cont & (Topology topo) = Topology (cont & topo)

-- | 
newtype FlowTopology' a b = FlowTopology ( EfaGraph Node a b ) deriving (Show)
type FlowTopology = FlowTopology' NLabel ELabel

topoToFlowTopo :: Topology -> FlowTopology
topoToFlowTopo = FlowTopology . unTopology

instance Graph FlowTopology' where
         empty = FlowTopology empty
         isEmpty (FlowTopology topo) = isEmpty topo
         match n (FlowTopology topo) = (mcont, FlowTopology g)
           where (mcont, g) = match n topo
         mkGraph ns es = FlowTopology (mkGraph ns es)
         labNodes (FlowTopology topo) = labNodes topo
         labEdges (FlowTopology topo) = labEdges topo

instance DynGraph FlowTopology' where
         cont & (FlowTopology topo) = FlowTopology (cont & topo)

-- | 
newtype SecTopology' a b = SecTopology ( EfaGraph Node a b ) deriving (Show)
type SecTopology = SecTopology' NLabel ELabel

instance Graph SecTopology' where
         empty = SecTopology empty
         isEmpty (SecTopology topo) = isEmpty topo
         match n (SecTopology topo) = (mcont, SecTopology g)
           where (mcont, g) = match n topo
         mkGraph ns es = SecTopology (mkGraph ns es)
         labNodes (SecTopology topo) = labNodes topo
         labEdges (SecTopology topo) = labEdges topo

instance DynGraph SecTopology' where
         cont & (SecTopology topo) = SecTopology (cont & topo)


fromFlowToSecTopology :: FlowTopology -> SecTopology
fromFlowToSecTopology (FlowTopology topo) = SecTopology topo

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
