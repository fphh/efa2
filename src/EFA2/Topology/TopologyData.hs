

module EFA2.Topology.TopologyData (
       module Data.Graph.Inductive, 
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
       isOtherSection,
       isOriginalEdge,
       isInnerStorageEdge,
       isIntersectionEdge,
       getStorageNumber,
       defaultELabel,
       unlabelEdge,
       fromFlowToSecTopology,
       getActiveStores,
       partitionInOutStatic) where

import Data.Graph.Inductive
import Data.Function
import Data.Maybe

import EFA2.Topology.EfaGraph
import EFA2.Utils.Utils

import Debug.Trace


import qualified Data.List as L

data NodeType = Storage Int
              | InitStorage Int
              | Sink
              | Source
              | Crossing deriving (Show, Ord, Eq)

isStorage :: NodeType -> Bool
isStorage (Storage _) = True
isStorage (InitStorage _) = True
isStorage _ = False

getStorageNumber :: NodeType -> Int
getStorageNumber (Storage x) = x
getStorageNumber (InitStorage x) = x
getStorageNumber x = error $ "getStorageNumber: " ++ show x ++ " is not a storage"

data NLabel = NLabel { sectionNLabel :: Int,
                       recordNLabel :: Int,
                       nodeNLabel :: Int,
                       nodetypeNLabel :: NodeType } deriving (Show, Eq, Ord)

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




unlabelEdge :: LEdge a -> Edge
unlabelEdge (x, y, _) = (x, y)


newtype Topology' a b = Topology { unTopology :: EfaGraph a b } deriving (Show)
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
newtype FlowTopology' a b = FlowTopology { unFlowTopology :: EfaGraph a b } deriving (Show)
type FlowTopology = FlowTopology' NLabel ELabel

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
newtype SecTopology' a b = SecTopology { unSecTopology :: EfaGraph a b } deriving (Show)
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
getActiveStores topo = map sectionSort groupedIof
  where actTopo = elfilter isActiveEdge topo
        iof = mkInOutGraphFormat id actTopo
        sortedIof = L.sortBy (compare `on` stNum) (filter isSt iof)
        isSt (_, (_, l), _) = isStorage (nodetypeNLabel l)
        stNum (_, (_, l), _) | Storage x <- nodetypeNLabel l = x
                             | InitStorage x <- nodetypeNLabel l = x
        groupedIof = L.groupBy cmp sortedIof
        cmp a b = stNum a == stNum b
        sectionSort = L.sortBy (compare `on` sec)
        sec (_, (_, l), _) = sectionNLabel l



-- | Partition the storages in in and out storages, looking only at edges, not at values.
-- This means that nodes with in AND out edges cannot be treated.
partitionInOutStatic :: 
  Topology -> [InOutGraphFormat (LNode NLabel)] -> ([InOutGraphFormat (LNode NLabel)], [InOutGraphFormat (LNode NLabel)])
partitionInOutStatic topo iof = L.partition p iof
  where p ([], _, []) = False
        p (ins, (nid, l), outs) | ((_:_), []) <- (ins', outs') = True
                                | ([], (_:_)) <- (ins', outs') = True
                                | otherwise = False
          where ins' = filter q ins
                outs' = filter r outs
                q (n, _) = flowDirection e == WithDir && (isOriginalEdge e || isInnerStorageEdge e)
                  where e = thd3 $ fromJust (getLEdge topo n nid)
                        cond = isOriginalEdge e || isInnerStorageEdge e
                r (n, _) = flowDirection e == AgainstDir && (isOriginalEdge e || isInnerStorageEdge e)
                  where e = thd3 $ fromJust (getLEdge topo nid n)
                        cond = isOriginalEdge e || isInnerStorageEdge e

