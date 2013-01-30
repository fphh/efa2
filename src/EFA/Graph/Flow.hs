

module EFA.Graph.Flow where

import qualified EFA.Graph as Gr
import EFA.Graph
          (Edge(Edge), mkGraph,
           labNodes, labEdges,
           insNodes, insEdges)

import qualified EFA.Graph.Topology.Index as Idx
import EFA.Graph.Topology as Topo
import EFA.Signal.SequenceData

import EFA.Signal.Signal (fromScalar, sigSign, sigSum)
import EFA.Signal.Base (Sign(..))

import qualified Data.Foldable as Fold
import qualified Data.Map as M

import EFA.Utility (checkedLookup)


-- | Function to calculate flow states for the whole sequence
genSequFState :: SequFlowRecord (FlowRecord nty) -> SequFlowState nty
genSequFState sqFRec = fmap genFlowState sqFRec

-- | Function to extract the flow state out of a Flow Record
genFlowState ::  FlowRecord nty -> FlowState nty
genFlowState (FlRecord _time flowMap) =
   FlowState $ M.map (fromScalar . sigSign . sigSum) flowMap

-- | Function to generate Flow Topologies for all Sequences
genSequFlowTops ::
  (Ord nty, Show nty) =>
  Topology nty -> SequFlowState nty -> SequFlowTops nty
genSequFlowTops topo = fmap (genFlowTopology topo)

-- | Function to generate Flow Topology -- only use one state per signal
genFlowTopology ::
  (Ord nty, Show nty) => 
  Topology nty -> FlowState nty -> FlowTopology nty
genFlowTopology topo (FlowState fs) =
   mkGraph (labNodes topo) $
   map
      (\(Edge idx1 idx2, ()) ->
         case fs `checkedLookup` (PPosIdx idx1 idx2) of
            PSign -> (Edge idx1 idx2, Dir)
            NSign -> (Edge idx2 idx1, Dir)
            ZSign -> (Edge idx1 idx2, UnDir)) $
   labEdges topo


mkSectionTopology ::
  (Ord nty) =>
  Idx.Section -> FlowTopology nty -> SequFlowGraph nty
mkSectionTopology sid = Gr.ixmap (Idx.SecNode sid)

genSectionTopology ::
  (Ord nty) =>
  SequFlowTops nty -> SequData (SequFlowGraph nty)
genSectionTopology = zipWithSecIdxs mkSectionTopology


copySeqTopology :: 
  (Ord nty) =>
  SequData (SequFlowGraph nty) -> SequFlowGraph nty
copySeqTopology =
   Fold.foldl Gr.union Gr.empty


mkIntersectionEdges ::
   nty -> Idx.Section ->
   M.Map Idx.Section StoreDir ->
   [Topo.LEdge nty]
mkIntersectionEdges node startSec stores =
   concatMap
      (\secin ->
         map (\secout ->
                (Edge (Idx.SecNode secin node) (Idx.SecNode secout node), Dir)) $
         M.keys $ snd $ M.split secin outs) $
   startSec : M.keys ins
  where (ins, outs) = M.partition (In ==) stores


mkSequenceTopology ::
  (Ord nty) =>
  SequData (SequFlowGraph nty) -> SequFlowGraph nty
mkSequenceTopology sd = res
  where sqTopo = copySeqTopology sd
        initNode = Idx.SecNode Idx.initSection
        startElems = map f $ M.toList $ getActiveStores sqTopo
        f (n, io) =
          (mkIntersectionEdges n Idx.initSection (fmap snd io), (nid, Storage))
          where nid = initNode n

        res = insEdges (concatMap fst startElems)
              $ insNodes (map snd startElems) sqTopo
