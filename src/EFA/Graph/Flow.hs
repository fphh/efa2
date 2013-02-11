

module EFA.Graph.Flow where

import qualified EFA.Graph as Gr
import EFA.Graph
          (Edge(Edge), mkGraph,
           labNodes, labEdges,
           insNodes, insEdges)

import qualified EFA.Graph.Topology.Index as Idx
import EFA.Graph.Topology as Topo
import EFA.Signal.SequenceData
import EFA.Signal.Record


import EFA.Signal.Signal (fromScalar, sigSign, sigSum, neg)
import EFA.Signal.Vector (Storage,Walker)
import EFA.Signal.Base (Sign(PSign, NSign, ZSign),BSum, DArith0)

import Control.Applicative ((<$>), (<*>))

import qualified Data.Foldable as Fold
import qualified Data.Map as M

import EFA.Utility (checkedLookup)

adjustSigns ::
  (Show (v a), DArith0 a,
  Walker v, Storage v a, Ord nty, Show nty) =>
  Topology nty -> SequData (FlowState nty) ->
  SequData (FlowRecord nty v a) -> SequData (FlowRecord nty v a)
adjustSigns topo flowStates flowRec = f <$> flowStates <*> flowRec
  where f (FlowState state) (Record dt flow) =
          Record dt (M.foldrWithKey g M.empty state')
          where state' = uniquePPos topo state
                g ppos NSign acc = 
                  M.insert ppos (neg (flow `checkedLookup` ppos))
                    $ M.insert ppos' (neg (flow `checkedLookup` ppos')) acc
                    where ppos' = flipPos ppos
                g ppos _ acc =
                  M.insert ppos (flow `checkedLookup` ppos)
                    $ M.insert ppos' (flow `checkedLookup` ppos') acc
                    where ppos' = flipPos ppos
        uniquePPos topo state = foldl f M.empty (labEdges topo)
          where f acc (Edge idx1 idx2, ()) =
                  M.insert ppos (state `checkedLookup` ppos) acc
                  where ppos = PPosIdx idx1 idx2


-- | Function to calculate flow states for the whole sequence
genSequFState ::
  (Walker v, Storage v a, BSum a, Fractional a, Ord a) => 
  SequData (FlowRecord nty v a) -> SequData (FlowState nty)
genSequFState sqFRec = fmap genFlowState sqFRec

-- | Function to extract the flow state out of a Flow Record
genFlowState ::
  (Walker v, Storage v a, BSum a, Fractional a, Ord a) => 
  FlowRecord nty v a -> FlowState nty
genFlowState (Record _time flowMap) =
   FlowState $ M.map (fromScalar . sigSign . sigSum) flowMap

-- | Function to generate Flow Topologies for all Sequences
genSequFlowTops ::
  (Ord nty, Show nty) =>
  Topology nty -> SequData (FlowState nty) -> SequData (FlowTopology nty)
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
  Idx.Section -> FlowTopology nty -> (SequFlowGraph nty)
mkSectionTopology sid = Gr.ixmap (Idx.SecNode sid)

genSectionTopology ::
  (Ord nty) =>
  SequData (FlowTopology nty) -> SequData (SequFlowGraph nty)
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
