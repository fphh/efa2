

module EFA.Graph.Flow where

import qualified EFA.Graph as Gr
import EFA.Graph
          (Edge(Edge),
           labNodes, labEdges,
           insNodes, insEdges)

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Signal.SequenceData as SD
import EFA.Signal.SequenceData (SequData)
import EFA.Signal.Record
          (Record(Record), FlowState(FlowState), FlowRecord,
           PPosIdx(PPosIdx), flipPos)
import EFA.Graph.Topology
          (Topology, FlowTopology, ClassifiedTopology, SequFlowGraph,
           FlowDirection(Dir, UnDir))


import qualified EFA.Signal.Vector as SV
import EFA.Signal.Signal (fromScalar, sigSign, sigSum, neg)
import EFA.Signal.Base (Sign(PSign, NSign, ZSign),BSum, DArith0)

import qualified Data.Foldable as Fold
import qualified Data.Map as M
import Control.Monad (join)

import EFA.Utility (checkedLookup)


adjustSigns ::
  (Show (v a), DArith0 a,
  SV.Walker v, SV.Storage v a, Ord node, Show node) =>
  Topology node ->
  FlowState node -> FlowRecord node v a -> FlowRecord node v a
adjustSigns topo (FlowState state) (Record dt flow) =
   Record dt (M.foldrWithKey g M.empty uniquePPos)
      where g ppos NSign acc =
              M.insert ppos (neg (flow `checkedLookup` ppos))
                $ M.insert ppos' (neg (flow `checkedLookup` ppos')) acc
                where ppos' = flipPos ppos
            g ppos _ acc =
              M.insert ppos (flow `checkedLookup` ppos)
                $ M.insert ppos' (flow `checkedLookup` ppos') acc
                where ppos' = flipPos ppos
            uniquePPos = foldl h M.empty (labEdges topo)
              where h acc (Edge idx1 idx2, ()) =
                      M.insert ppos (state `checkedLookup` ppos) acc
                      where ppos = PPosIdx idx1 idx2


-- | Function to calculate flow states for the whole sequence
genSequFState ::
  (SV.Walker v, SV.Storage v a, BSum a, Fractional a, Ord a) =>
  SequData (FlowRecord node v a) -> SequData (FlowState node)
genSequFState sqFRec = fmap genFlowState sqFRec

-- | Function to extract the flow state out of a Flow Record
genFlowState ::
  (SV.Walker v, SV.Storage v a, BSum a, Fractional a, Ord a) =>
  FlowRecord node v a -> FlowState node
genFlowState (Record _time flowMap) =
   FlowState $ M.map (fromScalar . sigSign . sigSum) flowMap

-- | Function to generate Flow Topologies for all Sections
genSequFlowTops ::
  (Ord node, Show node) =>
  Topology node -> SequData (FlowState node) -> SequData (FlowTopology node)
genSequFlowTops topo = fmap (genFlowTopology topo)

-- | Function to generate Flow Topology -- only use one state per signal
genFlowTopology ::
  (Ord node, Show node) =>
  Topology node -> FlowState node -> FlowTopology node
genFlowTopology topo (FlowState fs) =
   Gr.fromList (labNodes topo) $
   map
      (\(Edge idx1 idx2, ()) ->
         case fs `checkedLookup` (PPosIdx idx1 idx2) of
            PSign -> (Edge idx1 idx2, Dir)
            NSign -> (Edge idx2 idx1, Dir)
            ZSign -> (Edge idx1 idx2, UnDir)) $
   labEdges topo


mkSectionTopology ::
  (Ord node) =>
  Idx.Section -> ClassifiedTopology node -> SequFlowGraph node
mkSectionTopology sid = Gr.ixmap (Idx.afterSecNode sid)


mkStorageEdges ::
   node -> M.Map Idx.Section Topo.StoreDir ->
   [Topo.LEdge node]
mkStorageEdges node stores = do
   let (ins, outs) =
          M.partition (Topo.In ==) $ M.mapKeys Idx.AfterSection stores
   secin <- Idx.initial : M.keys ins
   secout <- M.keys $ snd $ M.split secin outs
   return $
      (Edge (Idx.BndNode secin node) (Idx.BndNode secout node), Dir)

getActiveStoreSequences ::
   (Ord node) =>
   SequData (Topo.ClassifiedTopology node) ->
   M.Map node (M.Map Idx.Section Topo.StoreDir)
getActiveStoreSequences sq =
   Fold.foldl
      (M.unionWith (M.unionWith (error "duplicate section for node")))
      M.empty $
   SD.mapWithSection
      (\s g ->
          fmap (M.singleton s) $
          M.mapMaybe (join . Topo.maybeStorage) $ Gr.nodeLabels g) sq


type RangeGraph node = (M.Map Idx.Section SD.Range, SequFlowGraph node)

mkSequenceTopology ::
   (Ord node) =>
   SequData (FlowTopology node) ->
   RangeGraph node
mkSequenceTopology sd =
   (,) (Fold.fold $ SD.mapWithSectionRange (\s rng _ -> M.singleton s rng) sq) $
   insEdges (Fold.fold $ M.mapWithKey mkStorageEdges tracks) $
   insNodes
      (map (\n -> (Idx.initBndNode n, Topo.Storage (Just Topo.In))) $
       M.keys tracks) $
   Fold.fold $
   SD.mapWithSection mkSectionTopology sq
  where tracks = getActiveStoreSequences sq
        sq = fmap Topo.classifyStorages sd
