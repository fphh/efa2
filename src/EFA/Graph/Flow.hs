

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
  Walker v, Storage v a, Ord node, Show node) =>
  Topology node -> SequData (FlowState node) ->
  SequData (FlowRecord node v a) -> SequData (FlowRecord node v a)
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
        uniquePPos topol state = foldl h M.empty (labEdges topol)
          where h acc (Edge idx1 idx2, ()) =
                  M.insert ppos (state `checkedLookup` ppos) acc
                  where ppos = PPosIdx idx1 idx2


-- | Function to calculate flow states for the whole sequence
genSequFState ::
  (Walker v, Storage v a, BSum a, Fractional a, Ord a) => 
  SequData (FlowRecord node v a) -> SequData (FlowState node)
genSequFState sqFRec = fmap genFlowState sqFRec

-- | Function to extract the flow state out of a Flow Record
genFlowState ::
  (Walker v, Storage v a, BSum a, Fractional a, Ord a) => 
  FlowRecord node v a -> FlowState node
genFlowState (Record _time flowMap) =
   FlowState $ M.map (fromScalar . sigSign . sigSum) flowMap

-- | Function to generate Flow Topologies for all Sequences
genSequFlowTops ::
  (Ord node, Show node) =>
  Topology node -> SequData (FlowState node) -> SequData (FlowTopology node)
genSequFlowTops topo = fmap (genFlowTopology topo)

-- | Function to generate Flow Topology -- only use one state per signal
genFlowTopology ::
  (Ord node, Show node) =>
  Topology node -> FlowState node -> FlowTopology node
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
  (Ord node) =>
  Idx.Section -> FlowTopology node -> (SequFlowGraph node)
mkSectionTopology sid = Gr.ixmap (Idx.SecNode sid)


mkStorageEdges ::
   node -> M.Map Idx.Section StoreDir ->
   [Topo.LEdge node]
mkStorageEdges node stores = do
   let (ins, outs) = M.partition (In ==) stores
   secin <- Idx.initSection : M.keys ins
   secout <- M.keys $ snd $ M.split secin outs
   return $
      (Edge (Idx.SecNode secin node) (Idx.SecNode secout node), Dir)

getActiveStoreSequences ::
   (Ord section, Ord node, FlowDirectionField el) =>
   SequData (section, Gr.Graph node NodeType el) ->
   M.Map node (M.Map section StoreDir)
getActiveStoreSequences sq =
   Fold.foldl
      (M.unionWith (M.unionWith (error "duplicate section for node")))
      M.empty $
   fmap (\(s, g) ->
          fmap (M.singleton s) $
          M.mapMaybe snd $ getActiveStores g) sq

mkSequenceTopology ::
  (Ord node) =>
  SequData (FlowTopology node) -> SequFlowGraph node
mkSequenceTopology sd =
   insEdges (Fold.fold $ M.mapWithKey mkStorageEdges tracks) $
   insNodes
      (map (\n -> (Idx.SecNode Idx.initSection n, Storage)) $
       M.keys tracks) $
   Fold.foldMap (uncurry mkSectionTopology) sq
  where tracks = getActiveStoreSequences sq
        sq = zipWithSecIdxs (,) sd
