module EFA2.Topology.Flow (module EFA2.Topology.Flow) where

import qualified EFA2.Topology.EfaGraph as Gr
import EFA2.Topology.EfaGraph
          (Edge(Edge), mkGraph,
           labNodes, labEdges,
           insNodes, insEdges)

import qualified EFA2.Signal.Index as Idx
import EFA2.Topology.TopologyData as Topo
import EFA2.Signal.SequenceData

import EFA2.Signal.Signal (fromScalar, sigSign, sigSum)
import EFA2.Signal.Base (Sign(PSign, NSign, ZSign))

import qualified Data.Foldable as Fold
import qualified Data.Map as M


-- | Function to calculate flow states for the whole sequence
genSequFState :: SequFlowRecord FlowRecord -> SequFlowState
genSequFState sqFRec = fmap genFlowState sqFRec

-- | Function to extract the flow state out of a Flow Record
genFlowState ::  FlowRecord -> FlowState
genFlowState (FlRecord _time flowMap) =
   FlowState $ M.map (fromScalar . sigSign . sigSum) flowMap

-- | Function to generate Flow Topologies for all Sequences
genSequFlowTops :: Topology -> SequFlowState -> SequFlowTops
genSequFlowTops topo = fmap (genFlowTopology topo)

-- | Function to generate Flow Topology -- only use one state per signal
genFlowTopology :: Topology -> FlowState -> FlowTopology
genFlowTopology topo (FlowState fs) =
   mkGraph (labNodes topo) $
   map
      (\(Edge idx1 idx2, ()) ->
         (Edge idx1 idx2,
            case fs M.! PPosIdx idx1 idx2 of
               PSign -> WithDir
               NSign -> AgainstDir
               ZSign -> UnDir )) $
   labEdges topo


mkSectionTopology :: Idx.Section -> FlowTopology -> SecTopology
mkSectionTopology sid = Gr.ixmap (Idx.SecNode sid)

genSectionTopology :: SequFlowTops -> SequData SecTopology
genSectionTopology = zipWithSecIdxs mkSectionTopology


copySeqTopology :: SequData SecTopology -> SequFlowGraph
copySeqTopology =
   Gr.emap (ELabel OriginalEdge) .
   Fold.foldl Gr.union Gr.empty


mkIntersectionEdges ::
   Idx.Node -> Idx.Section ->
   M.Map Idx.Section (Topo.InOut node ELabel) ->
   [Topo.LEdge]
mkIntersectionEdges node startSec stores =
   concatMap
      (\secin ->
         map (\secout ->
                (Edge (Idx.SecNode secin node) (Idx.SecNode secout node), e)) $
         M.keys $ snd $ M.split secin outs) $
   startSec : M.keys ins
  where (ins, outs) = partitionInOutStatic stores

        e = defaultELabel { edgeType = IntersectionEdge }

mkSequenceTopology :: SequData SecTopology -> SequFlowGraph
mkSequenceTopology sd = res
  where sqTopo = copySeqTopology sd

        initNode = Idx.SecNode Idx.initSection
        rootNode = initNode (Idx.Node (-1))
        startElems = map f $ M.toList $ getActiveStores sqTopo
        f (n, io) =
           let nid = initNode n
           in  ((Edge rootNode nid, e) :
                   mkIntersectionEdges n Idx.initSection io,
                (nid, Storage))

        e = defaultELabel { edgeType = InnerStorageEdge }
        res =
           insEdges (concatMap fst startElems) $
           insNodes ((rootNode, Source) : map snd startElems) sqTopo
