module EFA2.Topology.Flow (module EFA2.Topology.Flow) where

import EFA2.Topology.EfaGraph
          (Edge(Edge), mkGraph,
           labNodes, labEdges,
           insNodes, insEdges,
           nmap, nodeSet)

import qualified EFA2.Signal.Index as Idx
import EFA2.Topology.TopologyData as Topo
import EFA2.Signal.SequenceData

import EFA2.Signal.Signal (fromScalar, sigSign, sigSum)
import EFA2.Signal.Base (Sign(PSign, NSign, ZSign))

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Tuple.HT (snd3)


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
genFlowTopology:: Topology -> FlowState -> FlowTopology
genFlowTopology topo (FlowState fs) =
   mkGraph (labNodes topo) $
   map
      (\(Edge idx1 idx2, l) ->
         (Edge idx1 idx2, l { flowDirection =
            case fs M.! PPosIdx idx1 idx2 of
               PSign -> WithDir
               NSign -> AgainstDir
               ZSign -> UnDir } )) $
   labEdges topo


mkSectionTopology :: Idx.Section -> FlowTopology -> SecTopology
mkSectionTopology sid =
   fromFlowToSecTopology . nmap (\n -> n { sectionNLabel = sid })

genSectionTopology :: SequFlowTops -> SequData SecTopology
genSectionTopology = zipWithSecIdxs mkSectionTopology


copySeqTopology :: SequData SecTopology -> Topology
copySeqTopology (SequData tops) =
   mkGraph
      (concat $ zipWith g offsets ns)
      (concat $ zipWith h offsets $ map labEdges tops)
  where ns = map labNodes tops
        g o = map (\(n, l) -> (n+o, l))
        h o = map (\(Edge n1 n2, l) -> (Edge (n1+o) (n2+o), l))

        offsets = L.scanl (+) 0 $ map length ns


mkIntersectionEdges ::
   (Idx.Section, node) ->
   M.Map Idx.Section (Topo.InOut node ELabel) -> [(Edge node, ELabel)]
mkIntersectionEdges startNode stores =
   concatMap
      (\(secin, n) ->
         map (\x -> (Edge n x, e)) $ M.elems $
         snd $ M.split secin outs) $
   startNode : M.toList ins
  where (instores, outstores) = partitionInOutStatic stores

        outs = fmap snd3 outstores
        ins = fmap snd3 instores

        e = defaultELabel { edgeType = IntersectionEdge }


mkSequenceTopology :: SequData SecTopology -> Topology
mkSequenceTopology sd = res
  where sqTopo = copySeqTopology sd

        maxNode = 1 + (S.findMax $ nodeSet sqTopo)
        rootNode = (maxNode, NLabel Idx.initSection (-1) Source)
        startElems =
           zipWith f [maxNode+1 ..] $ M.toList $ getActiveStores sqTopo
        f nid (st, (n, io)) =
           ((Edge maxNode nid, e) :
               mkIntersectionEdges (Idx.initSection, nid) io,
            (nid, NLabel Idx.initSection n (InitStorage st)))

        e = defaultELabel { edgeType = InnerStorageEdge }
        res =
           insEdges (concatMap fst startElems) $
           insNodes (rootNode : map snd startElems) sqTopo
