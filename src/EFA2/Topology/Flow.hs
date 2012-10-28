{-# LANGUAGE TupleSections, TypeOperators, FlexibleContexts #-}

module EFA2.Topology.Flow (module EFA2.Topology.Flow) where


--import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.List as L

import EFA2.Topology.TopologyData
import EFA2.Signal.SequenceData
-- import EFA2.Interpreter.Arith
import EFA2.Utils.Utils

import EFA2.Signal.Signal (fromScalar, sigSign, sigSum)
import EFA2.Signal.Base (Sign(PSign, NSign, ZSign))

import Data.Tuple.HT (snd3)


-- | Function to calculate flow states for the whole sequence 
genSequFState :: SequFlowRecord FlowRecord -> SequFlowState
genSequFState sqFRec = map genFlowState `fmap` sqFRec

-- | Function to extract the flow state out of a Flow Record  
genFlowState ::  FlowRecord -> FlowState
genFlowState (FlRecord _time flowMap) =
   FlowState $ M.map (fromScalar . sigSign . sigSum) flowMap

-- | Function to generate Flow Topologies for all Sequences
genSequFlowTops :: Topology -> SequFlowState -> SequFlowTops
genSequFlowTops topo = fmap (map (genFlowTopology topo))

-- | Function to generate Flow Topology -- only use one state per signal
genFlowTopology:: Topology -> FlowState -> FlowTopology
genFlowTopology topo (FlowState fs) =
   mkGraph (labNodes topo) $
   map
      (\(idx1, idx2, l) ->
         (idx1, idx2, l { flowDirection =
            case fs M.! PPosIdx idx1 idx2 of
               PSign -> WithDir
               NSign -> AgainstDir
               ZSign -> UnDir } )) $
   labEdges topo


mkSectionTopology :: SecIdx -> FlowTopology -> SecTopology
mkSectionTopology (SecIdx sid) =
   fromFlowToSecTopology . nmap (\n -> n { sectionNLabel = sid })

genSectionTopology :: SequFlowTops -> SequData [SecTopology]
genSectionTopology =
   fmap (map (uncurry mkSectionTopology) . zip [0..])


copySeqTopology :: SequData [SecTopology] -> Topology
copySeqTopology (SequData tops) =
   mkGraph
      (concat $ zipWith g offsets ns)
      (concat $ zipWith h offsets $ map labEdges tops)
  where ns = map labNodes tops
        g o = map (\(n, l) -> (n+o, l))
        h o = map (\(n1, n2, l) -> (n1+o, n2+o, l))

        offsets = reverse $ L.foldl' f [0] (map length ns)
        f (a:acc) l = (a+l):a:acc


mkIntersectionEdges ::
   Topology -> LNode NLabel ->
   [InOutGraphFormat (LNode NLabel)] -> [LEdge ELabel]
mkIntersectionEdges topo startNode stores =
   concatMap (\(n, ns) -> map (n,, e) ns) $
   map (\(n, l) -> (n, map fst (filter (q l) outs))) $
   startNode:ins
  where (instores, outstores) = partitionInOutStatic topo stores

        outs = map snd3 outstores
        ins = map snd3 instores

        q lin (_, lout) = sectionNLabel lout > sectionNLabel lin

        e = defaultELabel { edgeType = IntersectionEdge }


mkSequenceTopology :: SequData [SecTopology] -> Topology
mkSequenceTopology sd = res
  where sqTopo = copySeqTopology sd

        grpStores = getActiveStores sqTopo
        storeLabs = map g grpStores
        g ((_, (_, l), _):_) = l

        maxNode = 1 + (snd $ nodeRange sqTopo)
        startNodes = map f (zip (map (,maxNode) [maxNode+1 ..]) storeLabs)
        f ((nid1, nid2), NLabel _ n (Storage sn)) =
          [ (nid1, NLabel (-1) n (InitStorage sn)), (nid2, NLabel (-1) (-1) Source) ]

        interSecEs = concatMap (uncurry (mkIntersectionEdges sqTopo)) (zip (map head startNodes) grpStores)

        e = defaultELabel { edgeType = InnerStorageEdge }
        startEdges = map (\((nid1, _):(nid2, _):_) -> (nid2, nid1, e)) startNodes
        res = insEdges (startEdges ++ interSecEs) (insNodes (concat startNodes) sqTopo)
