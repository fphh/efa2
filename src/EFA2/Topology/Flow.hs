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


-- | Function to calculate flow states for the whole sequence 
genSequFState :: SequFlowRecord FlowRecord -> SequFlowState
genSequFState sqFRec = map genFlowState `fmap` sqFRec

-- | Function to extract the flow state out of a Flow Record  
genFlowState ::  FlowRecord -> FlowState
genFlowState (FlRecord _time flowMap) = FlowState $ M.map f flowMap
  where f flow = fromScalar $ sigSign (sigSum flow)

-- | Function to generate Flow Topologies for all Sequences
genSequFlowTops :: Topology -> SequFlowState -> SequFlowTops         
genSequFlowTops topo (SequData sequFlowStates) = SequData $ map (genFlowTopology topo) sequFlowStates  

-- | Function to generate Flow Topology -- only use one state per signal
genFlowTopology:: Topology -> FlowState -> FlowTopology
genFlowTopology topo (FlowState fs) = res 
  where res :: FlowTopology
        res = mkGraph (labNodes topo) (concatMap f $ labEdges topo)
        f (idx1, idx2, l) =
           case fs M.! (PPosIdx idx1 idx2) of
              PSign -> [(idx1, idx2, l { flowDirection = WithDir })] 
              NSign -> [(idx1, idx2, l { flowDirection = AgainstDir})]
              ZSign -> [(idx1, idx2, l { flowDirection = UnDir})]


mkSectionTopology :: SecIdx -> FlowTopology -> SecTopology
mkSectionTopology (SecIdx sid) t = fromFlowToSecTopology $ nmap f t
  where f n = n { sectionNLabel = sid }

genSectionTopology :: SequFlowTops -> SequData [SecTopology]
genSectionTopology (SequData tops) = SequData (map (uncurry mkSectionTopology) (zip [0..] tops))


copySeqTopology :: SequData [SecTopology] -> Topology
copySeqTopology (SequData tops) = mkGraph (concat ns'') (concat es'')
  where ns = map labNodes tops
        ns' = zip offsets ns
        es' = zip offsets (map labEdges tops)
        ns'' = map g ns'
        g (o, nlist) = map (\(n, l) -> (n+o, l)) nlist
        es'' = map h es'
        h (o, elist) = map (\(n1, n2, l) -> (n1+o, n2+o, l)) elist

        offsets = reverse $ L.foldl' f [0] (map length ns)
        f (a:acc) l = (a+l):a:acc


mkIntersectionEdges :: Topology -> LNode NLabel -> [InOutGraphFormat (LNode NLabel)] -> [(Node, Node, ELabel)]
mkIntersectionEdges topo startNode stores = interSecEs
  where (instores, outstores) = partitionInOutStatic topo stores

        outs = map f outstores
        ins = map f instores
        f (_, x, _) = x

        es = map (\(n, l) -> (n, map fst (filter (q l) outs))) (startNode:ins)
        q lin (_, lout) = sectionNLabel lout > sectionNLabel lin

        e = defaultELabel { edgeType = IntersectionEdge }
        interSecEs = concatMap (\(n, ns) -> map (n,, e) ns) es


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
