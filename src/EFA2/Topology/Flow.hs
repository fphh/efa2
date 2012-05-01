{-# LANGUAGE TupleSections #-}

module EFA2.Topology.Flow (module EFA2.Topology.Flow) where


import Data.Graph.Inductive
import Data.Function
import qualified Data.Map as M
import qualified Data.List as L

import Debug.Trace

import EFA2.Topology.TopologyData
import EFA2.Signal.SequenceData
import EFA2.Interpreter.Arith
import EFA2.Utils.Utils

-- | Function to calculate flow states for the whole sequence 
genSequFState :: SequFlowRecord -> SequFlowState
genSequFState (SequData sqFRec) = SequData $ map genFlowState sqFRec

-- | Function to extract the flow state out of a Flow Record  
genFlowState :: FlowRecord ->  FlowState
genFlowState fRec@(FlowRecord time flowMap) = FlowState $ M.map f flowMap
  where f flow = sign (cfoldr (+) 0 flow)

{-
-- | Function to check flow state on validity
checkFlowState :: Topology -> FlowState -> Bool
checkFlowState top@(gr nodes edges) (FlowState fs) = checkNodes && checkEdges 
  where checkNodes = map f nodes
        checkEdges = map g edges
        
        g edge@(idx1,idx2) | fs M.! (PPosIdx idx1 idx2) ==  fs M.! (PPosIdx idx2 idx1)
-}        
        
-- | Function to generate Flow Topologies for all Sequences
genSequFlowTops :: Topology -> SequFlowState -> SequFlowTops         
genSequFlowTops topo (SequData sequFlowStates) = SequData $ map (genFlowTopology topo) sequFlowStates  
    


-- | Function to generate Flow Topology -- only use one state per signal
genFlowTopology:: Topology -> FlowState -> FlowTopology
genFlowTopology topo (FlowState fs) = mkGraph (labNodes topo) (concat $ map f (labEdges topo)) 
  where f edge@(idx1,idx2, l) | fs M.! (PPosIdx idx1 idx2) == PSign = [(idx1, idx2, l { flowDirection = WithDir })] 
        f edge@(idx1,idx2, l) | fs M.! (PPosIdx idx1 idx2) == NSign = [(idx1, idx2, l { flowDirection = AgainstDir})]
        f edge@(idx1,idx2, l) | fs M.! (PPosIdx idx1 idx2) == ZSign = [(idx1, idx2, l { flowDirection = UnDir})]
{-
  where f edge@(idx1,idx2,_) | fs M.! (PPosIdx idx1 idx2) == PSign = [edge] 
        f edge@(idx1,idx2,lab) | fs M.! (PPosIdx idx1 idx2) == NSign = [(idx2,idx1,lab)]
        f edge@(idx1,idx2,_) | fs M.! (PPosIdx idx1 idx2) == ZSign = []
-}

mkSectionTopology :: SecIdx -> FlowTopology -> SecTopology
mkSectionTopology (SecIdx sid) t@(FlowTopology topo) = SecTopology $ nmap f topo
  where f n = n { sectionNLabel = sid }

genSectionTopology :: SequFlowTops -> SequData [SecTopology]
genSectionTopology (SequData tops) = SequData (map (uncurry mkSectionTopology) (zip [0..] tops))


copySeqTopology :: SequData [SecTopology] -> Topology
copySeqTopology (SequData tops) = mkGraph (concat ns'') (concat es'')
  where tops' = map unSecTopology tops
        ns = map labNodes tops'
        ns' = zip offsets ns
        es' = zip offsets (map labEdges tops')
        ns'' = map g ns'
        g (o, nlist) = map (\(n, l) -> (n+o, l)) nlist
        es'' = map h es'
        h (o, elist) = map (\(n1, n2, l) -> (n1+o, n2+o, l)) elist

        offsets = reverse $ L.foldl' f [0] (map length ns)
        f (a:acc) l = (a+l):a:acc

-- TODO: Warn about Storages with more then one edge in or out!
isActStore :: (Node, NLabel, [ELabel], [ELabel]) -> Bool
isActStore a@(_, l, ins, outs) = isStorage (nodetypeNLabel l) && not (null (filter isActiveEdge es))
  where es = ins ++ outs


mkIntersectionEdges :: [(Node, NLabel, [ELabel], [ELabel])] -> [(Node, Node, ELabel)]
mkIntersectionEdges stores = interSecEs
  where (instores, outstores) = L.partition p stores
        p (n, _, [], [o]) = flowDirection o == AgainstDir
        p (n, _, [i], []) = flowDirection i == WithDir

        outs = map f outstores
        ins = map f instores
        f (n, l, _, _) = (n, l)

        es = map (\(n, l) -> (n, map fst (filter (q l) outs))) ins
        q lin (_, lout) = sectionNLabel lout > sectionNLabel lin

        e = defaultELabel { edgeType = IntersectionEdge}
        interSecEs = concatMap (\(n, ns) -> map (n,, e) ns) es


mkSequenceTopology :: SequData [SecTopology] -> Topology
mkSequenceTopology sd = trace (show grpStores) $ insEdges interSecEs sqTopo
  where sqTopo = copySeqTopology sd
        ns = map (\(n, l) -> (n, l, map snd (lpre sqTopo n), map snd (lsuc sqTopo n))) (labNodes sqTopo)

        stores = filter isActStore ns
        grpStores = L.groupBy cmp (L.sortBy (compare `on` stNum) stores)
        cmp a b = stNum a == stNum b
        stNum (_, l, _, _) | Storage x <- nodetypeNLabel l = x

        interSecEs = concatMap mkIntersectionEdges grpStores
