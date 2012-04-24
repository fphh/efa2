module EFA2.Topology.Flow (module EFA2.Topology.Flow) where


import Data.Graph.Inductive
import qualified Data.Map as M
import qualified Data.List as L

-- import EFA2.Topology.GraphData
import EFA2.Topology.Topology

import EFA2.Signal.Sequence
import EFA2.Signal.SignalAnalysis


type Topology = Gr NLabel () -- ELabel
newtype FlowTopology = FlowTopology (Gr NLabel ()) deriving (Show)
newtype SecTopology = SecTopology { unSecTopology :: Gr NLabel () } deriving (Show)

newtype FlowState = FlowState (PPosData Sign) deriving (Show)
type SequFlowState = SequData [FlowState]
type SequFlowTops = SequData [FlowTopology]

-- | Function to calculate flow states for the whole sequence 
genSequFState :: SequFlowRecord -> SequFlowState
genSequFState (SequData sqFRec) = SequData $ map genFlowState sqFRec

-- | Function to extract the flow state out of a Flow Record  
genFlowState :: FlowRecord ->  FlowState
genFlowState fRec@(FlowRecord time flowMap) = FlowState $ M.map  f flowMap  
  where f flow = sign (foldr (+) 0 flow)

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
genSequFlowTops top (SequData sequFlowStates) = SequData $ map (genFlowTopology top) sequFlowStates  
    


-- | Function to generate Flow Topology -- only use one state per signal
genFlowTopology:: Topology -> FlowState -> FlowTopology
genFlowTopology top (FlowState fs) = FlowTopology $ mkGraph (labNodes top) (concat $ map f (labEdges top)) 
  where f edge@(idx1,idx2,_) | fs M.! (PPosIdx idx1 idx2) == PSign = [edge] 
        f edge@(idx1,idx2,lab) | fs M.! (PPosIdx idx1 idx2) == NSign = [(idx2,idx1,lab)]
        f edge@(idx1,idx2,_) | fs M.! (PPosIdx idx1 idx2) == ZSign = []



mkSectionTopology :: SecIdx -> FlowTopology -> SecTopology
mkSectionTopology (SecIdx sid) (FlowTopology topo) = (SecTopology $ nmap f topo)
  where f n = n { sectionNLabel = sid }

genSectionTopology :: SequFlowTops -> SequData [SecTopology]
genSectionTopology (SequData tops) = SequData (map (uncurry mkSectionTopology) (zip [0..] tops))


mkSequenceTopology :: SequData [SecTopology] -> Topology
mkSequenceTopology (SequData tops) = mkGraph (concat ns'') (concat es'')
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
