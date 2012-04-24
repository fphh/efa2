module EFA2.Topology.Flow (module EFA2.Topology.Flow) where


import Data.Graph.Inductive
import qualified Data.Map as M

-- import EFA2.Topology.GraphData
import EFA2.Topology.Topology

import EFA2.Signal.Sequence
import EFA2.Signal.SignalAnalysis


type Topology = Gr NLabel () -- ELabel
data FlowTopology = FlowTopology (Gr NLabel ()) deriving (Show)

data FlowState = FlowState (PPosData Sign) deriving (Show)
type SequFlowState = SequData FlowState
type SequFlowTops = SequData FlowTopology

-- | Function to calculate flow states for the whole sequence 
genSequFState :: SequFlowRecord -> SequFlowState
genSequFState (SequData sqFRec) = SequData $ map genFlowState sqFRec

-- | Function to extract the flow state out of a Flow Record  
genFlowState :: FlowRecord ->  FlowState
genFlowState fRec@(FlowRecord time flowMap) = FlowState $ M.map  f flowMap  
  where f flow = sign (foldl (+) 0 flow)

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