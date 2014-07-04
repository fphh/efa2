module EFA.Action.DemandAndControl where


import qualified EFA.Flow.Topology.Index as TopoIdx
--import qualified EFA.Data.OD.Signal.Flow as SignalFlow 
--import qualified EFA.Data.ND as ND

import qualified Data.Map as Map

data Var node = Power (TopoIdx.Power node ) | Ratio (TopoIdx.X node) deriving (Show,Eq,Ord)
data ControlVar node = ControlPower (TopoIdx.Power node ) | ControlRatio (TopoIdx.X node) deriving (Show,Eq,Ord)
data DemandVar node = DemandPower (TopoIdx.Power node ) | DemandRatio (TopoIdx.X node) deriving (Show,Eq,Ord)

unControlVar :: ControlVar node -> Var node
unControlVar (ControlPower x) = (Power x)
unControlVar (ControlRatio x) = (Ratio x)

unDemandVar :: ControlVar node -> Var node
unDemandVar (ControlPower x) = (Power x)
unDemandVar (ControlRatio x) = (Ratio x)


type ControlMap node a = Map.Map (ControlVar node) a 
type DemandMap node a = Map.Map (DemandVar node) a 

-- type DemandCycle inst dim label vec a b = SignalFlow.Signal inst label vec a (ND.Data dim b)

{-
lookupControlVar :: 
  (TopoQty.Section node (Result.Result b)  
  (ControlVar node) -> 
lookupControlVar flowSection (ControlPower idx) = TopoQty.lookupPower flowSection idx    
lookupControlVar flowSection (ControlRatio idx) = TopoQty.lookupX flowSection idx    

-}
