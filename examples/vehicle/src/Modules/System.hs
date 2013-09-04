{-# LANGUAGE FlexibleContexts #-}

module Modules.System where

import EFA.Application.Utility (makeTopology)

import qualified EFA.Flow.Sequence.Index as SeqIdx

import EFA.Signal.Record (SigId(..))

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as Topo

import qualified Data.Map as Map
import Data.Map (Map)


data Node =
     Tank
--   | EngineFlange
   | ConBattery
   | Battery
   | ConES
--   | MotorFlange
   | ConFrontBrakes
   | Chassis
   | Resistance
   | ElectricSystem
   | FrontBrakes
   | VehicleInertia
   | RearBrakes
   deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault
   typ t =
      case t of
         Tank -> Node.Source
         ConBattery -> Node.Crossing
         Battery -> Node.storage
         ConES -> Node.Crossing
         ConFrontBrakes -> Node.Crossing
         Chassis -> Node.Crossing
         Resistance -> Node.Sink
         ElectricSystem -> Node.Sink
         FrontBrakes -> Node.Sink
         RearBrakes -> Node.Sink
         VehicleInertia -> Node.storage

----------------------------------------------------------------------
-- * Define System Topology
topology :: Topo.Topology Node
topology = makeTopology edgeList

-- Define Edges with all their Properties
edgeList :: [(Node, Node, String, String, String)]
edgeList = [(Tank, ConBattery, "Engine&Generator", "Fuel","GeneratorClamps"),
            (ConBattery, ConES,"Wire","Wire","Wire"),
            (ConES, ConFrontBrakes,"Motor&Gearbox","MotorClamps","OutShaft"),
            (ConFrontBrakes, Chassis, "FrontWheels","FrontWheelHub","FrontTires"),
            (Chassis, Resistance,"ToResistance","ToResistance","ToResistance"),
            (ConBattery, Battery,"BatteryResistance","BatteryClamps","BatteryCore"),
            (ConES, ElectricSystem,"DCDC","HighVoltage","LowVoltage"),
            (ConFrontBrakes, FrontBrakes,"ToFrontBrakes","ToFrontBrakes","ToFrontBrakes"),
            (Chassis, RearBrakes,"RearWheels","RearTires", "RearWheelHubs"),
            (Chassis, VehicleInertia,"ToIntertia","ToInertia", "ToInertia")]


edgeNames :: Map (Node, Node) String
edgeNames = Map.fromList el
  where el = map f edgeList
        f (x, y, lab, _, _) = ((x, y), lab)


powerPositonNames :: Map (SeqIdx.PPos Node) SigId
powerPositonNames = Map.fromList $ concat $ map f edgeList
  where f (n1,n2,_,l1,l2) = [(SeqIdx.ppos n1 n2, SigId $ "Power-"++l1),
                             (SeqIdx.ppos n2 n1, SigId $ "Power-"++l2)]

showPowerId :: SeqIdx.PPos Node -> String
showPowerId ppos = f (Map.lookup  ppos powerPositonNames)
  where
    f (Just sid) = show sid
    f Nothing = (show ppos)


convertPowerId :: SeqIdx.PPos Node -> SigId
convertPowerId ppos =  f (Map.lookup  ppos powerPositonNames)
  where
    f (Just sid) = sid
    f Nothing = SigId (show ppos)

----------------------------------------------------------------------
-- * Calculate Flow States

flowStates :: [Topo.FlowTopology Node]
flowStates = StateAnalysis.advanced topology
