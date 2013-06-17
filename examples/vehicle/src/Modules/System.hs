{-# LANGUAGE FlexibleContexts #-}

module Modules.System where

import qualified EFA.Example.Index as XIdx
import EFA.Example.Utility (makeEdges)

import EFA.Signal.Record (SigId(..))

import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr

import qualified Data.Map as Map
import Data.Map (Map)


data Node = Tank | EngineFlange | ConBattery | Battery | ConES | MotorFlange | ConFrontBrakes | Chassis | Resistance | ElectricSystem | FrontBrakes | VehicleInertia | RearBrakes deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault

----------------------------------------------------------------------
-- * Define System Topology
topology :: TD.Topology Node
topology = Gr.fromList ns (makeEdges es)
  where ns = [(Tank, TD.Source),
              (ConBattery, TD.Crossing),
              (Battery, TD.storage),
              (ConES, TD.Crossing),
              (ConFrontBrakes, TD.Crossing),
              (Chassis, TD.Crossing),
              (Resistance, TD.Sink),
              (ElectricSystem, TD.Sink),      -- vehicle electric system
              (FrontBrakes, TD.Sink),
              (RearBrakes, TD.Sink),
              (VehicleInertia, TD.storage)]

        --extract edge Info
        es = map f edgeList
          where f (n1,n2,_,_,_) = (n1,n2)

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


powerPositonNames :: Map (XIdx.PPos Node) SigId
powerPositonNames = Map.fromList $ concat $ map f edgeList
  where f (n1,n2,_,l1,l2) = [(XIdx.ppos n1 n2, SigId $ "Power-"++l1),
                             (XIdx.ppos n2 n1, SigId $ "Power-"++l2)]

showPowerId :: XIdx.PPos Node -> String
showPowerId ppos = f (Map.lookup  ppos powerPositonNames)
  where
    f (Just sid) = show sid
    f Nothing = (show ppos)


convertPowerId :: XIdx.PPos Node -> SigId
convertPowerId ppos =  f (Map.lookup  ppos powerPositonNames)
  where
    f (Just sid) = sid
    f Nothing = SigId (show ppos)

----------------------------------------------------------------------
-- * Calculate Flow States

flowStates :: [TD.FlowTopology Node]
flowStates = StateAnalysis.advanced topology
