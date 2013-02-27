{-# LANGUAGE FlexibleContexts #-}

module Modules.System where

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import EFA.Example.Utility (makeEdges)
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified Data.Map as M
import EFA.Signal.Record(PPosIdx(..),SigId(..))

data Node = Tank | EngineFlange | ConBattery | Battery | ConES | MotorFlange | ConFrontBrakes | Chassis | Resistance | ElectricSystem | FrontBrakes | VehicleInertia | RearBrakes deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault

----------------------------------------------------------------------
-- * Define System Topology
topology :: TD.Topology Node
topology = Gr.mkGraph ns (makeEdges es)
  where ns = [(Tank, TD.Source),
--              (EngineFlange, TD.Crossing),
              (ConBattery, TD.Crossing), -- electric crossing at battery to vehicle electric system
              (Battery, TD.Storage),
              (ConES, TD.Crossing),  -- electric crossing to vehicle electric system
--              (MotorFlange, TD.Crossing),
              (ConFrontBrakes, TD.Crossing),
              (Chassis, TD.Crossing),
              (Resistance, TD.Sink),
              (ElectricSystem, TD.Sink),      -- vehicle electric system
              (FrontBrakes, TD.Sink),
              (RearBrakes, TD.Sink),
              (VehicleInertia, TD.Storage)]

        --extract edge Info
        es = map f edgeList
          where f (n1,n2,_,_,_) = (n1,n2)

-- Define Edges with all their Properties
edgeList :: [(Node, Node, String, String, String)]
edgeList = [--(Tank, EngineFlange, "Engine", "Fuel","CrankShaft"),
            --(EngineFlange, ConBattery,"Generator","GeneratorFlange","GeneratorClamps"),
            (Tank, ConBattery, "Engine&Generator", "Fuel","GeneratorClamps"),
            (ConBattery, ConES,"Wire","Wire","Wire"),
            (ConES, ConFrontBrakes,"Motor&Gearbox","MotorClamps","OutShaft"),
--            (ConES, MotorFlange,"Motor","MotorClamps","MotorFlange"),
--            (MotorFlange, ConFrontBrakes,"Gearbox","InShaft","OutShaft"),
            (ConFrontBrakes, Chassis, "FrontWheels","FrontWheelHub","FrontTires"),
            (Chassis, Resistance,"ToResistance","ToResistance","ToResistance"),
            (ConBattery, Battery,"BatteryResistance","BatteryClamps","BatteryCore"),
            (ConES, ElectricSystem,"DCDC","HighVoltage","LowVoltage"),
            (ConFrontBrakes, FrontBrakes,"ToFrontBrakes","ToFrontBrakes","ToFrontBrakes"),
            (Chassis, RearBrakes,"RearWheels","RearTires", "RearWheelHubs"),
            (Chassis, VehicleInertia,"ToIntertia","ToInertia", "ToInertia")]


powerPositonNames :: M.Map (PPosIdx Node) SigId
powerPositonNames = M.fromList $ concat $ map f edgeList
  where f (n1,n2,_,l1,l2) = [(PPosIdx n1 n2, SigId l1),
                             (PPosIdx n2 n1, SigId l2)]

----------------------------------------------------------------------
-- * Calculate Flow States

flowStates :: [TD.FlowTopology Node]
flowStates = StateAnalysis.advanced topology
