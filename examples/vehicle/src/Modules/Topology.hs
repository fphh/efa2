{-# LANGUAGE FlexibleContexts #-}

module EXAMPLES.Vehicle.SeriesHybrid.Topology where

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph as Gr
import EFA.Example.Utility (edgeVar, makeEdges, (.=))
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

data Nodes = Tank | Con_engine | Con_battery | Battery | Con_evs | Con_motor | Con_frontBrakes | Con_chassis | DrivingResistance | ElectricSystem | FrontBrakes | VehicleInertia | RearBrakes deriving (Eq, Ord, Show)

instance Node.Show Nodes

----------------------------------------------------------------------
-- * Define System Topology
system :: TD.Topology Nodes
system = Gr.mkGraph ns (makeEdges es)
  where ns = [(Tank, TD.Source),
              (Con_engine , TD.Crossing),
              (Con_battery, TD.Crossing),
              (Battery, TD.Storage),
              (Con_evs, TD.Crossing),  -- electric crossing to vehicle electric system
              (Con_motor, TD.Crossing),  -- connection motor / gearbox
              (Con_frontBrakes, TD.Crossing),  -- connection brakes
              (Con_chassis, TD.Crossing),  -- connection inertia
              (DrivingResistance, TD.Sink),      -- driving resistance                  
              (ElectricSystem, TD.Sink),      -- vehicle electric system
              (FrontBrakes, TD.Sink),     -- vehicle brakes
              (RearBrakes, TD.Sink),     -- vehicle brakes
              (VehicleInertia, TD.Storage)]  -- vehicle inertia
             
        es = [(Tank, Con_engine),  -- ic engine
              (Con_engine, Con_battery),  -- generator
              (Con_battery, Con_evs),  -- electric connection        
              (Con_evs, Con_motor),  -- motor
              (Con_motor, Con_frontBrakes),  -- gearbox 
              (Con_frontBrakes, Con_chassis),  -- front wheels             
              (Con_chassis, DrivingResistance),  -- driving resistance
              (Con_battery, Battery),  -- battery              
              (Con_evs, ElectricSystem),  -- dcdc          
              (Con_frontBrakes, FrontBrakes), -- brakes
              (Con_chassis, RearBrakes), -- brakes
              (Con_chassis, VehicleInertia)] -- inertia             


----------------------------------------------------------------------
-- * Calculate Flow States

flowStates :: [TD.FlowTopology Nodes]
flowStates = StateAnalysis.advanced topo    
