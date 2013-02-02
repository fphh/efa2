{-# LANGUAGE FlexibleContexts #-}

module EXAMPLES.Vehicle.SeriesHybrid.Signals where

import Data.Foldable (foldMap)
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis


import EFA.Signal.Record (SigId(SigId),PPosIdx(..),
                          getSig,getTime,extractLogSignals, 
                          PowerRecord, SignalRecord, genPowerRecord)

import qualified EFA.Graph.Topology.Node as Node
import EFA.Signal.Signal((.*), (.+), (.-), neg)

import EXAMPLES.Vehicle.SeriesHybrid.System  (Nodes(..))
 


---------------------------------------------------------------------------------------
-- * Model the System Topology

condition :: SignalRecord [] Double -> SignalRecord [] Double
condition rec = extractLogSignals rec [(SigId "engine1.Speed",id),
                                       (SigId "engine1.flange_b.tau",neg),
                                       (SigId "engine1.FuelPower",id),
                                       (SigId "electricmotor2.speedsensor1.w",id),
                                       (SigId "electricmotor2.signalcurrent1.p.i",neg),
                                       (SigId "electricmotor2.signalcurrent1.v",neg),
                                       (SigId "electricmotor2.flange_a.tau",id),
                                       (SigId "battery1.pin_p.v",id),
                                       (SigId "battery1.pin_p.i",id),
                                       (SigId "battery1.constantvoltage1.v",neg),
                                       (SigId "battery1.constantvoltage1.i",id),
                                       (SigId "electricmotor1.speedsensor1.w",id),
                                       (SigId "electricmotor1.flange_a.tau",neg),
                                       (SigId "electricmotor1.signalcurrent1.p.i",id),
                                       (SigId "electricmotor1.signalcurrent1.p.v",id),
                                       (SigId "gearbox1.flange_a.tau",id),
                                       (SigId "gearbox1.inertia1.w",id),
                                       (SigId "gearbox1.flange_b.tau",neg),
                                       (SigId "gearbox1.inertia2.w",id),
                                       (SigId "brake1.lossPower",id),
                                       (SigId "brake1.w",id),
                                       (SigId "brake2.lossPower",id),
                                       (SigId "brake2.w",id),
                                       (SigId "idealrollingwheel1.flangeR.tau",id),
                                       (SigId "idealrollingwheel1.flangeT.f",neg),
                                       (SigId "idealrollingwheel2.flangeR.tau",neg),
                                       (SigId "idealrollingwheel2.flangeT.f",id),
                                       (SigId "chassis1.flange_a.f",neg),
                                       (SigId "chassis1.flange_a1.f",id),
                                       (SigId "speedsensor1.v",id),
                                       (SigId "drivingresistance1.force1.f",neg)]
  
--------------------------------------------------------------------------------------- 
-- * Calculate special signals

calculatePower :: SignalRecord [] Double -> PowerRecord Nodes [] Double
calculatePower rec = pRec
  where
      -- Convenience function
      g sigId = getSig rec $ SigId sigId
  
      time = getTime rec
      speed = g "speedsensor1.v" 
    
      -- generator
      generatorCurrent = g "electricmotor2.signalcurrent1.p.i"
      generatorVoltage = neg $ g "electricmotor2.signalcurrent1.v"
      generatorElectricPower =  generatorCurrent .* generatorVoltage
   
      -- battery
      batteryPoleVoltage = g "battery1.pin_p.v"
      batteryPoleCurrent = g "battery1.pin_p.i"                      
      batteryPolePower = batteryPoleVoltage.*batteryPoleCurrent
      
      -- dcdc -- TODO !!     
      dcdcPowerHV = g "battery1.pin_p.i"
      dcdcPowerLV = g "battery1.pin_p.i"
      
      -- chassis       
      frontAxleForce = g "chassis1.flange_a.f"
      rearAxleForce =  g "chassis1.flange_a1.f"                 
      frontAxlePower =  frontAxleForce.*speed        
      rearAxlePower =  rearAxleForce.*speed
      kineticPower = (frontAxlePower.+rearAxlePower).-resistancePower
      
      resistancePower = g "drivingresistance1.force1.f".*speed
  
        
--------------------------------------------------------------------------------------- 
-- ## Build Power Record
      
      pRec = genPowerRecord time 
             -- engine
             [(PPosIdx Tank Con_engine, 
               g "engine1.Speed" .* g "engine1.flange_b.tau",                
               g "engine1.FuelPower"
              ),
              
              -- generator 
              (PPosIdx Con_engine Con_battery, 
               g  "electricmotor2.speedsensor1.w".* g "electricmotor2.flange_a.tau",
               g "electricmotor2.signalcurrent1.p.i".* g "electricmotor2.signalcurrent1.v"
              ),
              
              -- connection
              (PPosIdx Con_battery Con_evs,
               batteryPolePower.-generatorElectricPower,
               batteryPolePower.-generatorElectricPower
              ),
              
              --motor
              (PPosIdx Con_evs Con_motor,
               g "electricmotor1.speedsensor1.w".* g "electricmotor1.flange_a.tau",
               g "electricmotor1.signalcurrent1.p.i".* g "electricmotor1.signalcurrent1.p.v"
              ),
              
              -- gearbox
              (PPosIdx Con_motor Con_frontBrakes,
               g "gearbox1.flange_a.tau".* g "gearbox1.inertia1.w",
               g "gearbox1.flange_b.tau".* g "gearbox1.inertia2.w"
              ),
              
              -- front wheels
              (PPosIdx Con_frontBrakes Con_chassis,
               g "idealrollingwheel1.flangeR.tau".* g "brake1.w",
               g "idealrollingwheel1.flangeT.f".* speed 
              ),
              
              -- driving Resistance
              (PPosIdx Con_chassis DrivingResistance,
               g "drivingresistance1.force1.f".* speed,
               g "drivingresistance1.force1.f".* speed
              ),
              
              -- battery
              (PPosIdx Con_battery Battery,
               g "battery1.pin_p.v".* g "battery1.pin_p.i",
               g "battery1.constantvoltage1.v".* g "battery1.constantvoltage1.i"
              ),
              
              -- DCDC
              (PPosIdx Con_evs ElectricSystem,
               dcdcPowerHV,
               dcdcPowerLV
              ),
              
              -- Front brake
              (PPosIdx Con_frontBrakes FrontBrakes,
               g "brake1.lossPower",
               g "brake1.lossPower"
              ),
              
              --Rear brake
              (PPosIdx Con_chassis RearBrakes,
               g "idealrollingwheel2.flangeR.tau".* g "brake2.w",
               g "idealrollingwheel2.flangeT.f".* speed
              ),
              
              --kinetic power
              (PPosIdx Con_chassis VehicleInertia,
               kineticPower,
               kineticPower
               
              )]
              

 
