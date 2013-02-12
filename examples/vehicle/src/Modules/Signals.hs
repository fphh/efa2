{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}



module EXAMPLES.Vehicle.SeriesHybrid.Signals where


import EFA.Signal.Record (SigId(SigId),PPosIdx(..),
                          getSig,getTime,extractLogSignals, 
                          PowerRecord, SignalRecord, genPowerRecord)

-- import qualified EFA.Graph.Topology.Node as Node
import EFA.Signal.Signal((.*), (.+), (.-), neg, TC, Signal, len, fromList)
import EFA.Signal.Typ(UT,Typ)
import EFA.Signal.Data(Data(..),Nil, (:>))

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
                                      -- (SigId "battery1.pin_p.v",id),
                                       (SigId "battery1.pin_p.i",id),
                                       (SigId "potentialsensor1.p.v",id),
                                       (SigId "battery1.constantvoltage1.v",neg), -- constant 200
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
                                       (SigId "drivingresistance1.force1.f",neg),
                                       (SigId "brake1.tau",neg),
                                       (SigId "brake2.tau",neg)]
  
--------------------------------------------------------------------------------------- 
-- * Calculate special signals

calculatePower :: SignalRecord [] Double -> PowerRecord Nodes [] Double
calculatePower rec = pRec
  where
      -- Convenience function
      g sigId = getSig rec $ SigId sigId
  
      time = getTime rec
      zeroSig = fromList (replicate (len time) 0) :: TC Signal (Typ UT UT UT) (Data ([] :> Nil) Double)
      
      speed = g "speedsensor1.v" 
      voltage = g "potentialsensor1.p.v"

    
      -- generator
      generatorElectricPower =  g "electricmotor2.signalcurrent1.p.i" .* voltage
   
      -- battery
      batteryClampsPower = voltage.*g "battery1.pin_p.i"
      batteryInternalVoltage = fromList (replicate (len time) 200) :: TC Signal (Typ UT UT UT) (Data ([] :> Nil) Double)
      batteryInternalPower = g "battery1.pin_p.i".*batteryInternalVoltage
      
      -- dcdc -- TODO !!     
      dcdcPowerHV = zeroSig
      dcdcPowerLV = zeroSig

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
             [(PPosIdx Tank EngineFlange, 
               g "engine1.FuelPower",             
               g "engine1.Speed" .* g "engine1.flange_b.tau"                
              ),
              
              -- generator 
              (PPosIdx EngineFlange ConBattery, 
               g  "electricmotor2.speedsensor1.w".* g "electricmotor2.flange_a.tau",
               generatorElectricPower
              ),
              
              -- connection
              (PPosIdx ConBattery ConES,
               generatorElectricPower .- batteryClampsPower,
               generatorElectricPower .- batteryClampsPower 
              ),
              
              --motor
              (PPosIdx ConES MotorFlange,
               (g "electricmotor1.signalcurrent1.p.i".* voltage),
               g "electricmotor1.speedsensor1.w".* g "electricmotor1.flange_a.tau"
              ),
              
              -- gearbox
              (PPosIdx MotorFlange ConFrontBrakes,
               g "gearbox1.flange_a.tau".* g "gearbox1.inertia1.w",
               g "gearbox1.flange_b.tau".* g "gearbox1.inertia2.w"
              ),
              
              -- front wheels
              (PPosIdx ConFrontBrakes Chassis,
               g "idealrollingwheel1.flangeR.tau".* g "brake1.w",
               g "idealrollingwheel1.flangeT.f".* speed 
              ),
              
              -- driving Resistance
              (PPosIdx Chassis Resistance,
               g "drivingresistance1.force1.f".* speed,
               g "drivingresistance1.force1.f".* speed
              ),
              
              -- battery
              (PPosIdx ConBattery Battery,
               batteryClampsPower,
               batteryInternalPower
              ),
              
              -- DCDC
              (PPosIdx ConES ElectricSystem,
               dcdcPowerHV,
               dcdcPowerLV
              ),
              
              -- Front brake
              (PPosIdx ConFrontBrakes FrontBrakes,
               g "brake1.lossPower",
               g "brake1.lossPower"
              ),
              
              --Rear brake
              (PPosIdx Chassis RearBrakes,
               g "idealrollingwheel2.flangeR.tau".* g "brake2.w",
               g "idealrollingwheel2.flangeT.f".* speed
              ),
              
              --kinetic power
              (PPosIdx Chassis VehicleInertia,
               kineticPower,
               kineticPower
               
              )]
              

 