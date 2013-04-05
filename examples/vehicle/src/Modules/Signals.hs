{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}


module Modules.Signals where


import EFA.Signal.Record (SigId(SigId),PPosIdx(..),
                          getSig,getTime,extractLogSignals,
                          PowerRecord, SignalRecord, genPowerRecord)

-- import qualified EFA.Graph.Topology.Node as Node
import EFA.Signal.Signal((.*), (.-), neg, TC, Signal, len, fromList)
import EFA.Signal.Typ(UT,Typ)
import EFA.Signal.Data(Data(..),Nil, (:>))

import Modules.System  (Node(..))


---------------------------------------------------------------------------------------
-- * Model the System Topology

condition :: SignalRecord [] Double -> SignalRecord [] Double
condition rec = extractLogSignals rec
                [(SigId "engine1._speed_log",id),
                 (SigId "engine1._torque_log",neg),
                 (SigId "engine1._fuelPower_log",id),
                 (SigId "engine1._switchOn_log",id),
                 (SigId "electricVehicleSystem1._voltage_log",id),                 
                 (SigId "electricVehicleSystem1._current_log",id),                 
                 (SigId "generator._speed_log",id),
                 (SigId "generator._current_log",neg),
                 (SigId "generator._voltage_log",neg),
                 (SigId "generator._torque_log",id),
                 (SigId "battery1._poleCurrent_log",id),
                 (SigId "battery1._poleVoltage_log",id),
                 (SigId "battery1._internalVoltage_log",neg),
                 (SigId "battery1._internalCurrent_log",id),
                 (SigId "motor._speed_log",id),
                 (SigId "motor._torque_log",neg),
                 (SigId "motor._current_log",id),
                 (SigId "motor._voltage_log",id),
                 (SigId "gearbox1._torqueInputShaft_log",id),
                 (SigId "gearbox1._speedInputShaft_log",id),
                 (SigId "gearbox1._torqueOutputShaft_log",neg),
                 (SigId "gearbox1._speedOutputShaft_log",id),
                 (SigId "_frontBrakesLossPower_log",id),
                 (SigId "_frontBrakesSpeed_log",id),
                 (SigId "_frontBrakesTorque_log",neg),                 
                 (SigId "_rearBrakesLossPower_log",id),
                 (SigId "_rearBrakesSpeed_log",id),
                 (SigId "_rearBrakesTorque_log",neg),                 
                 (SigId "_frontWheelsTorque_log",id),
                 (SigId "_frontWheelsForce_log",neg),
                 (SigId "_rearWheelsTorque_log",neg),
                 (SigId "_rearWheelsForce_log",id),
                 (SigId "chassis1._resistanceForce_log",neg),
                 (SigId "chassis1._propulsionForce_log",id),                 
                 (SigId "_vehicleSpeed_log",id)                
                ]

---------------------------------------------------------------------------------------
-- * Calculate special signals

calculatePower :: SignalRecord [] Double -> PowerRecord Node [] Double
calculatePower rec = pRec
  where
      -- Convenience function
      g sigId = getSig rec $ SigId sigId

      time = getTime rec
      zeroSig = fromList (replicate (len time) 0) :: TC Signal (Typ UT UT UT) (Data ([] :> Nil) Double)

      speed = g "_vehicleSpeed_log"
      voltage = g "battery1._poleVoltage_log"


      -- generator
      generatorElectricPower =  g "generator._current_log" .* voltage

      -- battery
      batteryClampsPower = voltage.*g "battery1._poleCurrent_log"
      batteryInternalVoltage = fromList (replicate (len time) 200) :: TC Signal (Typ UT UT UT) (Data ([] :> Nil) Double)
      batteryInternalPower = g "battery1._poleCurrent_log".*batteryInternalVoltage

      -- dcdc -- TODO !!
      powerEVS = (g "electricVehicleSystem1._voltage_log") .* (g "electricVehicleSystem1._current_log")


       -- chassis
      kineticPower = (g "chassis1._propulsionForce_log" .- g "chassis1._resistanceForce_log").*speed


---------------------------------------------------------------------------------------
-- ## Build Power Record

      pRec = genPowerRecord time

              -- engine
             [(PPosIdx Tank ConBattery,
               g "engine1._fuelPower_log",
               generatorElectricPower
              ),

              -- connection
              (PPosIdx ConBattery ConES,
               generatorElectricPower .- batteryClampsPower,
               generatorElectricPower .- batteryClampsPower
              ),

              -- --motor
              (PPosIdx ConES ConFrontBrakes,
               (g "motor._current_log".* voltage),
               g "gearbox1._torqueOutputShaft_log".* g "gearbox1._speedOutputShaft_log"
              ),

              -- front wheels
              (PPosIdx ConFrontBrakes Chassis,
               g "_frontWheelsTorque_log".* g "_frontBrakesSpeed_log",
               g "_frontWheelsForce_log".* speed
              ),

              -- driving Resistance
              (PPosIdx Chassis Resistance,
               g "chassis1._resistanceForce_log".* speed,
               g "chassis1._resistanceForce_log".* speed
              ),

              -- battery
              (PPosIdx ConBattery Battery,
               batteryClampsPower,
               batteryInternalPower
              ),

              -- DCDC
              (PPosIdx ConES ElectricSystem,
               powerEVS,
               powerEVS
              ),

              -- Front brake
              (PPosIdx ConFrontBrakes FrontBrakes,
               g "_frontBrakesLossPower_log",
               g "_frontBrakesLossPower_log"
              ),

              --Rear brake
              (PPosIdx Chassis RearBrakes,
               g "_rearWheelsTorque_log".* g "_rearBrakesSpeed_log",
               g "_rearWheelsForce_log".* speed
              ),

              --kinetic power
              (PPosIdx Chassis VehicleInertia,
               kineticPower,
               kineticPower

              )]


---------------------------------------------------------------------------------------
-- ## Signalgroups for Plotting


vehPowers :: [PPosIdx Node]
vehPowers = [PPosIdx Chassis VehicleInertia,
             PPosIdx Chassis RearBrakes,
             PPosIdx ConFrontBrakes FrontBrakes,
             PPosIdx Chassis Resistance]


-- Building Signal Record for better Plotting of the original signals
vehicle :: [SigId]
vehicle = [SigId "_vehicleSpeed_log",
           SigId "_frontWheelsTorque_log",
           SigId "_rearWheelsTorque_log",
           SigId "_frontBrakesTorque_log",
           SigId "_rearBrakesTorque_log",
           SigId "chassis1._resistanceForce_log",
           SigId "PPosIdx Chassis VehicleInertia"
          ]

-- Building Signal Record for better Plotting of the original signals
driveline:: [SigId]
driveline = [SigId "_vehicleSpeed_log",
             SigId "motor._torque_log",
             SigId "gearbox1._torqueInputShaft_log",
             SigId "gearbox1._torqueOutputShaft_log"
            ]

-- Building Signal Record for better Plotting of the original signals
motor:: [SigId]
motor = [SigId "_vehicleSpeed_log",
        SigId "motor._torque_log",
        SigId "motor._speed_log",
        SigId "motor._current_log",
        SigId "motor._voltage_log"
        ]

-- Building Signal Record for better Plotting of the original signals
electric:: [SigId]
electric = [SigId "_vehicleSpeed_log",
          SigId "battery1._poleVoltage_log",
          SigId "battery1._poleCurrent_log",
          SigId "motor._current_log",
          SigId "motor._voltage_log",
          SigId "generator._current_log",
          SigId "generator._voltage_log"
          ]

-- Building Signal Record for better Plotting of the original signals
battery:: [SigId]
battery  = [SigId "_vehicleSpeed_log",
          SigId "battery1.poleVoltage_log",
          SigId "battery1.poleCurrent_log",
          SigId "battery1.onternalVoltage_log"
--          SigId "_batteryInternalCurrent_log"
          ]


-- Building Signal Record for better Plotting of the original signals
generator:: [SigId]
generator =  [SigId "_vehicleSpeed_log",
            SigId "generator._current_log",
            SigId "generator._voltage_log",
            SigId "generator._torque_log",
            SigId "generator._speed_log",
            SigId "engine1._speed_log"
            ]


xyEngine :: (SigId,SigId)
xyEngine = (SigId "engine1._speed_log", SigId "engine1._torque_log")

xyGenerator :: (SigId,SigId)
xyGenerator = (SigId "engine1._speed_log", SigId "generator._torque_log")

xyMotor :: (SigId,SigId)
xyMotor = (SigId "motor._speed_log", SigId "motor._torque_log")