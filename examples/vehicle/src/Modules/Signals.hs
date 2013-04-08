{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}


module Modules.Signals where

import qualified EFA.Example.Index as XIdx

import EFA.Signal.Record (SigId(SigId),
                          getSig,getTime,extractLogSignals, 
                          PowerRecord, SignalRecord, genPowerRecord)

-- import qualified EFA.Graph.Topology.Node as Node
import EFA.Signal.Signal((.*), (.-), neg, TC, Signal, len, fromList)
import EFA.Signal.Typ(UT,Typ)
import EFA.Signal.Data(Data(..),Nil, (:>))

import Modules.System  (Node(..))

{-
  vehicleSpeed_o = speedsensor1.v; - OK
  engineSpeed_o = speedsensor2.w;  --- WRONG
  engineTorque_o = engine1.flange_b.tau; - OK
  fuelPower_o = engine1.FuelPower; - OK
  generatorSpeed_o = electricmotor2.speedsensor1.w; --OK
  generatorTorque_o = electricmotor2.flange_a.tau; -- OK
  generatorVoltage_o = electricmotor2.signalcurrent1.v;
  generatorCurrent_o = electricmotor2.signalcurrent1.p.i; -- None
  batteryPoleCurrent_o = battery1.pin_p.i;
  batteryPoleVoltage_o = potentialsensor1.p.v;
  batteryInternalCurrent_o = battery1.constantvoltage1.i;
  batteryInternalVoltage_o = battery1.constantvoltage1.v;
  motorSpeed_o = electricmotor1.speedsensor1.w;
  motorTorque_o = electricmotor1.flange_a.tau;
  motorCurrent_o = electricmotor1.signalcurrent1.p.i;
  motorVoltage_o = electricmotor1.signalcurrent1.p.v;
  gearboxTorqueIS_o = gearbox1.flange_a.tau;
  gearboxSpeedIS_o = gearbox1.inertia1.w;
  gearboxTorqueOS_o = gearbox1.flange_b.tau;
  gearboxSpeedOS_o = gearbox1.inertia2.w;
  frontBrakesSpeed_o = brake1.w;
  frontBrakesTorque_o = brake1.tau;
  frontBrakesLossPower_o = brake1.lossPower;
  rearBrakesSpeed_o = brake2.w;
  rearBrakesTorque_o = brake2.tau;
  rearBrakesLossPower_o = brake2.lossPower;
  frontWheelsTorque_o = idealrollingwheel1.flangeR.tau;
  frontWheelsForce_o = idealrollingwheel1.flangeT.f;
  rearWheelsTorque_o = idealrollingwheel2.flangeR.tau;
  rearWheelsForce_o = idealrollingwheel2.flangeT.f;
  chassisPropulsionForce_o = chassis1.flange_a1.f;

  chassisResistanceForce_o = chassis1.drivingresistance1.force1.f;
-}

---------------------------------------------------------------------------------------
-- * Model the System Topology

condition :: SignalRecord [] Double -> SignalRecord [] Double
condition rec = extractLogSignals rec
                [(SigId "engineSpeed_o",id),
                 (SigId "engineTorque_o",neg),
                 (SigId "fuelPower_o",id),
                 (SigId "generatorSpeed_o",id),
                 (SigId "generatorCurrent_o",neg),
                 (SigId "generatorVoltage_o",neg),
                 (SigId "generatorTorque_o",id),
                 (SigId "batteryPoleCurrent_o",id),
                 (SigId "batteryPoleVoltage_o",id),
                 (SigId "batteryInternalVoltage_o",neg),
                 (SigId "batteryInternalCurrent_o",id),
                 (SigId "motorSpeed_o",id),
                 (SigId "motorTorque_o",neg),
                 (SigId "motorCurrent_o",id),
                 (SigId "motorVoltage_o",id),
                 (SigId "gearboxTorqueIS_o",id),
                 (SigId "gearboxSpeedIS_o",id),
                 (SigId "gearboxTorqueOS_o",neg),
                 (SigId "gearboxSpeedOS_o",id),
                 (SigId "frontBrakesLossPower_o",id),
                 (SigId "frontBrakesSpeed_o",id),
                 (SigId "rearBrakesLossPower_o",id),
                 (SigId "rearBrakesSpeed_o",id),
                 (SigId "frontWheelsTorque_o",id),
                 (SigId "frontWheelsForce_o",neg),
                 (SigId "rearWheelsTorque_o",neg),
                 (SigId "rearWheelsForce_o",id),
                 (SigId "chassisPropulsionForce_o",id),
                 (SigId "vehicleSpeed_o",id),
                 (SigId "chassisResistanceForce_o",neg),
                 (SigId "frontBrakesTorque_o",neg),
                 (SigId "rearBrakesTorque_o",neg)]

---------------------------------------------------------------------------------------
-- * Calculate special signals

calculatePower :: SignalRecord [] Double -> PowerRecord Node [] Double
calculatePower rec = pRec
  where
      -- Convenience function
      g sigId = getSig rec $ SigId sigId

      time = getTime rec
      zeroSig = fromList (replicate (len time) 0) :: TC Signal (Typ UT UT UT) (Data ([] :> Nil) Double)

      speed = g "vehicleSpeed_o"
      voltage = g "batteryPoleVoltage_o"


      -- generator
      generatorElectricPower =  g "generatorCurrent_o" .* voltage

      -- battery
      batteryClampsPower = voltage.*g "batteryPoleCurrent_o"
      batteryInternalVoltage = fromList (replicate (len time) 200) :: TC Signal (Typ UT UT UT) (Data ([] :> Nil) Double)
      batteryInternalPower = g "batteryPoleCurrent_o".*batteryInternalVoltage

      -- dcdc -- TODO !!
      dcdcPowerHV = zeroSig
      dcdcPowerLV = zeroSig

       -- chassis
      kineticPower = (g "chassisPropulsionForce_o" .- g "chassisResistanceForce_o").*speed


---------------------------------------------------------------------------------------
-- ## Build Power Record

      pRec = genPowerRecord time

              -- engine
             [(XIdx.ppos Tank ConBattery,
               g "fuelPower_o",
               generatorElectricPower
              ),

              -- connection
              (XIdx.ppos ConBattery ConES,
               generatorElectricPower .- batteryClampsPower,
               generatorElectricPower .- batteryClampsPower
              ),

              -- --motor
              (XIdx.ppos ConES ConFrontBrakes,
               (g "motorCurrent_o".* voltage),
               g "gearboxTorqueOS_o".* g "gearboxSpeedOS_o"
              ),

              -- front wheels
              (XIdx.ppos ConFrontBrakes Chassis,
               g "frontWheelsTorque_o".* g "frontBrakesSpeed_o",
               g "frontWheelsForce_o".* speed
              ),

              -- driving Resistance
              (XIdx.ppos Chassis Resistance,
               g "chassisResistanceForce_o".* speed,
               g "chassisResistanceForce_o".* speed
              ),

              -- battery
              (XIdx.ppos ConBattery Battery,
               batteryClampsPower,
               batteryInternalPower
              ),

              -- DCDC
              (XIdx.ppos ConES ElectricSystem,
               dcdcPowerHV,
               dcdcPowerLV
              ),

              -- Front brake
              (XIdx.ppos ConFrontBrakes FrontBrakes,
               g "frontBrakesLossPower_o",
               g "frontBrakesLossPower_o"
              ),

              --Rear brake
              (XIdx.ppos Chassis RearBrakes,
               g "rearWheelsTorque_o".* g "rearBrakesSpeed_o",
               g "rearWheelsForce_o".* speed
              ),

              --kinetic power
              (XIdx.ppos Chassis VehicleInertia,
               kineticPower,
               kineticPower

              )]


---------------------------------------------------------------------------------------
-- ## Signalgroups for Plotting


vehPowers :: [XIdx.PPos Node]
vehPowers = [XIdx.ppos Chassis VehicleInertia,
             XIdx.ppos Chassis RearBrakes,
             XIdx.ppos ConFrontBrakes FrontBrakes,
             XIdx.ppos Chassis Resistance]


-- Building Signal Record for better Plotting of the original signals
vehicle :: [SigId]
vehicle = [SigId "vehicleSpeed_o",
           SigId "frontWheelsTorque_o",
           SigId "rearWheelsTorque_o",
           SigId "frontBrakesTorque_o",
           SigId "rearBrakesTorque_o",
           SigId "chassisResistanceForce_o",
           SigId "PPosIdx Chassis VehicleInertia"
          ]

-- Building Signal Record for better Plotting of the original signals
driveline:: [SigId]
driveline = [SigId "vehicleSpeed_o",
             SigId "motorTorque_o",
             SigId "gearboxTorqueIS_o",
             SigId "gearboxTorqueOS_o"
            ]

-- Building Signal Record for better Plotting of the original signals
motor:: [SigId]
motor = [SigId "vehicleSpeed_o",
        SigId "motorTorque_o",
        SigId "motorSpeed_o",
        SigId "motorCurrent_o",
        SigId "motorVoltage_o"
        ]

-- Building Signal Record for better Plotting of the original signals
electric:: [SigId]
electric = [SigId "vehicleSpeed_o",
          SigId "batteryPoleVoltage_o",
          SigId "batteryPoleCurrent_o",
          SigId "motorCurrent_o",
          SigId "motorVoltage_o",
          SigId "generatorCurrent_o",
          SigId "generatorVoltage_o"
          ]

-- Building Signal Record for better Plotting of the original signals
battery:: [SigId]
battery  = [SigId "vehicleSpeed_o",
          SigId "batteryPoleVoltage_o",
          SigId "batteryPoleCurrent_o",
          SigId "batteryInternalVoltage_o"
--          SigId "batteryInternalCurrent_o"
          ]


-- Building Signal Record for better Plotting of the original signals
generator:: [SigId]
generator =  [SigId "vehicleSpeed_o",
            SigId "generatorCurrent_o",
            SigId "generatorVoltage_o",
            SigId "generatorTorque_o",
            SigId "generatorSpeed_o",
            SigId "engineSpeed_o"
            ]


xyEngine :: (SigId,SigId)
xyEngine = (SigId "engineSpeed_o", SigId "engineTorque_o")

xyGenerator :: (SigId,SigId)
xyGenerator = (SigId "engineSpeed_o", SigId "generatorTorque_o")

xyMotor :: (SigId,SigId)
xyMotor = (SigId "motorSpeed_o", SigId "motorTorque_o")
