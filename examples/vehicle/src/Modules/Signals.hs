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
                [(SigId "engine1.Speed",id),
                 (SigId "engine1.flange_b.tau",neg),
                 (SigId "engine1.FuelPower",id),
                 (SigId "electricmotor2.speedsensor1.w",id),
                 (SigId "electricmotor2.signalcurrent1.p.i",neg),
                 (SigId "electricmotor2.signalcurrent1.v",neg),
                 (SigId "electricmotor2.flange_a.tau",id),
                 (SigId "battery1.pin_p.i",id),
                 (SigId "potentialsensor1.p.v",id),
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
                 (SigId "chassis1.flange_a1.f",id),
                 (SigId "speedsensor1.v",id),
                 (SigId "chassis1.drivingresistance1.force1.f",neg),
                 (SigId "brake1.tau",neg),
                 (SigId "brake2.tau",neg)]

---------------------------------------------------------------------------------------
-- * Calculate special signals

calculatePower :: SignalRecord [] Double -> PowerRecord Node [] Double
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
      kineticPower = (g "chassis1.flange_a1.f" .- g "chassis1.drivingresistance1.force1.f").*speed


---------------------------------------------------------------------------------------
-- ## Build Power Record

      pRec = genPowerRecord time

              -- engine
             [(PPosIdx Tank ConBattery,
               g "engine1.FuelPower",
               generatorElectricPower
              ),

              -- connection
              (PPosIdx ConBattery ConES,
               generatorElectricPower .- batteryClampsPower,
               generatorElectricPower .- batteryClampsPower
              ),

              -- --motor
              (PPosIdx ConES ConFrontBrakes,
               (g "electricmotor1.signalcurrent1.p.i".* voltage),
               g "gearbox1.flange_b.tau".* g "gearbox1.inertia2.w"
              ),

              -- front wheels
              (PPosIdx ConFrontBrakes Chassis,
               g "idealrollingwheel1.flangeR.tau".* g "brake1.w",
               g "idealrollingwheel1.flangeT.f".* speed
              ),

              -- driving Resistance
              (PPosIdx Chassis Resistance,
               g "chassis1.drivingresistance1.force1.f".* speed,
               g "chassis1.drivingresistance1.force1.f".* speed
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


---------------------------------------------------------------------------------------
-- ## Signalgroups for Plotting


vehPowers :: [PPosIdx Node]
vehPowers = [PPosIdx Chassis VehicleInertia,
             PPosIdx Chassis RearBrakes,
             PPosIdx ConFrontBrakes FrontBrakes,
             PPosIdx Chassis Resistance]


-- Building Signal Record for better Plotting of the original signals
vehicle :: [SigId]
vehicle = [SigId "speedsensor1.v",
           SigId "idealrollingwheel1.flangeR.tau",
           SigId "idealrollingwheel2.flangeR.tau",
           SigId "brake1.tau",
           SigId "brake2.tau",
           SigId "chassis1.drivingresistance1.force1.f",
           SigId "PPosIdx Chassis VehicleInertia"
          ]

-- Building Signal Record for better Plotting of the original signals
driveline:: [SigId]
driveline = [SigId "speedsensor1.v",
             SigId "electricmotor1.flange_a.tau",
             SigId "gearbox1.flange_a.tau",
             SigId "gearbox1.flange_b.tau"
            ]

-- Building Signal Record for better Plotting of the original signals
motor:: [SigId]
motor = [SigId "speedsensor1.v",
        SigId "electricmotor1.flange_a.tau",
        SigId "electricmotor1.speedsensor1.w",
        SigId "electricmotor1.signalcurrent1.p.i",
        SigId "electricmotor1.signalcurrent1.p.v"
        ]

-- Building Signal Record for better Plotting of the original signals
electric:: [SigId]
electric = [SigId "speedsensor1.v",
          SigId "potentialsensor1.p.v",
          SigId "battery1.pin_p.i",
          SigId "electricmotor1.signalcurrent1.p.i",
          SigId "electricmotor1.signalcurrent1.p.v",
          SigId "electricmotor2.signalcurrent1.p.i",
          SigId "electricmotor2.signalcurrent1.v"
          ]

-- Building Signal Record for better Plotting of the original signals
battery:: [SigId]
battery  = [SigId "speedsensor1.v",
          SigId "potentialsensor1.p.v",
          SigId "battery1.pin_p.i",
          SigId "battery1.constantvoltage1.v"
--          SigId "battery1.constantvoltage1.i"
          ]


-- Building Signal Record for better Plotting of the original signals
generator:: [SigId]
generator =  [SigId "speedsensor1.v",
            SigId "electricmotor2.signalcurrent1.p.i",
            SigId "electricmotor2.signalcurrent1.v",
            SigId "electricmotor2.flange_a.tau",
            SigId "electricmotor2.speedsensor1.w",
            SigId "engine1.Speed"
            ]


xyEngine :: (SigId,SigId)
xyEngine = (SigId "engine1.Speed", SigId "engine1.flange_b.tau")

xyGenerator :: (SigId,SigId)
xyGenerator = (SigId "engine1.Speed", SigId "electricmotor2.flange_a.tau")

xyMotor :: (SigId,SigId)
xyMotor = (SigId "electricmotor1.speedsensor1.w", SigId "electricmotor1.flange_a.tau")