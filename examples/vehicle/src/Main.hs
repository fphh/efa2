{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Foldable (foldMap)
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified Data.Map as M

import EFA.Example.Utility (edgeVar, makeEdges, (.=))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph as Gr
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.System as EqGen

import EFA.IO.CSVImport (modelicaCSVImport)
import qualified EFA.Signal.SequenceData as SD
-- import EFA.Signal.SequenceData () 
import EFA.Signal.Record (SigId(SigId), PPosIdx(PPosIdx),SignalRecord,getSig,getTime,extractLogSignals,Record(Record), SignalRecord, genPowerRecord)
import qualified EFA.Signal.Plot as PL
import EFA.Signal.Sequence 
  (makeSequenceRaw,makeSeqFlowGraph, 
   removeZeroTimeSections, 
   removeLowEnergySections,genSequFlow)
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Signal((.*),(.+),(.-),neg)

import qualified EFA.Report.Report as Rep

import qualified EFA.Graph.Draw as Draw-- (drawTopology)
import qualified EFA.Graph.Topology.Node as Node

import Data.Monoid ((<>))


 


---------------------------------------------------------------------------------------
-- ## Model the System Topology

-- Define Section Names
sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

data Nodes = Tank | Con_engine | Con_battery | Battery | Con_evs | Con_motor | Con_frontBrakes | Con_chassis | DrivingResistance | ElectricSystem | FrontBrakes | VehicleInertia | RearBrakes deriving (Eq, Ord, Show)

instance Node.Show Nodes

-- Define System Topology
topo :: TD.Topology Nodes
topo = Gr.mkGraph ns (makeEdges es)
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

sol :: [TD.FlowTopology Nodes]
sol = StateAnalysis.advanced topo    

main :: IO ()
main = do


---------------------------------------------------------------------------------------
 -- Show Topology
  
  -- drawTopologyXs' [head sol]
 
---------------------------------------------------------------------------------------
-- Topology State Analysis

  putStrLn ("Number of possible flow states: " ++ show (length sol))
  -- drawTopologyXs' (take 20 sol)
             
--------------------------------------------------------------------------------------- 
-- ## Read and condition signal from Csv-file
  
  rec <- modelicaCSVImport "Vehicle_res_short_neu.csv" :: IO (SignalRecord [] Double)
  
  let recConditioned = extractLogSignals rec [(SigId "engine1.Speed",id),
                                              (SigId "engine1.flange_b.tau",Sig.neg),
                                              (SigId "engine1.FuelPower",id),
                                              (SigId "electricmotor2.speedsensor1.w",id),
                                              (SigId "electricmotor2.signalcurrent1.p.i",Sig.neg),
                                              (SigId "electricmotor2.signalcurrent1.v",Sig.neg),
                                              (SigId "electricmotor2.flange_a.tau",id),
                                              (SigId "battery1.pin_p.v",id),
                                              (SigId "battery1.pin_p.i",id),
                                              (SigId "battery1.constantvoltage1.v",Sig.neg),
                                              (SigId "battery1.constantvoltage1.i",id),
                                              (SigId "electricmotor1.speedsensor1.w",id),
                                              (SigId "electricmotor1.flange_a.tau",Sig.neg),
                                              (SigId "electricmotor1.signalcurrent1.p.i",id),
                                              (SigId "electricmotor1.signalcurrent1.p.v",id),
                                              (SigId "gearbox1.flange_a.tau",id),
                                              (SigId "gearbox1.inertia1.w",id),
                                              (SigId "gearbox1.flange_b.tau",Sig.neg),
                                              (SigId "gearbox1.inertia2.w",id),
                                              (SigId "brake1.lossPower",id),
                                              (SigId "brake1.w",id),
                                              (SigId "brake2.lossPower",id),
                                              (SigId "brake2.w",id),
                                              (SigId "idealrollingwheel1.flangeR.tau",id),
                                              (SigId "idealrollingwheel1.flangeT.f",Sig.neg),
                                              (SigId "idealrollingwheel2.flangeR.tau",Sig.neg),
                                              (SigId "idealrollingwheel2.flangeT.f",id),
                                              (SigId "chassis1.flange_a.f",Sig.neg),
                                              (SigId "chassis1.flange_a1.f",id),
                                              (SigId "speedsensor1.v",id),
                                              (SigId "drivingresistance1.force1.f",Sig.neg)]
  
 --------------------------------------------------------------------------------------- 
-- ## Calculate special signals
  
      -- Convenience function
      g sigId = getSig recConditioned $ SigId sigId
  
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
              

 
 ---------------------------------------------------------------------------------------
  -- ## Grouping Signals for Plotting
   
  -- Building Signal Record for better Plotting of the original signals 


      vehicleSigs = [SigId "speedsensor1.v",
                     SigId "idealrollingwheel1.flangeR.tau",
                     SigId "idealrollingwheel2.flangeR.tau",
                     SigId "brake1.tau",
                     SigId "brake2.tau",
                     SigId "drivingresistance1.force1.f"]

  -- Building Signal Record for better Plotting of the original signals 
      driveLineSigs = [SigId "speedsensor1.v",
                       SigId "electricmotor1.flange_a.tau",
                       SigId "gearbox1.flange_a.tau",                        
                       SigId "gearbox1.flange_b.tau"
                       ]
  
  -- Building Signal Record for better Plotting of the original signals 
      motorSigs = [SigId "speedsensor1.v",                        
                   SigId "electricmotor1.flange_a.tau",
                   SigId "electricmotor1.speedsensor1.w",
                   SigId "electricmotor1.signalcurrent1.p.i",
                   SigId "electricmotor1.signalcurrent1.p.v"
                   ]
  
  -- Building Signal Record for better Plotting of the original signals 
      electricSigs =  [SigId "speedsensor1.v",                        
                       SigId "battery1.pin_p.v",
                       SigId "battery1.pin_p.i",
                       SigId "electricmotor1.signalcurrent1.p.i",
                       SigId "electricmotor1.signalcurrent1.p.v",
                       SigId "electricmotor2.signalcurrent1.p.i",
                       SigId "electricmotor2.signalcurrent1.p.v"
                       ]
       
  -- Building Signal Record for better Plotting of the original signals 
      batterySigs = [SigId "speedsensor1.v",                        
                    SigId "battery1.pin_p.v",
                    SigId "battery1.pin_p.i",
                    SigId "battery1.constantvoltage1.v",
                    SigId "battery1.constantvoltage1.i"
                   ]
  
  -- Building Signal Record for better Plotting of the original signals 
      generatorSigs = [SigId "speedsensor1.v",                        
                       SigId "electricmotor2.signalcurrent1.p.i",
                       SigId "electricmotor2.signalcurrent1.v",
                       SigId "electricmotor2.flange_a.tau",
                       SigId "electricmotor2.speedsensor1.w",
                       SigId "engine1.Speed",
                       SigId "engine1.Speed"                       
                      ]
 
  PL.rPlotSelect vehicleSigs ("Vehicle Signals",recConditioned)   
  PL.rPlotSelect driveLineSigs ("DriveLine Signals",recConditioned)    
  PL.rPlotSelect electricSigs ("Electric System Signals",recConditioned)    
  PL.rPlotSelect  motorSigs    ("Motor Signals",recConditioned)
  PL.rPlotSelect batterySigs  ("Battery Signals",recConditioned)   
  PL.rPlotSelect generatorSigs ("Generator and Engine Signals",recConditioned)    
  
  -- PL.rPlotSplit 9 ("Record",recConditioned)
  -- PL.rPlotSplit 9 ("Record",pRec)
  -- PL.xyplot "MotorPower" time motorMechPower
  -- PL.xyplot "MotorPower" time motorElectricPower
  
  
  -- PL.xyplot "Blah" time engineSpeed
  -- putStrLn $ disp engineSpeed
  -- Rep.report [] ("Test",recConditioned)

  Rep.report [Rep.RAll] ("PowerRecord",recConditioned)    
  Rep.report [Rep.RAll] ("PowerRecord",pRec)    
 
  ---------------------------------------------------------------------------------------
  -- ## Pre-Processing Signals 
  let 
  
      -- Add ZeroCrossings and Slice Power Signals in Sections
      (sequA,sequPRecA) = makeSequenceRaw pRec  
      
      -- Remove Sections with Zero Time Duration
      (sequB,sequPRecB)= removeZeroTimeSections (sequA,sequPRecA)
      -- ByPass
      -- (sequB,sequPRecB)= (sequA,sequPRecA)
      
      
      -- Integrate PowerRecords in Sections to FlowRecords
      sequFRecB = genSequFlow sequPRecB
       
      -- Drop Sections with negligible EnergyFlow using a given Threshold
      (sequ, sequPRec, sequFRec) = removeLowEnergySections (sequB,sequPRecB,sequFRecB) (100) 
      -- Bypass
      -- (sequ, sequPRec, sequFRec) = (sequB,sequPRecB,sequFRecB) 
      
  Rep.report [] ("Sequenz",sequ)    
  print sequ
  -- PL.rPlot ("Sequ", sequPRec)    
  Rep.report [] ("SequencePowerRecord", sequPRec)

  
  ---------------------------------------------------------------------------------------
   -- ## Provide solver with Given Variables, Start Solver and generate Sequence Flow Graph     
   
  let   
      makeGiven initStorage sqFlowRec = (EqGen.dtime Idx.initSection .= 1)  
                                       <> (EqGen.storage (Idx.SecNode Idx.initSection Battery) .= initStorage) 
                                       <> foldMap f (zip [Idx.Section 0 ..] ds)
        where 
              SD.SequData ds =  fmap f2 sqFlowRec
              f2 (Record t xs) = (sum $ Sig.toList t, M.toList $ M.map (sum . Sig.toList) xs)      
              f (sec, (dt, es)) = (EqGen.dtime sec .= dt) <> foldMap g es                                                          
                where g (PPosIdx a b, e) = (edgeVar EqGen.energy sec a b .= e)
      
   -- Generate Sequence Topology 
      sequTopo = makeSeqFlowGraph topo sequFRec
      
      
                
     -- Generate Given from Initial Storage and Sequence Flow            
      env = EqGen.solve (makeGiven 12.34567 sequFRec)  sequTopo
  
  -- print env

  -- Show Sequence Flow Graph
  Draw.sequFlowGraph sequTopo


  -- Show Sequence Flow with Numbers
  Draw.sequFlowGraphAbsWithEnv sequTopo env
  
  
  -- PL.rPlotSelect vehicleSigs ("Vehicle Signals",recConditioned)   
  
  print sequTopo

