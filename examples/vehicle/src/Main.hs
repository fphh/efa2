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
import EFA.Signal.Record (SigId(SigId), PPosIdx(PPosIdx),PowerCalc(Extra),PowerCalc (Take), PowerCalc(Mult), SignalOps(Negate),SignalRecord,getSig,getTime,selectRecord, extractLogSignals, SignalRecord(..), generatePowerRecord, FlowRecord(..))
import qualified EFA.Signal.Plot as PL
import EFA.Signal.Sequence 
  (makeSequenceRaw,makeSeqFlowGraph, 
   removeZeroTimeSections, 
   removeLowEnergySections,genSequFlow)
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Signal((.*),(.+),(.-),neg)

import qualified EFA.Report.Report as Rep

import qualified EFA.Graph.Draw as Draw-- (drawTopology)
import qualified EFA.Graph.Topology.Nodes as N

import Data.Monoid ((<>))


 


---------------------------------------------------------------------------------------
-- ## Model the System Topology

-- Define Section Names
sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

data Nodes = Tank | Con_engine | Con_battery | Battery | Con_evs | Con_motor | Con_frontBrakes | Con_chassis | DrivingResistance | ElectricSystem | FrontBrakes | VehicleInertia | RearBrakes deriving (Eq, Ord, Show)

instance N.ShowNode Nodes where
         showNode = show

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
  
  let recConditioned = extractLogSignals rec [(SigId "engine1.Speed",[]),
                                                 (SigId "engine1.flange_b.tau",[Negate]),
                                                 (SigId "engine1.FuelPower",[]),
                                                 (SigId "electricmotor2.speedsensor1.w",[]),
                                                 (SigId "electricmotor2.signalcurrent1.p.i",[Negate]),
                                                 (SigId "electricmotor2.signalcurrent1.v",[Negate]),
                                                 (SigId "electricmotor2.flange_a.tau",[]),
                                                 (SigId "battery1.pin_p.v",[]),
                                                 (SigId "battery1.pin_p.i",[]),
                                                 (SigId "battery1.constantvoltage1.v",[Negate]),
                                                 (SigId "battery1.constantvoltage1.i",[]),
                                                 (SigId "electricmotor1.speedsensor1.w",[]),
                                                 (SigId "electricmotor1.flange_a.tau",[Negate]),
                                                 (SigId "electricmotor1.signalcurrent1.p.i",[]),
                                                 (SigId "electricmotor1.signalcurrent1.p.v",[]),
                                                 (SigId "gearbox1.flange_a.tau",[]),
                                                 (SigId "gearbox1.inertia1.w",[]),
                                                 (SigId "gearbox1.flange_b.tau",[Negate]),
                                                 (SigId "gearbox1.inertia2.w",[]),
                                                 (SigId "brake1.lossPower",[]),
                                                 (SigId "brake1.w",[]),
                                                 (SigId "brake2.lossPower",[]),
                                                 (SigId "brake2.w",[]),
                                                 (SigId "idealrollingwheel1.flangeR.tau",[]),
                                                 (SigId "idealrollingwheel1.flangeT.f",[Negate]),
                                                 (SigId "idealrollingwheel2.flangeR.tau",[Negate]),
                                                 (SigId "idealrollingwheel2.flangeT.f",[]),
                                                 (SigId "chassis1.flange_a.f",[Negate]),
                                                 (SigId "chassis1.flange_a1.f",[]),
                                                 (SigId "speedsensor1.v",[]),
                                                 (SigId "drivingresistance1.force1.f",[Negate])]
  
--------------------------------------------------------------------------------------- 
-- ## Calculate extra Signals and build SignalRecord
  
      time = getTime rec
      
      get = getSig recConditioned
    
      -- generator
      generatorCurrent = get (SigId "electricmotor2.signalcurrent1.p.i")
      generatorVoltage = neg $ get (SigId "electricmotor2.signalcurrent1.v")
      generatorElectricPower =  generatorCurrent .* generatorVoltage
   
      -- battery
      batteryPoleVoltage = get (SigId "battery1.pin_p.v")
      batteryPoleCurrent = get (SigId "battery1.pin_p.i")                      
      batteryPolePower = batteryPoleVoltage.*batteryPoleCurrent
      
      -- dcdc -- TODO !!     
      dcdcPowerHV = get (SigId "battery1.pin_p.i")
      dcdcPowerLV = get (SigId "battery1.pin_p.i")
      
      -- chassis       
      frontAxleForce =  get (SigId "chassis1.flange_a.f")
      rearAxleForce =  get (SigId "chassis1.flange_a1.f")                  
      speed = get (SigId "speedsensor1.v") 
      frontAxlePower =  frontAxleForce.*speed        
      rearAxlePower =  rearAxleForce.*speed
      kineticPower = (frontAxlePower.+rearAxlePower).-resistancePower
                      
 -- driving resistance
      resistanceForce = neg $ get (SigId "drivingresistance1.force1.f")
      resistancePower = speed.* resistanceForce                 
      
      recExtraSignals = SignalRecord time ( M.fromList [(SigId "connectionPower", batteryPolePower.-generatorElectricPower),
                                                   (SigId "kineticPower", kineticPower),
                                                   (SigId "dcdcPowerHV", dcdcPowerHV),
                                                   (SigId "dcdcPowerLV", dcdcPowerLV)
                                                  ])
  
        
--------------------------------------------------------------------------------------- 
-- ## Build Power Record
      
      pRec = generatePowerRecord recConditioned  recExtraSignals
      
              -- engine
              [(PPosIdx Tank Con_engine,                 
                [Take "engine1.Speed", Mult "engine1.flange_b.tau"],                
                [Take "engine1.FuelPower"]
               ),
               
              -- generator 
               (PPosIdx Con_engine Con_battery, 
                [Take "electricmotor2.speedsensor1.w", Mult "electricmotor2.flange_a.tau"],
                [Take "electricmotor2.signalcurrent1.p.i", Mult "electricmotor2.signalcurrent1.v"]
               ),
               
               -- connection
               (PPosIdx Con_battery Con_evs,
                [Extra "connectionPower"],
                [Extra "connectionPower"]
                ),
               
               --motor
               (PPosIdx Con_evs Con_motor,
                [Take "electricmotor1.speedsensor1.w", Mult "electricmotor1.flange_a.tau"],
                [Take "electricmotor1.signalcurrent1.p.i", Mult "electricmotor1.signalcurrent1.p.v"]
               ),
               
               -- gearbox
               (PPosIdx Con_motor Con_frontBrakes,
                [Take "gearbox1.flange_a.tau", Mult "gearbox1.inertia1.w"],
                [Take "gearbox1.flange_b.tau", Mult "gearbox1.inertia2.w"]
               ),
      
               -- front wheels
               (PPosIdx Con_frontBrakes Con_chassis,
                [Take "idealrollingwheel1.flangeR.tau", Mult "brake1.w"],
                [Take "idealrollingwheel1.flangeT.f", Mult "speedsensor1.v" ]
               ),
                 
               -- driving Resistance
               (PPosIdx Con_chassis DrivingResistance,
                [Take "drivingresistance1.force1.f", Mult "speedsensor1.v"],
                [Take "drivingresistance1.force1.f", Mult "speedsensor1.v"]
               ),
               
               -- battery
               (PPosIdx Con_battery Battery,
                [Take "battery1.pin_p.v", Mult "battery1.pin_p.i"],
                [Take "battery1.constantvoltage1.v", Mult "battery1.constantvoltage1.i"]
               ),
               
               -- DCDC
               (PPosIdx Con_evs ElectricSystem,
                [Extra "dcdcPowerHV"],
                [Extra "dcdcPowerLV"]
               ),
                 
               -- Front brake
               (PPosIdx Con_frontBrakes FrontBrakes,
                [Take "brake1.lossPower"],
                [Take "brake1.lossPower"]
               ),
               
               --Rear brake
               (PPosIdx Con_chassis RearBrakes,
                [Take "idealrollingwheel2.flangeR.tau", Mult "brake2.w"],
                [Take "idealrollingwheel2.flangeT.f", Mult "speedsensor1.v"]
               ),
               
               --kinetic power
               (PPosIdx Con_chassis VehicleInertia,
                [Extra "kineticPower"],
                [Extra "kineticPower"]
               )]
              

 
 ---------------------------------------------------------------------------------------
  -- ## Grouping Signals for Plotting
   
  -- Building Signal Record for better Plotting of the original signals 

      recVehicle = selectRecord recConditioned idlist
        where idlist = [SigId "speedsensor1.v",
                        SigId "idealrollingwheel1.flangeR.tau",
                        SigId "idealrollingwheel2.flangeR.tau",
                        SigId "brake1.tau",
                        SigId "brake2.tau",
                        SigId "drivingresistance1.force1.f"]

  -- Building Signal Record for better Plotting of the original signals 
      recDriveLine = selectRecord recConditioned idlist
        where idlist = [SigId "speedsensor1.v",
                        SigId "electricmotor1.flange_a.tau",
                        SigId "gearbox1.flange_a.tau",                        
                        SigId "gearbox1.flange_b.tau"
                       ]
  
  -- Building Signal Record for better Plotting of the original signals 
      recMotor = selectRecord recConditioned idlist
        where idlist = [SigId "speedsensor1.v",                        
                        SigId "electricmotor1.flange_a.tau",
                        SigId "electricmotor1.speedsensor1.w",
                        SigId "electricmotor1.signalcurrent1.p.i",
                        SigId "electricmotor1.signalcurrent1.p.v"
                       ]
  
  -- Building Signal Record for better Plotting of the original signals 
      recElectric = selectRecord recConditioned idlist
        where idlist = [SigId "speedsensor1.v",                        
                        SigId "battery1.pin_p.v",
                        SigId "battery1.pin_p.i",
                        SigId "electricmotor1.signalcurrent1.p.i",
                        SigId "electricmotor1.signalcurrent1.p.v",
                        SigId "electricmotor2.signalcurrent1.p.i",
                        SigId "electricmotor2.signalcurrent1.p.v"
                       ]
       
  -- Building Signal Record for better Plotting of the original signals 
      recBattery = selectRecord recConditioned idlist
        where idlist = [SigId "speedsensor1.v",                        
                        SigId "battery1.pin_p.v",
                        SigId "battery1.pin_p.i",
                        SigId "battery1.constantvoltage1.v",
                        SigId "battery1.constantvoltage1.i"
                       ]
  
  -- Building Signal Record for better Plotting of the original signals 
      recGenerator = selectRecord recConditioned idlist
        where idlist = [SigId "speedsensor1.v",                        
                        SigId "electricmotor2.signalcurrent1.p.i",
                        SigId "electricmotor2.signalcurrent1.v",
                        SigId "electricmotor2.flange_a.tau",
                        SigId "electricmotor2.speedsensor1.w",
                        SigId "engine1.Speed",
                        SigId "engine1.Speed"                       
                       ]
 
  PL.rPlot ("Vehicle Signals",recVehicle)   
  PL.rPlot ("DriveLine Signals",recDriveLine)   
  PL.rPlot ("Electric System Signals",recElectric)   
  PL.rPlot ("Motor Signals",recMotor)   
  PL.rPlot ("Battery Signals",recBattery)   
  PL.rPlot ("Generator and Engine Signals",recGenerator)   
  
  PL.rPlotSplit ("Record",recConditioned) 9
  PL.rPlotSplitPower ("Record",pRec) 9
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
              f2 (FlowRecord t xs) = (sum $ Sig.toList t, M.toList $ M.map (sum . Sig.toList) xs)      
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
--  PL.rPlot ("VehicleSignals",recVehicle)    
  

