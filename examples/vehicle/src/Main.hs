{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Foldable (foldMap)
import qualified EFA.Graph.Topology.StateAnalysis as StateAnalysis

import qualified Data.Map as M

import EFA.Example.Utility (edgeVar, makeEdges, (.=))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))
import EFA.Utility(checkedLookup)

import qualified EFA.Graph as Gr
import qualified EFA.Graph.Topology as TD
import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.System as EqGen

import EFA.IO.ASCIIImport (modelicaASCIIImport)
import EFA.IO.CSVImport (modelicaCSVImport)
import qualified EFA.Signal.SequenceData as SD
import qualified EFA.Signal.Plot as PL
import EFA.Signal.Sequence (makeSequence, makeSequenceRaw,makeSeqFlowGraph, genSequ,addZeroCrossings,removeZeroTimeSections )
import qualified EFA.Signal.Signal as Sig
import EFA.Signal.Signal((.*),(.+),(./),(.-),neg)

import qualified EFA.Report.Report as Rep

import qualified EFA.Signal.Base as SB

import EFA.Graph.Draw -- (drawTopology)

import Data.Monoid ((<>))

import Debug.Trace


sec0, sec1, sec2, sec3, sec4 :: Idx.Section
sec0 :~ sec1 :~ sec2 :~ sec3 :~ sec4 :~ _ = Stream.enumFrom $ Idx.Section 0

tank, con_engine, con_battery, battery, con_evs, con_motor, con_frontBrakes, con_chassis, drivingResistance, electricSystem, frontBrakes, vehicleInertia, con_frontBrakes, rearBrakes :: Idx.Node
tank :~ con_engine :~ con_battery :~ battery :~ con_evs :~ con_motor :~ con_frontBrakes :~ con_chassis :~ drivingResistance :~ electricSystem :~ frontBrakes :~ vehicleInertia :~ rearBrakes :~ _ = Stream.enumFrom $ Idx.Node 0


(!?) = checkedLookup

topo :: TD.Topology
topo = Gr.mkGraph ns (makeEdges es)
  where ns = [(tank, TD.Source),
              (con_engine , TD.Crossing),
              (con_battery, TD.Crossing),
              (battery, TD.Storage),
              (con_evs, TD.Crossing),  -- electric crossing to vehicle electric system
              (con_motor, TD.Crossing),  -- connection motor / gearbox
              (con_frontBrakes, TD.Crossing),  -- connection brakes
              (con_chassis, TD.Crossing),  -- connection inertia
              (drivingResistance, TD.Sink),      -- driving resistance                  
              (electricSystem, TD.Sink),      -- vehicle electric system
              (frontBrakes, TD.Sink),     -- vehicle brakes
              (rearBrakes, TD.Sink),     -- vehicle brakes
              (vehicleInertia, TD.Storage)]  -- vehicle inertia
             
        es = [(tank, con_engine),  -- ic engine
              (con_engine, con_battery),  -- generator
              (con_battery, con_evs),  -- electric connection        
              (con_evs, con_motor),  -- motor
              (con_motor, con_frontBrakes),  -- gearbox 
              (con_frontBrakes, con_chassis),  -- front wheels             
              (con_chassis, drivingResistance),  -- driving resistance
              (con_battery, battery),  -- battery              
              (con_evs, electricSystem),  -- dcdc          
              (con_frontBrakes, frontBrakes), -- brakes
              (con_chassis,rearBrakes), -- brakes
              (con_chassis, vehicleInertia)] -- inertia             


makeGiven initStorage xs =
  (EqGen.dtime Idx.initSection .= 1)
  <> (EqGen.storage (Idx.SecNode Idx.initSection battery) .= initStorage) 
  <> foldMap f (zip [Idx.Section 0 ..] xs)
  where f (sec, (t, ps)) =
          (EqGen.dtime sec .= t)
          <> foldMap g ps
          where g (SD.PPosIdx a b, p) = edgeVar EqGen.energy sec a b .= p*t

main :: IO ()
main = do
  
  -- ## import signals
  rec <- modelicaCSVImport "Vehicle_res_short.csv" :: IO (SD.Record [] Double)
  
  -- Get imported Signals
  let SD.Record time sigMap = rec 
      sigIDList = M.toList sigMap

  -- Plot imported Signals
  -- mapM_ (\ (x,y) -> PL.xyplot (show x) time y) (take 50 $ drop 50 sigIDList)
  -- mapM_ (\ (x,_) -> putStrLn(show x))  sigIDList -- sigIDList 
 --  PL.rPlot ("Roh",rec)  
  

-- ## Identify Signale and calculate Power Signals
  
  -- engine crankshaft
  let engineSpeed = sigMap !? (SD.SigId "engine1.Speed")
      engineTorque = neg $ sigMap !? (SD.SigId "engine1.flange_b.tau")
      
      fuelPower = sigMap !? (SD.SigId "engine1.FuelPower")
      engineMechPower = engineSpeed.*engineTorque :: Sig.UTSigL
  
  -- generator
      generatorSpeed = sigMap !? (SD.SigId "electricmotor2.speedsensor1.w")
      generatorTorque =  sigMap !? (SD.SigId "electricmotor2.flange_a.tau")
      generatorCurrent = sigMap !? (SD.SigId "electricmotor2.signalcurrent1.p.i")
      generatorVoltage = sigMap !? (SD.SigId "electricmotor2.signalcurrent1.p.v")
      
      generatorMechPower =  generatorSpeed .* generatorTorque
      generatorElectricPower =  generatorCurrent .* generatorVoltage
  
  -- battery
      batteryPoleVoltage = sigMap !? (SD.SigId "battery1.pin_p.v")
      batteryPoleCurrent = sigMap !? (SD.SigId "battery1.pin_p.i")                      
      batteryInnerVoltage = sigMap !? (SD.SigId "battery1.constantvoltage1.v")                      
      batteryInnerCurrent = sigMap !? (SD.SigId "battery1.constantvoltage1.i")                      
      
      batteryPolePower = batteryPoleVoltage.*batteryPoleCurrent
      batteryInnerPower = batteryInnerCurrent.*batteryInnerVoltage
      
  -- dcdc      
      dcdcPowerHV = Sig.convert $ Sig.untype $ time -- .*(Sig.toScalar 0)      
      dcdcPowerLV = Sig.convert $ Sig.untype $ time -- .*(Sig.toScalar 0) 
      
  -- motor    
      motorSpeed = sigMap !? (SD.SigId "electricmotor1.speedsensor1.w")
      motorTorque =  sigMap !? (SD.SigId "electricmotor1.flange_a.tau") 
      motorCurrent = sigMap !? (SD.SigId "electricmotor1.signalcurrent1.p.i")
      motorVoltage = sigMap !? (SD.SigId "electricmotor1.signalcurrent1.p.v")
      
      motorMechPower =  motorSpeed .* motorTorque
      motorElectricPower =  motorCurrent.* motorVoltage
       
  -- gearbox
      gearboxTorqueIn = sigMap !? (SD.SigId "gearbox1.flange_a.tau")
      gearboxSpeedIn = sigMap !? (SD.SigId "gearbox1.inertia1.w")
      
      gearboxTorqueOut = sigMap !? (SD.SigId "gearbox1.flange_b.tau")
      gearboxSpeedOut = sigMap !? (SD.SigId "gearbox1.inertia2.w")
      
      gearboxPowerIn = gearboxTorqueIn.*gearboxSpeedIn
      gearboxPowerOut = gearboxTorqueOut.*gearboxSpeedOut
        
                        
  -- brake 1
      brakePowerFront = sigMap !? (SD.SigId "brake1.lossPower")
      brakeSpeedFront = sigMap !? (SD.SigId "brake1.w")

  -- brake 2    
      brakePowerRear = sigMap !? (SD.SigId "brake2.lossPower")
      brakeSpeedRear = sigMap !? (SD.SigId "brake2.w")
        
      
  -- wheel 1    
      wheelTorqueFront = sigMap !? (SD.SigId "idealrollingwheel1.flangeR.tau")
      wheelSpeedFront = brakeSpeedFront
      
      wheelForceFront = sigMap !? (SD.SigId "idealrollingwheel1.flangeT.f")
      
      wheelHubPowerFront = wheelTorqueFront.*wheelSpeedFront
      tirePowerFront = wheelForceFront.*speed
      
  -- wheel2    
      wheelTorqueRear = sigMap !? (SD.SigId "idealrollingwheel2.flangeR.tau")
      wheelSpeedRear = brakeSpeedRear
      
      wheelForceRear = sigMap !? (SD.SigId "idealrollingwheel2.flangeT.f")

      wheelHubPowerRear = wheelTorqueRear.*wheelSpeedRear
      tirePowerRear = wheelForceRear.*speed      
                      
      
 -- chassis       
      frontAxleForce =  sigMap !? (SD.SigId "chassis1.flange_a.f")
      rearAxleForce =  sigMap !? (SD.SigId "chassis1.flange_a1.f")                  
      speed = sigMap !? (SD.SigId "speedsensor1.v") 
      frontAxlePower =  frontAxleForce.*speed        
      rearAxlePower =  rearAxleForce.*speed
      kineticPower = (frontAxlePower.+rearAxlePower).-resistancePower
                      
-- driving resistance              
      resistanceForce = sigMap !? (SD.SigId "drivingresistance1.force1.f")
      resistancePower = speed.* resistanceForce                 
  {-
  PL.xyplot "engineTorque" time engineTorque      
  PL.xyplot "engineSpeed" time engineSpeed  
  PL.xyplot "fuelPower" time fuelPower
   
  PL.xyplot "generatorTorque" time generatorTorque  
  PL.xyplot "generatorSpeed" time generatorSpeed    
    
  PL.xyplot "motorTorque" time motorTorque  
  PL.xyplot "motorSpeed" time motorSpeed    
    
  PL.xyplot "batteryPoleVoltage" time batteryPoleVoltage  
  PL.xyplot "batteryPoleCurrent" time batteryPoleCurrent   
  PL.xyplot "batteryInnerVoltage" time batteryInnerVoltage  

  -}
  -- ## Calculate Power Signals  


  -- ## Populate Power Record
  let pRec :: SD.PowerRecord [] Double
      pRec = SD.PowerRecord (Sig.fromList $ Sig.toList time) 
                            (M.map (Sig.fromList . Sig.toList) pMap)
             
      -- setEdgePowers       
      -- setEdgePowers :: Idx.Node Idx.Node Sig.UTSigL Sig.UTSigL
      setEdgePowers node1 node2 x y =  [(SD.PPosIdx node1 node2, x),
                                        (SD.PPosIdx node2 node1, y)]
      
      f = setEdgePowers 
      
      -- Vorschläge:
      --   Edge-Synonyme einführen, die einem Namen entsprechen
      --   eine beladungsoperation pro Kante (setEdgePowers)
      --   Kantentypen mit Wirkungsgrad 1, eventuell mit stateanalysis true or false
      
      pMap = M.fromList (concat pList)
          
      pList = [f tank con_engine fuelPower engineMechPower, -- engine
               f con_engine con_battery generatorMechPower generatorElectricPower, -- generator
               f con_battery con_evs (batteryPolePower.-generatorElectricPower) (batteryPolePower.-generatorElectricPower), -- connection
               f con_evs con_motor motorElectricPower motorMechPower, --motor
               f con_motor con_frontBrakes gearboxPowerIn gearboxPowerOut, -- gearbox 
               f con_frontBrakes con_chassis wheelHubPowerFront tirePowerFront, -- front wheels
               f con_chassis drivingResistance resistancePower resistancePower, -- driving Resistance
               
               f con_battery battery batteryPolePower batteryInnerPower, -- battery 
               f con_evs electricSystem dcdcPowerHV dcdcPowerLV , -- DCDC
               f con_frontBrakes frontBrakes brakePowerFront brakePowerFront, -- Brake 
               f con_chassis rearBrakes brakePowerRear brakePowerRear, --rearbrake
               f con_chassis vehicleInertia kineticPower kineticPower] --kinetic power 
                
  -- PL.rPlot ("Record",rec) 
  -- print pRec  
  
  -- mapM_ (\ (x,y) -> PL.xyplot (show x) time y) (concat pList)
  
  -- ## Analysis
  let sol = StateAnalysis.advanced topo
  
      -- seq = chopAtZeroCrossingsPowerRecord pRec
      -- sequFRec = makeSequence pRec
      
      (sequ, sequPRec) = genSequ  $ addZeroCrossings pRec
      (sequFilt, SD.SequData l) = makeSequenceRaw pRec  -- removeZeroTimeSections $ genSequ  $ addZeroCrossings pRec
      
      (SD.Sequ sList) = sequ
      (SD.Sequ sfList) = sequFilt
      
      ds =  map f l
      f (SD.PowerRecord t ds) =
        (sum $ Sig.toList t, M.toList $ M.map (sum . Sig.toList) ds)

      -- SD.Sequ s = sequ
      -- sequTopo = makeSeqFlowGraph topo sequFRec
      -- env = EqGen.solve (makeGiven 12.34567 ds)  sequTopo

  -- print env
  -- print sequ
  -- putStrLn ("Number of flow states: " ++ show (length sol))
  -- drawTopologyXs' [head sol]
  --Rep.report [Rep.RAll] ("Test",pRec)    
  Rep.report [] ("Sequenz",sequ)    
  print sequ
  Rep.report [] ("Sequenz",sequFilt)    
  print sequFilt
  --Rep.report sequ
  --print $ length sList
--   print sequPRec
  --print ""
  --print sequFilt
  --print $ length sfList
  -- print sequPRecFilt
  -- Rep.report [] ("SequencePowerRecord", sequPRec)
  -- drawTopology sequTopo env
  --drawTopologySimple sequTopo
  --print sequTopo
  --PL.rPlot ("Sequ", sequ)
  -- print sequ
  -- Rep.report [Rep.RAll] ("Test",pRec)
  -- putStrLn "Servus!"
  -- PL.rPlot ("Sequ", sequPRec)