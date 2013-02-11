{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Modules.Plots where

import Data.Map as M (mapKeys)
import EFA.Utility(checkedLookup)
import EFA.Signal.Record ()
import qualified EFA.Signal.Plot as PL
import EFA.Signal.Typ (Typ,A,T,P,Tt)
import EFA.Report.Typ(TDisp)

import EFA.Signal.Signal(Signal)
import EFA.Signal.Vector as V (Walker,Storage,FromList)
-- import qualified EFA.Signal.Plot as PL


import EFA.Signal.Record (SigId(..),Record(..),PowerRecord,SignalRecord)
import qualified Graphics.Gnuplot.Value.Atom as Atom (C)
import qualified Graphics.Gnuplot.Value.Tuple as Tuple (C)

import Modules.System as System

---------------------------------------------------------------------------------------
  -- | * Group Signals for Plotting

class (Fractional a,
           Show (v a),
           V.FromList v,
           V.Walker v,
           V.Storage v a,
           Atom.C a,
           Tuple.C a) =>  Plottable v a where                             
instance Plottable [] Double where    
  
  
plot:: (TDisp t1,
        TDisp t2,
        Ord id,
        Show id,
        Plottable v a) =>
       String -> Record s t1 t2 id v a -> [id] -> IO()  
plot title rec sigIds = do PL.rPlotSelect sigIds (title,rec)
                           
                           
-- Building Signal Record for better Plotting of the original signals 
vehicle ::(Plottable v a) =>
          SignalRecord v a -> IO()                           
vehicle rec = plot "Vehicle" rec [SigId "speedsensor1.v",
                                  SigId "idealrollingwheel1.flangeR.tau",
                                  SigId "idealrollingwheel2.flangeR.tau",
                                  SigId "brake1.tau",
                                  SigId "brake2.tau",
                                  SigId "drivingresistance1.force1.f"
                                 ]

-- Building Signal Record for better Plotting of the original signals 
driveline:: Plottable v a =>  SignalRecord v a -> IO()
driveline rec =  plot "DriveLine" rec [SigId "speedsensor1.v",
                                       SigId "electricmotor1.flange_a.tau",
                                       SigId "gearbox1.flange_a.tau",                        
                                       SigId "gearbox1.flange_b.tau"
                                      ]
             
-- Building Signal Record for better Plotting of the original signals 
motor:: Plottable v a =>  SignalRecord v a -> IO()
motor rec = plot "Motor" rec [SigId "speedsensor1.v",                        
                              SigId "electricmotor1.flange_a.tau",
                              SigId "electricmotor1.speedsensor1.w",
                              SigId "electricmotor1.signalcurrent1.p.i",
                              SigId "electricmotor1.signalcurrent1.p.v"
                             ]
            
-- Building Signal Record for better Plotting of the original signals 
electric:: Plottable v a =>  SignalRecord v a -> IO()
electric rec =  plot "Electric" rec [SigId "speedsensor1.v",                        
                                 SigId "potentialsensor1.p.v",
                                 SigId "battery1.pin_p.i",
                                 SigId "electricmotor1.signalcurrent1.p.i",
                                 SigId "electricmotor1.signalcurrent1.p.v",
                                 SigId "electricmotor2.signalcurrent1.p.i",
                                 SigId "electricmotor2.signalcurrent1.v"
                                ]
            
-- Building Signal Record for better Plotting of the original signals 
battery:: Plottable v a =>  SignalRecord v a -> IO()
battery rec = plot "Battery" rec [SigId "speedsensor1.v",                        
                                  SigId "potentialsensor1.p.v",
                                  SigId "battery1.pin_p.i",
                                  SigId "battery1.constantvoltage1.v",
                                  SigId "battery1.constantvoltage1.i"
                                 ]
          

-- Building Signal Record for better Plotting of the original signals 
generator:: Plottable v a =>  SignalRecord v a -> IO()
generator rec = plot "Generator" rec [SigId "speedsensor1.v",                        
                                      SigId "electricmotor2.signalcurrent1.p.i",
                                      SigId "electricmotor2.signalcurrent1.v",
                                      SigId "electricmotor2.flange_a.tau",
                                      SigId "electricmotor2.speedsensor1.w",
                                      SigId "engine1.Speed",
                                      SigId "engine1.Speed"                       
                                     ]
  
 
-- Plot Power Records with readible 
mkPlotPowers :: Show (v a) => PowerRecord Nodes v a -> Record Signal (Typ A T Tt) (Typ A P Tt) SigId v a 
mkPlotPowers (Record time pMap) = Record time newMap
  where -- replace old with new keys
    newMap = M.mapKeys f pMap
    f key = checkedLookup System.powerPositonNames key
    
genPowers :: Plottable v a => PowerRecord Nodes v a -> IO()
genPowers pRec =  plot "GenerationPowers" (mkPlotPowers pRec) [SigId "Fuel",    
                                                             SigId "CrankShaft",
                                                             SigId "BatteryClamps",
                                                             SigId "BatteryCore",
                                                             SigId "Wire"
                                                            ]

propPowers :: (Show (v a), Plottable v a) => PowerRecord Nodes v a -> IO()
propPowers pRec = plot "PropulsionPowers" (mkPlotPowers pRec) [SigId "MotorClamps",
                                                             SigId "MotorFlange",    
                                                             SigId "InShaft",
                                                             SigId "OutShaft",
                                                             SigId "ToFrontBrakes",
                                                             SigId "FrontWheelHub",
                                                             SigId "FrontTires"
                                                              ]
                  
vehPowers :: (Show (v a), Plottable v a) => PowerRecord Nodes v a -> IO()
vehPowers pRec = plot "VehiclePowers"  (mkPlotPowers pRec) [SigId "ToFrontBrakes",
                                                          SigId "RearTires",    
                                                          SigId "ToInertia",
                                                          SigId "ToResistance"
                                                         ]


