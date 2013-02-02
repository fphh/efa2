{-# LANGUAGE FlexibleContexts #-}

module EXAMPLES.Vehicle.SeriesHybrid.Plots where

import EFA.Signal.Record ()
import qualified EFA.Signal.Plot as PL
import EFA.Signal.Record (SigId(..))
---------------------------------------------------------------------------------------
  -- | * Group Signals for Plotting
  
  
plot title rec sigs = do PL.rPlotSelect sigs (title,rec)
   
-- Building Signal Record for better Plotting of the original signals 
vehicle rec = plot "Vehicle" rec [SigId "speedsensor1.v",
                                  SigId "idealrollingwheel1.flangeR.tau",
                                  SigId "idealrollingwheel2.flangeR.tau",
                                  SigId "brake1.tau",
                                  SigId "brake2.tau",
                                  SigId "drivingresistance1.force1.f"
                                 ]

-- Building Signal Record for better Plotting of the original signals 
driveline rec =  plot "DriveLine" rec [SigId "speedsensor1.v",
                                   SigId "electricmotor1.flange_a.tau",
                                   SigId "gearbox1.flange_a.tau",                        
                                   SigId "gearbox1.flange_b.tau"
                                  ]
             
-- Building Signal Record for better Plotting of the original signals 
motor rec = plot "Motor" rec [SigId "speedsensor1.v",                        
                          SigId "electricmotor1.flange_a.tau",
                          SigId "electricmotor1.speedsensor1.w",
                          SigId "electricmotor1.signalcurrent1.p.i",
                          SigId "electricmotor1.signalcurrent1.p.v"
                         ]
  
-- Building Signal Record for better Plotting of the original signals 
electric rec =  plot "Electric" rec [SigId "speedsensor1.v",                        
                                 SigId "battery1.pin_p.v",
                                 SigId "battery1.pin_p.i",
                                 SigId "electricmotor1.signalcurrent1.p.i",
                                 SigId "electricmotor1.signalcurrent1.p.v",
                                 SigId "electricmotor2.signalcurrent1.p.i",
                                 SigId "electricmotor2.signalcurrent1.p.v"
                                ]
            
-- Building Signal Record for better Plotting of the original signals 
battery rec = plot "Battery" rec [SigId "speedsensor1.v",                        
                              SigId "battery1.pin_p.v",
                              SigId "battery1.pin_p.i",
                              SigId "battery1.constantvoltage1.v",
                              SigId "battery1.constantvoltage1.i"
                             ]
          

-- Building Signal Record for better Plotting of the original signals 
generator rec = plot "Generator" rec [SigId "speedsensor1.v",                        
                                  SigId "electricmotor2.signalcurrent1.p.i",
                                  SigId "electricmotor2.signalcurrent1.v",
                                  SigId "electricmotor2.flange_a.tau",
                                  SigId "electricmotor2.speedsensor1.w",
                                  SigId "engine1.Speed",
                                  SigId "engine1.Speed"                       
                                 ]
  
 
