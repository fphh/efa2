{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Modules.Plots where

import Modules.System as System

import qualified EFA.Signal.Plot as Plot

import qualified EFA.Signal.Vector as V

import EFA.Signal.Typ (Typ,A,T,P,Tt)
import EFA.Signal.Signal (Signal)
import EFA.Signal.Record (SigId(..), Record(..), PowerRecord, SignalRecord)
import EFA.Signal.Record as Record
import EFA.Hack.Record as HRecord


import EFA.Report.Typ (TDisp)

import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import Control.Functor.HT (void)
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Graph as Graph
import EFA.Report.Typ (TDisp, DisplayType(Typ_T), getDisplayUnit, getDisplayTypName)
import qualified Graphics.Gnuplot.Advanced as Plot

import qualified Data.Map as M
import EFA.Utility(checkedLookup)

import Debug.Trace

import Data.Monoid ((<>))

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
plot title rec sigIds = Plot.recordSelect sigIds title rec
                           
                           
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
mkPlotPowers :: Show (v a) => PowerRecord Node v a -> Record Signal (Typ A T Tt) (Typ A P Tt) SigId v a 
mkPlotPowers (Record time pMap) = Record time newMap
  where -- replace old with new keys
    newMap = M.mapKeys f pMap
    f key = checkedLookup System.powerPositonNames key
    
genPowers :: Plottable v a => PowerRecord Node v a -> IO()
genPowers pRec =  plot "GenerationPowers" (mkPlotPowers pRec) [SigId "Fuel",    
                                                          --   SigId "CrankShaft",
                                                             SigId "BatteryClamps",
                                                             SigId "BatteryCore",
                                                             SigId "Wire"
                                                            ]

propPowers :: (Show (v a), Plottable v a) => PowerRecord Node v a -> IO()
propPowers pRec = plot "PropulsionPowers" (mkPlotPowers pRec) [SigId "MotorClamps",
                                                             SigId "OutShaft",
                                                             SigId "ToFrontBrakes",
                                                             SigId "FrontWheelHub",
                                                             SigId "FrontTires"
                                                              ]
                  
vehPowers :: (Show (v a), Plottable v a) => PowerRecord Node v a -> IO()
vehPowers pRec = plot "VehiclePowers"  (mkPlotPowers pRec) [SigId "ToFrontBrakes",
                                                          SigId "RearTires",    
                                                          SigId "ToInertia",
                                                          SigId "ToResistance"
                                                         ]





{-
vehPowers2 :: (Show (v a), Plottable v a) => [String] -> [PowerRecord Node v a] -> IO()
vehPowers2 recNames powerRecords = Plot.recordIOList "VehiclePowers" (zipWith g recNames $ map HRecord.namePowers powerRecords) 
  where
    f rec = Record.extract [SigId "ToFrontBrakes",                                                     
                            SigId "RearTires",                                                       
                            SigId "ToInertia",                                                       
                            SigId "ToResistance"
                           ] $ mkPlotPowers rec 
            
    g name (Record time sigs) = Record time (M.mapKeys (\ (SigId x) -> SigId (name ++ "_" ++ x) ) sigs)
-}    

plotPowers :: (Fractional a,
                      V.Walker v,
                      V.Storage v a,
                      V.FromList v,
                      Tuple.C a,
                      Atom.C a, 
                      Show (v a)) =>
              M.Map (PPosIdx System.Node) (SigId) ->  [String] -> [PowerRecord Node v a] -> [SigId]-> IO() 
plotPowers signalNameMap recNames powerRecords sigIDList = if length recNames == length powerRecords then
                                                     Plot.recordIOList "VehiclePowers" $  zipWith HRecord.recName recNames $ 
                                                     map (Record.extract sigIDList) $                                                    
                                                     (map (HRecord.namePowers signalNameMap) powerRecords)
                                                  else error("Length between List of Records and Names doesn't match")


{-


-- Can differ between Plots


preProcessing: 
- Signal / Signal to PlotRecord 
- Record -> PlotRecord
- [Record] -> PlotRecord
- Sequ -> PlotRecord





plotOpts = Terminal Pdf FilePath | Y-Label Name | Title | Grid

-- Can differ between Plots and Records
rStyleOpts = LineWidth | IncrementLineWith | IncrementLineStyle | LineSyleList | Increment

-- 
RecordOpts = Selection .. | NamePowers | NameSignals | AddLeadingSignal | Normate  (Record -> Record commutierbar)

MultiOpts = AutoWindowNames Title |  Split Int with LeadingSignal | Split 


alternative Idee: 

timePlotData = SplitRecord Int Record | 


-- Alternativ rPlot = timePlot

time :: String -> [PlotOpts] -> [StyleOpts] -> [RecordOpts] -> [Record] -> IO ()
time title Options recordList = 

void . Plot.default . Frame.cons (recordAttr) .  $ foldMap record recList


-- alternative Anwendungen !!

-- vorher in Record konvertieren
time :: String -> [PlotOpts] -> [StyleOpts] -> [RecordOpts] -> (Time,Signal) -> IO ()

time :: String -> [PlotOpts] -> [StyleOpts] -> [RecordOpts] -> Record -> IO ()
time :: String -> [PlotOpts] -> [StyleOpts] -> [RecordOpts] -> [Record] -> IO ()

time :: String -> [PlotOpts] -> [StyleOpts] -> [RecordOpts] -> (Record,SequData Record) -> IO ()

-- vorher in record-Liste konvertieren 
time :: String -> [PlotOpts] -> [StyleOpts] -> [RecordOpts] -> [SequData Record] -> IO ()


timeIO :: String -> [(plotOpts,([styleopts, recordOpts,record]))] 


!! Keine Typ-Sicherheit, wenn wir nur Gaph2D.T verwenden !! --> alles Zeit auf Achse

time :: String -> [Gaph2D.T]

rStyleOpts = LineWidth | IncrementLineWith | IncrementLineStyle | LineSyleList | Increment

-}

------------------

data GlobalOpts = GlobalOpts {
  gridAcc :: Bool, 
  gtitleAcc :: Maybe String }


grid :: Bool -> GlobalOpts -> GlobalOpts
grid b gopts = gopts { gridAcc = b }

gtitle :: String -> GlobalOpts -> GlobalOpts
gtitle ti gopts = gopts { gtitleAcc = Just ti }

globalDeflt = GlobalOpts {
  gridAcc = False,
  gtitleAcc = Nothing }

------------------

data RecordOpts = RecordOpts {
  sectionAcc :: [SigId],
  normAcc :: Bool }  
                  
                  

section :: [SigId] -> RecordOpts ->  RecordOpts 
section sigIds rec = rec {sectionAcc = sigIds}

norm :: Bool ->  RecordOpts ->  RecordOpts 
norm b rec = rec {normAcc = b}

recordDeflt :: RecordOpts
recordDeflt = RecordOpts {
  sectionAcc = [],
  normAcc = False } 

recOpts =
  section [SigId "1", SigId "2"] $
  recordDeflt


------------------

data WindowOpts = WindowOpts {
  wtitleAcc :: Maybe String }
                  
                  
windowDeflt = WindowOpts {
  wtitleAcc = Nothing
  }

wtitle :: String -> WindowOpts -> WindowOpts
wtitle ti gopts = gopts { wtitleAcc = Just ti }
        

------------------

type Window rec = (WindowOpts, [(RecordOpts, rec)])

--timeIO :: GlobalOpts -> [Window rec] -> IO ()
timeIO gopts = mapM_ (timeIOSingle gopts)



--timeIOSingle ::  GlobalOpts -> Window rec -> IO ()
timeIOSingle gopts (wopts, recList) =
  mapM_ (plotAttr . Frame.cons frameAttr . Plot.record . snd . trace "bla") recList
  where
    winAttr = buildWinAttr wopts
    frameAttr = buildFrameAttr gopts wopts
    plotAttr = buildPlotAttr gopts
    
    
buildFrameAttr :: (Graph.C graph) => GlobalOpts -> WindowOpts -> Opts.T graph     
buildFrameAttr gopts wopts = 
  Opts.title ti $
  Opts.grid (gridAcc gopts) $     
  Opts.xLabel ("Time [" ++ (show $ getDisplayUnit Typ_T) ++ "]") $
  Opts.yLabel ("")
  Opts.deflt
  where ti = case gtitleAcc gopts <> wtitleAcc wopts of
          Just str -> str
          _ -> ""
  
buildPlotAttr gopts = Plot.plotDefault

buildWinAttr wopts = undefined


  {-
    -- | Plot Attributes
recordAttr ::
   (Graph.C graph) =>
   String -> Opts.T graph
recordAttr name =
   Opts.title (name) $
   Opts.grid True $
   Opts.xLabel ("Time [" ++ (show $ getDisplayUnit Typ_T) ++ "]") $
   Opts.yLabel ("")
--    Opts.yLabel (show  "[" ++ (show $ getDisplayUnit Typ_P) ++ "]") $
--   Opts.size (Scale 0.7) $
   Opts.deflt
-}