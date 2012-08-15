import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import  EFA2.Interpreter.Env
import EFA2.Utils.Utils
import EFA2.Signal.Signal
import EFA2.Display.Report

import qualified Data.Map as M 
import EFA2.Display.ReportSequence
import EFA2.Display.DispSequence

import EFA2.Display.Plot

time = sfromList [0,10..50]

p1 = sfromList [1,0,0,1,0,0] :: PSigL
p2 = sfromList [1,0,1,1,1,0] :: PSigL

pmap = M.fromList [(PPosIdx 0 1,p1),(PPosIdx 1 0,p2)]

rec = PowerRecord time pmap
rec0 = addZeroCrossings rec
(sequ,sqRec) = genSequ rec0

main = do

  report [] ("rec",rec)
  report [RAll] ("rec0",rec0)

--  report sequ 
  report [] ("sequRec",sqRec) 
  

  -- rPlot ("rec", rec)
  -- rPlot ("rec0", rec0)
  -- rPlot ("SqRec1",sqRec) 

    
