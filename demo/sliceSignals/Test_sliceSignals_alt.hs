import EFA2.Interpreter.Env

import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import EFA2.Signal.Signal (PSigL)

import EFA2.Report.Sequence
import EFA2.Report.Report

import EFA2.Signal.Plot

import EFA2.Utils.Utils

import qualified Data.Map as M


time = S.fromList [0,10..50]

p1 = S.fromList [1,0,0,1,0,0] :: PSigL
p2 = S.fromList [1,0,1,1,1,0] :: PSigL

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

    
