
import qualified Data.Map as M

import EFA2.Display.Report
import EFA2.Signal.Signal
import EFA2.Signal.Base

import EFA2.Display.DispSignal
import EFA2.Display.DispSequence

import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData

l = [1..5] :: [Double]
ll = [l,l]

-- t = toTable ll

p1 = sfromList l :: PSigL
p2 = p1 :: PSigL

t = sfromList [0..4] :: TSigL

r = PowerRecord t (M.fromList [(PPosIdx 0 0,p1),(PPosIdx 0 1,p2)]) 

main = do
  
  putStrLn $ show t
  
  report [] ("TestMatrix",ll) 
  report [] ("Power1",p1) 
  report [RVertical,RAll] ("PowerRecord",r) 

  