
import qualified Data.Map as M

import EFA.Report.Report
import EFA.Signal.Signal
import EFA.Signal.Base

import EFA.Report.Signal
import EFA.Report.Sequence

import EFA.Signal.Sequence
import EFA.Signal.SequenceData

l = [1..5] :: [Double]
ll = [l,l]

-- t = toTable ll

p1 = sfromList l :: PSigL
p2 = p1 :: PSigL

t = sfromList [0..4] :: TSigL

r = PowerRecord t (M.fromList [(PPosIdx 0 0,p1),(PPosIdx 0 1,p2)]) 

main = do
  
  putStrLn $ show t
  
  report [RVertical] ("TestMatrix",ll) 
  report [RVertical] ("Power1",p1) 
  report [RVertical] ("PowerRecord",r) 

  