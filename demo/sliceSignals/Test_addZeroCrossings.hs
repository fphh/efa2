import EFA2.Signal.Sequence
import  EFA2.Interpreter.Env
import EFA2.Utils.Utils

import qualified Data.Map as M 


time = [0..5]
p1 = [0,1,-2,-3,3]
p2 = [0,1,-4,-3,6]

pRec = PowerRecord time (M.fromList [(PPosIdx 0 1,p1),(PPosIdx 1 0, p2)])
pRec0 = addZeroCrossings pRec

main = do
  putStrLn (myShowList $ genXSig pRec) 
  putStrLn (myShowList $ genXSig pRec0)
