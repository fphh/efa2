import EFA2.Signal.Sequence
import  EFA2.Term.Env
import EFA2.Utils.Utils

import qualified Data.Map as M 


time = [0..3]
p1 = [0,-5,5,0] -- ,5,5,-5,-5,0,2,2]
p2 = [0,-5,2,0] -- ,5,5,-5,-5,0,2,2]

--p2 = [0,0,1,1,1,1,2,2,2,2,2]

steps1 = makeSteps time p1
steps2 = makeSteps time p2

pmap = M.fromList [(PowerIdx 0 1,p1),(PowerIdx 1 0, p2)]
sequ = genSequ time pmap

main = do
  putStrLn (show $ zip time p1) 
--  putStrLn (show steps1)
--  putStrLn (show steps2)
  putStrLn "" 

  putStrLn (myShowList sequ)
--  putStrLn (myShowList $ genSequTime time sequ)
--  putStrLn (myShowList $ genSequPowerMaps pmap sequ)
  
  putStrLn (show $ addZeroCrossingPoints time pmap)
  