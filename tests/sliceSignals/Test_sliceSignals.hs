import EFA2.Signal.Sequence
import  EFA2.Term.Env
import EFA2.Utils.Utils

import qualified Data.Map as M 


time = [0..5]
p1 = [1,2,3,0,0] -- ,5,5,-5,-5,0,2,2]
p2 = [1,2,3,0,0] -- ,5,5,-5,-5,0,2,2]

--p2 = [0,0,1,1,1,1,2,2,2,2,2]

-- steps1 = makeSteps time p1
-- steps2 = makeSteps time p2

pmap = M.fromList [(PowerIdx 0 1,p1),(PowerIdx 1 0, p2)]
sequ = genSequ time pmap
(ztime,zpmap) = addZeroCrossingPoints time pmap
(seque,sequTime,sequPmaps) = genSequ ztime zpmap

main = do
  putStrLn (myShowList $ genXList time pmap) 
  putStrLn (myShowList $ genXList ztime zpmap)
  putStrLn (show seque) 
  putStrLn (myShowList sequTime) 
  putStrLn (myShowList sequPmaps)