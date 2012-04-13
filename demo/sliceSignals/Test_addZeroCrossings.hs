import EFA2.Signal.Sequence
import  EFA2.Term.Env
import EFA2.Utils.Utils

import qualified Data.Map as M 


time = [0..5]
p1 = [1,-2,-3,3]
p2 = [1,-4,-3,6]

pmap = M.fromList [(PowerIdx 0 1,p1),(PowerIdx 1 0, p2)]
sequ = genSequ time pmap
(ztime,zpmap) = addZeroCrossingPoints time pmap

main = do
  putStrLn (myShowList $ genXList time pmap) 
  putStrLn (myShowList $ genXList ztime zpmap)
