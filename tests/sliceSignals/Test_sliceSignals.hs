import EFA2.Signal.Sequence
import  EFA2.Term.Env
import EFA2.Utils.Utils

import qualified Data.Map as M 


time = [0..5]

-- becoming zero
p1 = [1,2,3,0,0,0]
pmap1 = M.fromList [(PowerIdx 0 1,p1)]

-- leaving zero
p1 = [1,2,3,0,-4,-4]
pmap2 = M.fromList [(PowerIdx 0 1,p1)]

-- only one start point
p1 = [1,0,0,0,0]
pmap3 = M.fromList [(PowerIdx 0 1,p1)]

-- only one end point
p1 = [0,0,0,0,1]
pmap4 = M.fromList [(PowerIdx 0 1,p1)]

-- only one middle Point
p1 = [0,0,1,0,0]
pmap5 = M.fromList [(PowerIdx 0 1,p1)]

-- two sgnals shifted to zero
p1 = [1,2,0,0,0]
p2 = [3,4,3,0,0]
pmap6 = M.fromList [(PowerIdx 0 1,p1),(PowerIdx 1 0, p2)]

-- two sgnals mixed event
p1 = [2,2,0,0,0]
p2 = [0,0,3,3,3]
pmap6 = M.fromList [(PowerIdx 0 1,p1),(PowerIdx 1 0, p2)]

(sq1,sqTime1,sqPmaps1) = genSequ time pmap1
(sq2,sqTime2,sqPmaps2) = genSequ time pmap2
(sq3,sqTime3,sqPmaps3) = genSequ time pmap3
(sq4,sqTime4,sqPmaps4) = genSequ time pmap4
(sq5,sqTime5,sqPmaps5) = genSequ time pmap5
(sq6,sqTime6,sqPmaps6) = genSequ time pmap6


main = do
  putStrLn (myShowList $ genXList time pmap) 
  putStrLn (show sq1) 
  putStrLn (myShowList sqTime1) 
  putStrLn (myShowList sqPmaps1)
  
  putStrLn (show sq2) 
  putStrLn (myShowList sqTime2) 
  putStrLn (myShowList sqPmaps2)
  
  putStrLn (show sq2) 
  putStrLn (myShowList sqTime2) 
  putStrLn (myShowList sqPmaps2)