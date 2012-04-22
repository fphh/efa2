import EFA2.Signal.Sequence
import  EFA2.Interpreter.Env
import EFA2.Utils.Utils

import qualified Data.Map as M 

{-
time = [0..5]
p1 = [0,1,-2,-3,3]
p2 = [0,1,-4,-3,6]
-}
{-
-- data points creating error
time = [0.6283185300827032,0.6283185310363775]
p1 = [-6.352554283279264e-9, 3.184188667620415e-9]
p2 = [-7.058393648088072e-9, 2.865769800858373e-9]
-}

{-
-- data points creating error
time = [0.6283185300827032,0.6283185310363775]
p1 = [-6.352554283279264, 3.184188667620415]
p2 = [-7.058393648088072, 2.865769800858373]
-}

{-
-- data points creating error
time = [0,1]
p1 = [-6, 3]
p2 = [-7, 2]
-}

{-
-}
-- data points working
time = [0,1]

p1 = [-1, 1]
p2 = [-1, 3]


{-


-- data points creating trouble
time = [0,1]
p1 = [-1, 3]
p2 = [-1, 1]
-}

-- (0.0,[0.0,0.0])
-- (0.6280000000000004,[-3.185301793133549e-3,-3.539224214592833e-3])
-- (0.6283185300827032,[-6.352554283279264e-9,-7.058393648088072e-9])
-- (0.6283185310363775,[3.184188667620415e-9,2.865769800858373e-9])
-- (0.6300000000000004,[1.681390048435415e-2,1.513251043591874e-2])




pRec = PowerRecord time (M.fromList [(PPosIdx 0 1,p1),(PPosIdx 1 0, p2)])
pRec0 = addZeroCrossings pRec





main = do
  putStrLn (myShowList $ genXSig pRec) 
  putStrLn (myShowList $ genXSig pRec0)
