{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs, OverlappingInstances, FlexibleContexts, ScopedTypeVariables #-}


import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import  EFA2.Interpreter.Env
import EFA2.Utils.Utils
import EFA2.Signal.Signal

import qualified Data.Map as M 

import EFA2.Signal.Base
import EFA2.Signal.Typ
import EFA2.Signal.Data
import EFA2.Display.Plot

import Data.Monoid
import EFA2.Display.DispSignal

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
-- time = sfromList [0,1]
-- p1 = sfromList [-1, 1]
-- p2 = sfromList [-1, 3]



{-


-- data points creating trouble
time = [0,1]
p1 = [-1, 3]
p2 = [-1, 1]
-}

-- (0.0,[0.0,0.0])
-- (0.6280000000000004,[-3.185301793133549e-3,-3.539224214592833e-3])
-- (0.6283185300827calcZeroTimes032,[-6.352554283279264e-9,-7.058393648088072e-9])
-- (0.6283185310363775,[3.184188667620415e-9,2.865769800858373e-9])
-- (0.6300000000000004,[1.681390048435415e-2,1.513251043591874e-2])

t = sfromList [0,1] :: TSigL
p1 = sfromList [-1,1] :: PSigL
p2 = sfromList [-1,3] :: PSigL

pRec = PowerRecord t (M.fromList [(PPosIdx 0 1,p1),(PPosIdx 1 0, p2)])
pRec0 = addZeroCrossings pRec

main = do
  putStrLn (show pRec)
  putStrLn (show pRec0)
  rPlot pRec0
