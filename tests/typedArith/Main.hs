

-- import EFA2.Utils.Utils
import EFA2.Signal.Signal
import EFA2.Display.DispSignal
import EFA2.Display.Plot

import qualified Data.Map as M 


-- data points working
time = sfromList [0,1] :: TSig
t = [0..3]

p1 = sfromList [-1, 1] :: PSigL
p2 = sfromList [-2, 3] :: PSigL

p3 = sfromList [-10, 10] :: PSigL
p4 = sfromList [-20, 30] :: PSigL

pList = [p1,p2]
pList2 = [p3,p4]

mp1 = fromSigList pList :: PSig2L
mp2 = fromSigList pList2 :: PSig2L
mp3 = mp1 .++ mp2 :: PSig2L

-- pRec = PowerRecord time (M.fromList [(PPosIdx 0 1,p1),(PPosIdx 1 0, p2)])
-- pRec0 = addZeroCrossings pRec

(t2,p5) = stdeltaMap2Reverse (-) t p1

main = do
  putStrLn (show pList) 
  putStrLn (show pList2)
  putStrLn (show mp1)
  putStrLn (show mp2)
  putStrLn (show $ mp3) 
  putStrLn (show $ mp1 .++ mp2) 
  putStrLn (show $ (shead mp3)) 
  putStrLn (show $ (stail mp3)) 
  putStrLn (show $ (sinit mp3)) 
  putStrLn (show $ (slast mp3)) 
  putStrLn (show $ (stranspose mp3)) 
  sigPlot mp3
  
  
  




  
  