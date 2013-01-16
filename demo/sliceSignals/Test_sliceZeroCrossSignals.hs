import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData

import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Signal (TSigL, PSigL)

import  EFA2.Equation.Env
import EFA2.Utility

import qualified Data.Map as M 


time = S.fromList [0,10..50] :: TSigL

t8 = "zero crossing"
p8 = S.fromList [2,2,2,-2,-2] :: PSigL
pmap8 = M.fromList [(PPosIdx 0 1,p8)]

-- Create lists over all test cases
titleList = [t8]
pmapList = [pmap8]
recList = map (PowerRecord time) pmapList  
list = idxList $ zip titleList (zip recList (map  (genSequ) (map addZeroCrossings recList)))

f (idx,(title,(pRec,(sq,sqRec)))) = do
  putStrLn ""
  putStrLn $ "Test " ++ show (idx+1) ++ ": " ++ title
  putStrLn ("XList: \n" ++ (show pRec))   
  putStrLn ("XList: \n" ++ (show (addZeroCrossings pRec)))   
  putStrLn ("Sequence: " ++  show sq) 
  putStrLn ("SequRec: " ++  show sqRec) 
  --putStrLn ("sequPMaps: " ++  show sqPmaps)
  

main = do
  mapM_ f list
  
    
