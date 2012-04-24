import EFA2.Signal.Sequence
import  EFA2.Interpreter.Env
import EFA2.Utils.Utils

import qualified Data.Map as M 


time = [0,10..50]

t8 = "zero crossing"
p8 = [2,2,2,-2,-2]
pmap8 = M.fromList [(PPosIdx 0 1,p8)]

-- Create lists over all test cases
titleList = [t8]
pmapList = [pmap8]
recList = map (PowerRecord time) pmapList  
list = idxList $ zip titleList (zip recList (map  (genSequ) (map addZeroCrossings recList)))

f (idx,(title,(pRec,(sq,sqRec)))) = do
  putStrLn ""
  putStrLn $ "Test " ++ show (idx+1) ++ ": " ++ title
  putStrLn ("XList: \n" ++ (myShowList $ idxList $ genXSig pRec))   
  putStrLn ("XList: \n" ++ (myShowList $ idxList $ genXSig (addZeroCrossings pRec)))   
  putStrLn ("Sequence: " ++  show sq) 
  putStrLn ("SequRec: " ++  show sqRec) 
  --putStrLn ("sequPMaps: " ++  show sqPmaps)
  

main = do
  mapM_ f list
  
    
