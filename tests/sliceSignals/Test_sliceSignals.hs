import EFA2.Signal.Sequence
import  EFA2.Term.Env
import EFA2.Utils.Utils

import qualified Data.Map as M 


time = [0,10..50]

t1 = "left event"
pa1 = [1,2,3,0,0,0]
pmap1 = M.fromList [(PowerIdx 0 1,pa1)]

t2 = "right event"
pa2 = [0,0,0,4,4,4]
pmap2 = M.fromList [(PowerIdx 0 1,pa2)]

t3 = "only one start point"
pa3 = [1,0,0,0,0]
pmap3 = M.fromList [(PowerIdx 0 1,pa3)]

t4 = "only one end point"
pa4 = [0,0,0,0,1]
pmap4 = M.fromList [(PowerIdx 0 1,pa4)]

t5 = "only one middle Point"
pa5 = [0,0,1,0,0]
pmap5 = M.fromList [(PowerIdx 0 1,pa5)]

t6 = "two signals shifted to zero"
pa6 = [1,2,0,0,0]
pb6 = [3,4,3,0,0]
pmap6 = M.fromList [(PowerIdx 0 1,pa6),(PowerIdx 1 0, pb6)]

t7 = "two signals mixed event"
pa7 = [2,2,0,0,0]
pb7 = [0,0,3,3,3]
pmap7 = M.fromList [(PowerIdx 0 1,pa7),(PowerIdx 1 0, pb7)]

t8 = "zero crossing"
p8 = [2,2,2,-2,-2]
pmap8 = M.fromList [(PowerIdx 0 1,p8)]

titleList = [t1,t2,t3,t4,t5,t6,t7,t8]
pmapList = [pmap1,pmap2,pmap3,pmap4,pmap5,pmap6,pmap7,pmap8]
resList = idxList $ zip titleList (zip pmapList (map  (genSequ time) pmapList))

f (idx,(title,(pmap,(sq,sqTime,sqPmaps)))) = do
  putStrLn ""
  putStrLn $ "Test " ++ show (idx+1) ++ ": " ++ title
  putStrLn ("XList: \n" ++ (myShowList $ idxList $ genXList time pmap))   
  putStrLn ("Sequence: " ++  show sq) 
  putStrLn ("SequTime: " ++  show sqTime) 
  putStrLn ("sequPMaps: " ++  show sqPmaps)
  

main = do
  mapM_ f resList
  
    
{-
  putStrLn ""
  putStrLn "1"
  putStrLn (show $ genXList time pmap1)   
  putStrLn (show sq1) 
  putStrLn (show sqTime1) 
  putStrLn (show sqPmaps1)
  putStrLn ""
  putStrLn "2"
  putStrLn (myShowList $ genXList time pmap2)   
  putStrLn (show sq2) 
  putStrLn (myShowList sqTime2) 
  putStrLn (myShowList sqPmaps2)
  putStrLn ""
  putStrLn "3"
  putStrLn (myShowList $ genXList time pmap3)     
  putStrLn (show sq3) 
  putStrLn (myShowList sqTime3) 
  putStrLn (myShowList sqPmaps3)
  putStrLn ""
  putStrLn "4"
  putStrLn (myShowList $ genXList time pmap4)     
  putStrLn (show sq4) 
  putStrLn (myShowList sqTime4) 
  putStrLn (myShowList sqPmaps4)
  putStrLn ""
  putStrLn "5"
  putStrLn (myShowList $ genXList time pmap5)     
  putStrLn (show sq5) 
  putStrLn (myShowList sqTime5) 
  putStrLn (myShowList sqPmaps5)
  putStrLn ""
  putStrLn "6"
  putStrLn (myShowList $ genXList time pmap6)     
  putStrLn (show sq6) 
  putStrLn (myShowList sqTime6) 
  putStrLn (myShowList sqPmaps6)
  putStrLn ""
  putStrLn "7"
  putStrLn (myShowList $ genXList time pmap7)     
  putStrLn (show sq7) 
  putStrLn (myShowList sqTime7) 
  putStrLn (myShowList sqPmaps7)
  -}