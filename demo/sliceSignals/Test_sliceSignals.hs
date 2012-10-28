import EFA2.Interpreter.Env

import qualified EFA2.Signal.Signal as S
import EFA2.Signal.Sequence
import EFA2.Signal.SequenceData
import EFA2.Report.Report
import EFA2.Report.Sequence

import EFA2.Utils.Utils

import qualified Data.Map as M 


time = S.fromList [0,10..50]

t1 = "left event"
pa1 = [1,2,3,0,0,0]
pmap1 = M.fromList [(PPosIdx 0 1,pa1)]

t2 = "right event"
pa2 = [0,0,0,4,4,4]
pmap2 = M.fromList [(PPosIdx 0 1,pa2)]

t3 = "only one start point"
pa3 = [1,0,0,0,0,0]
pmap3 = M.fromList [(PPosIdx 0 1,pa3)]

t4 = "only one end point"
pa4 = [0,0,0,0,0,1]
pmap4 = M.fromList [(PPosIdx 0 1,pa4)]

t5 = "only one middle Point"
pa5 = [0,0,1,0,0,0]
pmap5 = M.fromList [(PPosIdx 0 1,pa5)]

t6 = "two signals shifted to zero"
pa6 = [1,2,0,0,0,0]
pb6 = [3,4,3,0,0,0]
pmap6 = M.fromList [(PPosIdx 0 1,pa6),(PPosIdx 1 0, pb6)]

t7 = "two signals mixed event"
pa7 = [2,2,0,0,0,0]
pb7 = [0,0,3,3,3,3]
pmap7 = M.fromList [(PPosIdx 0 1,pa7),(PPosIdx 1 0, pb7)]

t8 = "zero crossing"
p8 = [2,2,2,-2,-2,-2]
pmap8 = M.fromList [(PPosIdx 0 1,p8)]

t9 = "zero start Point"
p9 = [0,2,2,2,2,2]
pmap9 = M.fromList [(PPosIdx 0 1,p9)]

t10 = "zero end Point"
p10 = [2,2,2,2,2,0]
pmap10 = M.fromList [(PPosIdx 0 1,p10)]

t11 = "two events sametime"
p11a = [2,2,0,0,0,0]
p11b = [0,0,0,2,2,2]
pmap11 = M.fromList [(PPosIdx 0 1,p11a),(PPosIdx 1 0,p11b)]

t12 = "single Zero Point"
p12 = [2,2,0,2,2,2]
pmap12 = M.fromList [(PPosIdx 0 1,p12)]




-- Create lists over all test cases
titleList = [t1,t2,t3,t4,t5,t6,t7,t9,t10,t8,t11,t12]
pmapList = map (M.map S.fromList) [pmap1,pmap2,pmap3,pmap4,pmap5,pmap6,pmap7,pmap9,pmap10,pmap8,pmap11,pmap12]
recList = map (PowerRecord time) pmapList  
list = idxList $ zip titleList (zip recList (map  (genSequ) (map addZeroCrossings recList)))

f (idx,(title,(pRec,(sq,sqRec)))) = do
  putStrLn ""
  putStrLn $ "Test " ++ show (idx+1) ++ ": " ++ title
  report [RAll] (title,pRec)
  report [RAll] (title,addZeroCrossings pRec)
  report [] (title, sq) --putStrLn ("Sequence: " ++  show sq) 
  report [RAll] (title,sqRec)
  
--  putStrLn ("pRec: \n" ++ (show pRec))   
--  putStrLn ("pRec0: \n" ++ (show $ addZeroCrossings pRec))   
--  putStrLn ("SequRec: " ++  show sqRec) 
  --putStrLn ("sequPMaps: " ++  show sqPmaps)
  

main = do
  mapM_ f list
  
    
