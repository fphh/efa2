import EFA2.Signal.Signal
import EFA2.Signal.Vector

v1 =  EList [0..1]
v2 =  EList [0..3]

s1 = TC v1 :: TC P EList Val 
s2 = TC v2 :: TC DT EList Val 

--s3 = s1 ~* s2 :: TC E EList Val
v3 = v1 .* v2 :: EList Val

s3 = apply2EC (.*) s1 s2 ::  TC P EList Val
s4 = s1 ~* s2 ::  TC E EList Val

main = do 
  putStrLn (show v3) 
  putStrLn (show s3)
  putStrLn (show s4)