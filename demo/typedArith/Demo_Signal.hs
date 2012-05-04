import EFA2.Signal.Signal
import EFA2.Signal.Vector

v1 =  EList [0..3]
v2 =  EList [0..3]

s1 = TC v1 :: TC P EList Val 
s2 = TC v2 :: TC DT EList Val 

--s3 = s1 ~* s2 :: TC E EList Val
v3 = v1 .* v2 :: EList Val

s3 = (apply2EC (.*) s1 s2) ::  TC E EList Val
s4 = (s1 ~* s2)  ::  TC E EList Val

(~*) :: (TMult t1 t2 t3, DMult d1 d2 d3,Show (c1 d1), Show (c2 d2)) => TC t1 c1 d1 -> TC t2 c2 d2 -> TC t3 c3 d3
(~*) x y = undefined -- apply2EC (.*) x y

main = do 
  putStrLn (show v3) 
  putStrLn (show s3)
  putStrLn (show s4)