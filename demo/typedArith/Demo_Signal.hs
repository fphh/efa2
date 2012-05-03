import EFA2.Signal.Signal
import EFA2.Signal.Vector

s1 = TC $ EList [0..1] :: TC P (EList Val) 
s2 = TC $ EList [0..3] :: TC DT (EList Val) 

s3 = s1 ~* s2 :: TC E (EList Val)


main = do 
  putStrLn (show s3)