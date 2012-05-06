{-# LANGUAGE FlexibleContexts #-}

import EFA2.Signal.Signal
--import EFA2.Signal.Vector
import EFA2.Signal.Vector2

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

v1 =  DC (UV.fromList [0..3]) :: (DC D1 (UVec Val))
v2 =  DC (UV.fromList [0..3]) :: (DC D1 (UVec Val))

s1 = TC 1 :: TC P Signal Val -- v1 :: TC E Signal (DC D1 (UVec Val)) 
s2 = TC 2 :: TC DT Signal Val -- v2 :: TC DT Signal (DC D1 (UVec Val)) 
s3 = TC 3 :: TC DT SampleVec Val -- v2 :: TC DT SampleVec (DC D1 (UVec Val)) 

v3 = v1 .* v2

-- s3 = (apply2EC (.*) s1 s2)
s4 = (s1 ~* s2) 

s5 = (s1 ~* s3)   
-- s6 = (s1 ~* s3)   

main = do 
  
--  putStrLn (show s1) 
--  putStrLn (show s2) 
  
--  putStrLn (show v3) 
  putStrLn (show s4)
  putStrLn (show s5)
--  putStrLn (show s6) 