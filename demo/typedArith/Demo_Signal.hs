{-# LANGUAGE FlexibleContexts #-}

import EFA2.Signal.Signal
--import EFA2.Signal.Vector
import EFA2.Signal.Vector2

import EFA2.Signal.Base

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

type Sig a b = TC a Signal H (DC D1 (UVec b)) 
type Power = TC P Signal H (DC D1 (UVec Val)) 
type DTime = TC DT Signal H (DC D1 (UVec Val)) 
type Energy =  TC E Signal H (DC D1 (UVec Val))

v1 =  DC (UV.fromList [0..3]) :: (DC D1 (UVec Val))
v2 =  DC (UV.fromList [0..3]) :: (DC D1 (UVec Val))

s1 = TC v1 :: Power
s2 = TC v2 :: DTime
s3 = TC v3 :: DTime

v3 = v1 .* v2

s4 = (s1 ~* s2) 
s5 = (s1 ~* s3)   

main = do 
  
--  putStrLn (show s1) 
--  putStrLn (show s2) 
  
--  putStrLn (show v3) 
  putStrLn (show s4)
  putStrLn (show s5)
--  putStrLn (show s6) 