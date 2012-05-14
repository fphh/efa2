{-# LANGUAGE FlexibleContexts, TypeOperators #-}

import EFA2.Signal.Signal
import EFA2.Signal.Vector
import EFA2.Signal.Data

import EFA2.Signal.Base
--import EFA2.Display.DispVector

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- type Sig a b = TC a Signal H (DC D1 (UVec b)) 
-- type Power = TC P Signal H (DC D1 (UVec Val)) 
-- type DTime = TC DT Signal H (DC D1 (UVec Val)) 
-- type Energy =  TC E Signal H (DC D1 (UVec Val))

v1 =  (UV.fromList [0..3]) 
v2 =  (UV.fromList [0..3]) 

s1 = TC $ Data $ D1 v1 :: TC Signal (Data (UVec :> Nil) Val)
s2 = TC $ Data $ D1 v2 :: TC Signal (Data (UVec :> Nil) Val)


s3 = s1 .* s2



main = do 
  

  putStrLn (show s3)
