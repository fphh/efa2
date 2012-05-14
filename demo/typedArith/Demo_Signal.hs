{-# LANGUAGE FlexibleContexts, TypeOperators #-}

import EFA2.Signal.Signal
import EFA2.Signal.Vector
import EFA2.Signal.Data
import EFA2.Signal.Typ
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

p1 = TC $ Data $ D1 v1 :: TC (Typ A P) Signal (Data (UVec :> Nil) Val)
p2 = TC $ Data $ D1 v1 :: TC (Typ A P) Signal (Data (UVec :> Nil) Val)
dt = TC $ Data $ D1 v2 :: TC (Typ D T) Signal (Data (UVec :> Nil) Val)
-- dp = TC $ Data $ D1 v2 :: TC (Typ D P) Signal (Data (UVec :> Nil) Val)


e = dt .* p1
p3 = p1 .+ dp
dp = p1 .- p2
dt2 = e ./ p2



main = do 
  
  putStrLn (show e)
  putStrLn (show p3)
  putStrLn (show dp)
  putStrLn (show dt)
