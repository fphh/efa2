{-# LANGUAGE FlexibleContexts, TypeOperators #-}

import EFA2.Signal.Signal
import EFA2.Signal.Vector
import EFA2.Signal.Data
import EFA2.Signal.Typ
import EFA2.Signal.Base

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV


v1 =  (UV.fromList [0..3]) 
v2 =  (UV.fromList [0..3]) 

p1 = TC $ Data $ D1 v1 :: TC Signal (Typ A P) (UVec Val)
p2 = TC $ Data $ D1 v1 :: TC Signal (Typ A P) (UVec Val)
dt = TC $ Data $ D1 v2 :: TC Signal (Typ D T) (UVec Val)

e = dt .* p1
p3 = p1 .+ dp
dp = p1 .- p2
dt2 = e ./ p2



main = do 
  
  putStrLn (show e)
  putStrLn (show p3)
  putStrLn (show dp)
  putStrLn (show dt)
