{-# LANGUAGE FlexibleContexts, TypeOperators, ExistentialQuantification, RankNTypes #-}

module Demo_Signal where

import EFA2.Signal.Signal
import EFA2.Signal.Vector
import EFA2.Signal.Data
import EFA2.Signal.Typ
import EFA2.Signal.Base
import EFA2.Report.Signal


import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV


v1 =  (UV.fromList [0..3]) 
v2 =  (UV.fromList [0..3]) 
v3 =  (V.fromList [v1,v2]) 

ps1 = TC $ Data $ D0 999 :: TC Scalar (Typ A P Tt) (DVal Val)
dps2 = TC $ Data $ D0 (-1) :: TC Scalar (Typ D P Tt) (DVal Val)

p1 = TC $ Data $ D1 v1 :: TC Signal (Typ A P Tt) (UVec Val)
p2 = TC $ Data $ D1 v1 :: TC Signal (Typ D P Tt) (UVec Val)
dt = TC $ Data $ D1 v2 :: TC Signal (Typ D T Tt) (UVec Val)

pm1 = TC $ Data $ D2 v3 :: TC Signal (Typ D P Tt) (UVec2 Val)  


-- TC (Data D1 (fromList [0.0,1.0,4.0,9.0]))

ps3 = ps1.+dps2
p4 = dps2.+p1
p4' = p1.+dps2 
e = dt .* p1
--p3 = p1 .+ dp
dp = p1 .- p2
--dt2 = e ./ p2
pm2 = p1.+pm1
e2 = ps1.*dt

data HL = forall a. (SDisplay a) => HL a


instance Show HL where
         show (HL x) = "HL (" ++ sdisp x ++ ")"

main = do 

  let x = show [HL e, HL dp, HL dt]
  
  putStrLn (show e)
  --putStrLn (show p3)
  putStrLn (show dp)
  putStrLn (show dt)
  putStrLn (sdisp dt)
  putStrLn (sdisp e)
  putStrLn (sdisp p1)
  putStrLn (sdisp p2)
  -- putStrLn (sdisp pm2)
--  putStrLn (sdisp pm3)

  putStrLn x