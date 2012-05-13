{-# LANGUAGE FlexibleContexts #-}

import EFA2.Signal.Signal
import EFA2.Signal.Vector

import EFA2.Signal.Base
--import EFA2.Display.DispVector

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- type Sig a b = TC a Signal H (DC D1 (UVec b)) 
-- type Power = TC P Signal H (DC D1 (UVec Val)) 
-- type DTime = TC DT Signal H (DC D1 (UVec Val)) 
-- type Energy =  TC E Signal H (DC D1 (UVec Val))

v1 =  (UV.fromList [0..3]) :: (UVec Val)
v2 =  (UV.fromList [0..3]) :: (UVec Val)

s1 = Signal v1 :: Signal (UVec Val)
s2 = Signal v2 :: Signal (UVec Val)


s3 = s1 .* s2



main = do 
  

  putStrLn (show s3)
