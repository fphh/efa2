{-# LANGUAGE FlexibleContexts #-}

import EFA2.Signal.Signal
import EFA2.Signal.Vector

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

v1 =  DC (UVec $ UV.fromList [0..3]) :: (DC D1 (UVec Val))
v2 =  DC (UVec $ UV.fromList [0..3]) :: (DC D1 (UVec Val))

s1 = TC v1 :: TC H P Signal (DC D1 (UVec Val)) 
s2 = TC v2 :: TC H DT Signal (DC D1 (UVec Val)) 

v3 = v1 .* v2 :: DC D1 (UVec Val)

s3 = (apply2EC (.*) s1 s2)
s4 = (s1 ~* s2)  


main = do 
  putStrLn (show v3) 
  putStrLn (show s3)
  putStrLn (show s4)
 