-- module Utils.Vector (module Utils.Vector) where

import Data.Monoid
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as GV

import Data.Ratio

-- import Prelude hiding ((++))

-- instance Monoid (UV.Vector a) where
--    mempty = UV.empty
--    mappend x y = (UV.++) x y

--class QuickAppend a where 
--      (.++) = mappend
      



(.++) :: Monoid m => m -> m -> m
(.++) = mappend

v3 :: UV.Vector Int
v3 = mempty

v1 :: UV.Vector Int
v1 = UV.fromList [0..1]




main = do 
  putStrLn $ show ([1] .++ [2, 3] .++ [0,0,0])  -- show $ v1 UV.++ v2
  putStrLn $ show (v1 .++ v3)
  
  