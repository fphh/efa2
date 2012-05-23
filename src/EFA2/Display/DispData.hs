{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses,FunctionalDependencies, TypeSynonymInstances, FlexibleContexts, UndecidableInstances #-} 

module EFA2.Display.DispVector (module EFA2.Display.DispVector) where

import EFA2.Display.DispBase
--import EFA2.Signal.Vector2
import EFA2.Signal.Base
import qualified Data.Vector.Unboxed as UV 

-- | display a single value  
dispSingle :: Disp a => a -> String
dispSingle x = sdisp x 

-- | display a single value  
dispRange :: Disp a => a -> a -> String
dispRange x y = sdisp x  ++ " - " ++ sdisp y 
 
-- | Data Container Display Class with Instances
class DataDisplay dim s where
  ddisp :: DC dim s -> String

instance Disp d => DataDisplay D0 (DVal d) where 
  ddisp (DC (DVal x))  = "DVal :" ++  sdisp x

instance (DRange D1 UVec d, Disp d,NeutralElement d, UV.Unbox d, Ord d, SFold UVec d (d, d)) => DataDisplay D1 (UVec d) where 
  ddisp x = "UVec: " ++ sdisp min ++ "-" ++ sdisp max 
    where (DC (DVal (min,max))) = getRange x

instance (DRange D1 Vec d, Disp d,NeutralElement d, Ord d, SFold Vec d (d, d) ) => DataDisplay D1 (Vec d) where 
  ddisp x = "Vec: " ++ sdisp min ++ "-" ++ sdisp max 
    where (DC (DVal (min,max))) = getRange x

instance (DRange D1 List d, Disp d,NeutralElement d, Ord d, SFold List d (d, d)) => DataDisplay D1 (List d) where 
  ddisp x = "List: " ++ sdisp min ++ "-" ++ sdisp max 
    where (DC (DVal (min,max))) = getRange x


{-
instance (DRange dim UVec Val) => DataDisplay dim (UVec Val) where 
  ddisp x = "UVec: " ++ sdisp min ++ "-" ++ sdisp max 
    where (DC (DVal (min,max))) = getRange x

instance (DRange dim Vec Val ) => DataDisplay dim (Vec Val) where 
  ddisp x  = "Vec: " ++ sdisp min ++ "-" ++ sdisp max 
    where (DC (DVal (min,max))) = getRange x

instance (DRange dim Vec Sign) => DataDisplay dim (Vec Sign) where 
  ddisp x  = "Vec: " ++ sdisp min ++ "-" ++ sdisp max 
    where (DC (DVal (min,max))) = getRange x

instance (DRange dim List Val)  => DataDisplay dim (List Val) where 
  ddisp x  = "List: " ++ sdisp min ++ "-" ++ sdisp max 
    where (DC (DVal (min,max))) = getRange x
-}
