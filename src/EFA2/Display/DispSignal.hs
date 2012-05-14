{-# LANGUAGE FlexibleInstances, GADTs #-}

module EFA2.Display.DispSignal (module EFA2.Display.DispSignal) where

import EFA2.Display.DispTyp
import EFA2.Display.DispBase

import EFA2.Signal.Data
import EFA2.Signal.Typ
import EFA2.Signal.Signal


-- | display a single value  
dispSingle ::  Disp a => a -> DisplayType -> String
dispSingle x t = disp x f s  ++ " " ++ show u
           where u = getDisplayUnit t
                 s = getUnitScale u
                 f = getDisplayFormat dispLength t u 


-- | display a single value  
dispRange :: Disp a => a -> a -> DisplayType -> String
dispRange x y t = disp x f s  ++ " - " ++ disp y f s  ++ " " ++ show u
           where u = getDisplayUnit t
                 s = getUnitScale u
                 f = getDisplayFormat dispLength t u
 
class SDisplay a where
  sdisp :: a -> String

instance (DeltaDisp t, DisplayTyp t, PartDisp t) => SDisplay (TC Signal t d)  where 
  sdisp x@(TC dat)  = "Signal - " ++ tdisp x ++ ": " ++ dispRange dmin dmax dtyp ++ " " ++ udisp x  
    where dtyp = getDisplayType x
          (D0 (dmin, dmax)) = dgetRange dat
          


  
 