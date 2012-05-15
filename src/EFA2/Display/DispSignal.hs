{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,FlexibleContexts,UndecidableInstances, TypeOperators#-}

module EFA2.Display.DispSignal (module EFA2.Display.DispSignal) where

import EFA2.Display.DispTyp
import EFA2.Display.DispBase

import EFA2.Signal.Data
import EFA2.Signal.Typ
import EFA2.Signal.Signal
import EFA2.Signal.Vector


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

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton y d) => SDisplay (TC Signal t (Data (y :> Nil) d)) where 
  sdisp x@(TC dat)  = "Signal - " ++ tdisp x ++ ": " ++ dispRange dmin dmax dtyp ++ " " ++ udisp x  
    where dtyp = getDisplayType x
--        r = dgetRange dat 
--        (Data (D0 (dmin, dmax))) = r
-- DGetRange (Data (y :> Nil)) (Data Nil) d,          
          (Data (D0 dmin)) = dminimum dat
          (Data (D0 dmax)) = dmaximum dat       


  
 