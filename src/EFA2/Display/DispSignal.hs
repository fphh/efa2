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
  -- typeDisp :: a -> String

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d) => SDisplay (TC Signal t (Data (v1 :> Nil) d)) where 
  sdisp x@(TC (Data (D1 v)))  = "Signal-D1 " ++ tdisp x ++ ": " ++ dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum v
          dmax = vmaximum v
  -- typeDist x = sdisp x ++ " :: TC Signal t (Data (v1 :> Nil) d) "
          
--        r = dgetRange dat 
--        (Data (D0 (dmin, dmax))) = r
-- DGetRange (Data (y :> Nil)) (Data Nil) d,          

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d,VSingleton v2 (v1 d)) => SDisplay (TC Signal t (Data (v2 :> v1 :> Nil) d)) where 
  sdisp x@(TC(Data (D2 v)))  = "Signal-D2 " ++ tdisp x ++ ": " ++ dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum $ vminimum v
          dmax = vmaximum $ vminimum v
 