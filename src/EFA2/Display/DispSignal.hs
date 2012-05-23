{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,FlexibleContexts,UndecidableInstances, TypeOperators#-}

module EFA2.Display.DispSignal (module EFA2.Display.DispSignal) where

import EFA2.Display.DispTyp
import EFA2.Display.DispBase

import EFA2.Signal.Data
import EFA2.Signal.Typ
import EFA2.Signal.Signal
import EFA2.Signal.Vector

import qualified Data.List as L 


-- | display a single value  
dispSingle ::  Disp a => a -> DisplayType -> String
dispSingle x t = disp f s x ++ " " ++ show u
           where u = getDisplayUnit t
                 s = getUnitScale u
                 f = getDisplayFormat dispLength t u 


-- | display a single value  
dispRange :: Disp a => a -> a -> DisplayType -> String
dispRange x y t = disp f s x ++ " - " ++ disp f s y ++ " " ++ show u
           where u = getDisplayUnit t
                 s = getUnitScale u
                 f = getDisplayFormat dispLength t u

dispAll :: Disp a => [a] -> DisplayType -> String
dispAll xs t = (L.intercalate " " $ map (disp f s) xs) ++  " " ++ show u
           where u = getDisplayUnit t
                 s = getUnitScale u
                 f = getDisplayFormat dispLength t u

 
class SDisplay a where
  sdisp :: a -> String

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d, FromToList (Data (v1 :> Nil)) d)
         => SDisplay (TC Signal t (Data (v1 :> Nil) d)) where 
  sdisp x@(TC (Data (D1 v)))  = "Sig-D1 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum v
          dmax = vmaximum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d, FromToList (Data (v1 :> Nil)) d)
         => SDisplay (TC FSignal t (Data (v1 :> Nil) d)) where 
  sdisp x@(TC (Data (D1 v)))  = "FSig-D1 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum v
          dmax = vmaximum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d, FromToList (Data (v1 :> Nil)) d)
         => SDisplay (TC TestRow t (Data (v1 :> Nil) d)) where 
  sdisp x@(TC (Data (D1 v)))  = "TestRow-D1 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum v
          dmax = vmaximum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d,VSingleton v2 (v1 d), FromToList (Data (v2 :> (v1 :> Nil))) d) => SDisplay (TC Signal t (Data (v2 :> v1 :> Nil) d)) where 
  sdisp x@(TC(Data (D2 v)))  = "Sig-D2 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum $ vminimum v
          dmax = vmaximum $ vminimum v
 
instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d,VSingleton v2 (v1 d), FromToList (Data (v2 :> (v1 :> Nil))) d) => SDisplay (TC FSignal t (Data (v2 :> v1 :> Nil) d)) where 
  sdisp x@(TC(Data (D2 v)))  = "Sig-D2 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum $ vminimum v
          dmax = vmaximum $ vminimum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d,VSingleton v2 (v1 d), FromToList (Data (v2 :> (v1 :> Nil))) d) => SDisplay (TC TestRow t (Data (v2 :> v1 :> Nil) d)) where 
  sdisp x@(TC(Data (D2 v)))  = "Test-D2 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum $ vminimum v
          dmax = vmaximum $ vminimum v
