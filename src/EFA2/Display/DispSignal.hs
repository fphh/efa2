{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses,FlexibleContexts,UndecidableInstances, TypeOperators, TypeSynonymInstances#-}

module EFA2.Display.DispSignal (module EFA2.Display.DispSignal) where

import EFA2.Display.DispTyp
import EFA2.Display.DispBase

import EFA2.Display.Report
import EFA2.Signal.Base
import EFA2.Signal.Data
import EFA2.Signal.Typ
import EFA2.Signal.Signal
import EFA2.Signal.Vector
import Debug.Trace
import Text.Printf
import qualified Data.List as L 
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- | Display single values
vdisp :: (DisplayTyp t) => TC s t (Data Nil Val)  -> String
vdisp x = printf f $ s*val              
  where t = getDisplayType x
        u = getDisplayUnit t
        (UnitScale s) = getUnitScale u
        (DisplayFormat f) = getDisplayFormat dispLength t u 
        (TC (Data (D0 val))) = x

-- | Display single values
sdisp :: (DisplayTyp t, VFromList v Double) => TC s t (Data (v :> Nil) Val)  -> [String]
sdisp xs = map g l -- (f l)
  where g x = printf f (s*x)              
        t = getDisplayType xs
        u = getDisplayUnit t
        (UnitScale s) = getUnitScale u
        (DisplayFormat f) = getDisplayFormat dispLength t u 
        l = stoList xs

        
-- | Display Signal Type        
class SigDisp s c where 
      sigDisp :: TC s t (c d)  -> String
      
instance SigDisp Signal (Data ([] :> Nil)) where      
         sigDisp x = "Sig1L" 

instance SigDisp Signal (Data (UV.Vector :> Nil)) where      
         sigDisp x = "Sig1U" 

instance SigDisp Signal (Data (V.Vector :> Nil)) where      
         sigDisp x = "Sig1V" 

instance SigDisp Signal (Data ([] :> [] :> Nil)) where      
         sigDisp x = "Sig2L" 

instance SigDisp Signal (Data (V.Vector :> UV.Vector :> Nil)) where      
         sigDisp x = "Sig2U" 

instance SigDisp Signal (Data (V.Vector :> V.Vector :> Nil)) where      
         sigDisp x = "Sig2V" 


instance (VFromList v Val,VSingleton v Double,UDisp t, SigDisp s (Data (v :> Nil))) => ToTable (TC s t (Data (v :> Nil) Val)) where
      toTable os (ti,x) = Table {tableTitle = "",
                         tableFormat = tf,
                         tableData = td,
                         tableSubTitle = ""}
        where td = TableData {tableBody =  [map (toDoc id ) (f x) ],
                              titleRow = [],
                              titleCols = [map (toDoc id) [ti,sigDisp x,tdisp x]],
                              endCols = [[toDoc id $ udisp x]]}

              max = smaximum x
              min = sminimum x
              tf = autoFormat td
{-              
              (Just (RIndices iv)) = L.find match os
              match (RIndices _) = True 
              match _ = False
              f x | L.elem RAll os && L.elem RIndices os = sdisp $ subSignal1D x iv               
-}              

              f x | L.elem RAll os = sdisp x              
              f x | otherwise = [(vdisp min) ++ " - " ++ (vdisp max)]

{-
<<<<<<< HEAD
=======
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

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d, VSingleton v1 d, DFromList (Data Nil) d)
         => SDisplay (TC Scalar t (Data Nil d)) where 
  sdisp x@(TC (Data (D0 v)))  = "Sig-D0 " -- ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
--    where dtyp = getDisplayType x
--          dmin = vminimum v
--          dmax = vmaximum v


instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d, DFromList (Data (v1 :> Nil)) d)
         => SDisplay (TC Signal t (Data (v1 :> Nil) d)) where 
  sdisp x@(TC (Data (D1 v)))  = "Sig-D1 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum v
          dmax = vmaximum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d, DFromList (Data (v1 :> Nil)) d)
         => SDisplay (TC FSignal t (Data (v1 :> Nil) d)) where 
  sdisp x@(TC (Data (D1 v)))  = "FSig-D1 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum v
          dmax = vmaximum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d, DFromList (Data (v1 :> Nil)) d)
         => SDisplay (TC TestRow t (Data (v1 :> Nil) d)) where 
  sdisp x@(TC (Data (D1 v)))  = "TestRow-D1 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum v
          dmax = vmaximum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d,VSingleton v2 (v1 d), DFromList (Data (v2 :> (v1 :> Nil))) d) => SDisplay (TC Signal t (Data (v2 :> v1 :> Nil) d)) where 
  sdisp x@(TC(Data (D2 v)))  = "Sig-D2 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum $ vminimum v
          dmax = vmaximum $ vminimum v
 
instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d,VSingleton v2 (v1 d), DFromList (Data (v2 :> (v1 :> Nil))) d) => SDisplay (TC FSignal t (Data (v2 :> v1 :> Nil) d)) where 
  sdisp x@(TC(Data (D2 v)))  = "Sig-D2 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum $ vminimum v
          dmax = vmaximum $ vminimum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d,VSingleton v1 d,VSingleton v2 (v1 d), DFromList (Data (v2 :> (v1 :> Nil))) d) => SDisplay (TC TestRow t (Data (v2 :> v1 :> Nil) d)) where 
  sdisp x@(TC(Data (D2 v)))  = "Test-D2 " ++ tdisp x ++ ": " ++ dispAll (stoList x) dtyp -- dispRange dmin dmax dtyp 
    where dtyp = getDisplayType x
          dmin = vminimum $ vminimum v
          dmax = vmaximum $ vminimum v
>>>>>>> master
-}