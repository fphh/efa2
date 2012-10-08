{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module EFA2.Display.DispSignal (module EFA2.Display.DispSignal) where

import EFA2.Display.DispTyp
          (TDisp, DisplayType, getDisplayFormat, getDisplayUnit)
import EFA2.Display.DispBase
          (Disp, dispLength, getUnitScale, disp)

import qualified EFA2.Signal.Signal as S
-- import qualified EFA2.Signal.Data as D
import qualified EFA2.Signal.Vector as SV
import EFA2.Signal.Signal (TC(TC), Scalar, Signal, FSignal, TestRow)
import EFA2.Signal.Data (Data(Data), (:>), Nil)

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

{-
instance (TDisp t, Disp d, SV.Singleton v1 d, D.FromList (Data Nil) d)
         => SDisplay (TC Scalar t (Data Nil d)) where
  sdisp x@(TC (Data v))  = "Sig-D0 " -- ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
--    where dtyp = S.getDisplayType x
--          dmin = SV.minimum v
--          dmax = SV.maximum v

-}
instance (TDisp t, Disp d) => SDisplay (TC Scalar t (Data Nil d)) where
  sdisp x@(TC (Data v))  = "Sig-D0 " ++ dispSingle v dtyp --  ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x


instance
   (TDisp t,
    SV.Singleton v1, SV.FromList v1, SV.Storage v1 d, Disp d, Ord d) =>
         SDisplay (TC Signal t (Data (v1 :> Nil) d)) where
  sdisp x@(TC (Data v))  = "Sig-D1 " ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x
          _dmin = SV.minimum v
          _dmax = SV.maximum v

instance
   (TDisp t,
    SV.Singleton v1, SV.FromList v1, SV.Storage v1 d, Disp d, Ord d) =>
         SDisplay (TC FSignal t (Data (v1 :> Nil) d)) where
  sdisp x@(TC (Data v))  = "FSig-D1 " ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x
          _dmin = SV.minimum v
          _dmax = SV.maximum v

instance
   (TDisp t,
    SV.Singleton v1, SV.FromList v1, SV.Storage v1 d, Disp d, Ord d) =>
         SDisplay (TC TestRow t (Data (v1 :> Nil) d)) where
  sdisp x@(TC (Data v))  = "TestRow-D1 " ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x
          _dmin = SV.minimum v
          _dmax = SV.maximum v

{-
instance (TDisp t, Disp d, SV.Singleton v1 d, SV.Singleton v2 (v1 d), D.FromList (v2 :> (v1 :> Nil)) d) => SDisplay (TC Signal t (Data (v2 :> v1 :> Nil) d)) where
  sdisp x@(TC(Data v))  = "Sig-D2 " ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x
          dmin = SV.minimum $ SV.minimum v
          dmax = SV.maximum $ SV.maximum v

instance (TDisp t, Disp d, SV.Singleton v1 d, SV.Singleton v2 (v1 d), D.FromList (v2 :> (v1 :> Nil)) d) => SDisplay (TC FSignal t (Data (v2 :> v1 :> Nil) d)) where
  sdisp x@(TC(Data v))  = "Sig-D2 " ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x
          dmin = SV.minimum $ SV.minimum v
          dmax = SV.maximum $ SV.maximum v

instance (TDisp t, Disp d, SV.Singleton v1 d, SV.Singleton v2 (v1 d), D.FromList (v2 :> (v1 :> Nil)) d) => SDisplay (TC TestRow t (Data (v2 :> v1 :> Nil) d)) where
  sdisp x@(TC(Data v))  = "Test-D2 " ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x
          dmin = SV.minimum $ SV.minimum v
          dmax = SV.maximum $ SV.maximum v
-}
