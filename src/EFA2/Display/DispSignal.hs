{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module EFA2.Display.DispSignal (module EFA2.Display.DispSignal) where

import EFA2.Display.DispTyp
          (TDisp, DisplayType, getDisplayFormat, getDisplayUnit)
import EFA2.Display.DispBase
          (Disp, dispLength, getUnitScale, disp)

import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Data as D
import qualified EFA2.Signal.Vector as SV
import EFA2.Signal.Signal (TC(TC))
import EFA2.Signal.Data (Data(Data), (:>), Nil)


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
dispAll xs t = (unwords $ map (disp f s) xs) ++  " " ++ show u
  where u = getDisplayUnit t
        s = getUnitScale u
        f = getDisplayFormat dispLength t u


class SDisplay v where
  sdisp ::
     (S.DispApp s, TDisp t, Disp d, Ord d, D.Storage v d) =>
     TC s t (Data v d) -> String

{-
instance (TDisp t, Disp d, SV.Singleton v1 d, D.FromList (Data Nil) d)
         => SDisplay (TC Scalar t (Data Nil d)) where
  sdisp x@(TC (Data v))  = "Sig-D0 " -- ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
--    where dtyp = S.getDisplayType x
--          dmin = SV.minimum v
--          dmax = SV.maximum v

-}
instance SDisplay Nil where
  sdisp x@(TC (Data v))  = "Sig-D0 " ++ dispSingle v dtyp --  ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x


instance (SV.Singleton v1, SV.FromList v1) => SDisplay (v1 :> Nil) where
   sdisp x@(TC v) = S.dispApp (S.app x) ++ "-D1 " ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
      where dtyp = S.getDisplayType x
            _dmin = D.minimum v
            _dmax = D.maximum v

{-
instance (SV.Singleton v1 d, SV.Singleton v2 (v1 d), D.FromList (v2 :> v1 :> Nil)) => SDisplay Signal (v2 :> v1 :> Nil) where
  sdisp x@(TC(Data v))  = "Sig-D2 " ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x
          dmin = SV.minimum $ SV.minimum v
          dmax = SV.maximum $ SV.maximum v

instance (SV.Singleton v1 d, SV.Singleton v2 (v1 d), D.FromList (v2 :> v1 :> Nil)) => SDisplay FSignal (v2 :> v1 :> Nil) where
  sdisp x@(TC(Data v))  = "Sig-D2 " ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x
          dmin = SV.minimum $ SV.minimum v
          dmax = SV.maximum $ SV.maximum v

instance (SV.Singleton v1 d, SV.Singleton v2 (v1 d), D.FromList (v2 :> v1 :> Nil)) => SDisplay (TC TestRow t (Data (v2 :> v1 :> Nil) d)) where
  sdisp x@(TC(Data v))  = "Test-D2 " ++ S.tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = S.getDisplayType x
          dmin = SV.minimum $ SV.minimum v
          dmax = SV.maximum $ SV.maximum v
-}
