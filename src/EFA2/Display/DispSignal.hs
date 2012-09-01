{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, FlexibleContexts, TypeOperators #-}

module EFA2.Display.DispSignal (module EFA2.Display.DispSignal) where

import EFA2.Display.DispTyp
import EFA2.Display.DispBase

import EFA2.Display.Report
import qualified EFA2.Signal.Signal as S
-- import qualified EFA2.Signal.Data as D
import qualified EFA2.Signal.Vector as SV
import EFA2.Signal.Signal (TC(TC), Scalar, Signal, FSignal, TestRow)
import EFA2.Signal.Data (Data(Data), (:>), Nil)
import EFA2.Signal.Base

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
        (TC (Data val)) = x

-- | Display single values
srdisp :: (DisplayTyp t, SV.FromList v Double) => TC s t (Data (v :> Nil) Val)  -> [String]
srdisp xs = map g l -- (f l)
  where g x = printf f (s*x)
        t = getDisplayType xs
        u = getDisplayUnit t
        (UnitScale s) = getUnitScale u
        (DisplayFormat f) = getDisplayFormat dispLength t u
        l = S.toList xs


-- | Display Signal Type
class SigDisp s c where
      sigDisp :: TC s t (Data c d)  -> String

instance SigDisp Signal ([] :> Nil) where
         sigDisp _ = "Sig1L"

instance SigDisp Signal (UV.Vector :> Nil) where
         sigDisp _ = "Sig1U"

instance SigDisp Signal (V.Vector :> Nil) where
         sigDisp _ = "Sig1V"

instance SigDisp Signal ([] :> [] :> Nil) where
         sigDisp _ = "Sig2L"

instance SigDisp Signal (V.Vector :> UV.Vector :> Nil) where
         sigDisp _ = "Sig2U"

instance SigDisp Signal (V.Vector :> V.Vector :> Nil) where
         sigDisp _ = "Sig2V"

instance SigDisp TestRow (V.Vector :> UV.Vector :> Nil) where
         sigDisp _ = "Test2U"

instance
      (SV.FromList v Val, SV.Singleton v Double, SV.Walker v Double Double,
       UDisp t, SigDisp s (v :> Nil)) =>
          ToTable (TC s t (Data (v :> Nil) Val)) where
      toTable os (ti,x) = [Table {tableTitle = "",
                         tableFormat = autoFormat td,
                         tableData = td,
                         tableSubTitle = ""}]
        where td = TableData {tableBody =  [map (toDoc id ) (f x) ],
                              titleRow = [],
                              titleCols = [map (toDoc id) [ti,sigDisp x,tdisp x]],
                              endCols = [[toDoc id $ udisp x]]}

              f y
                 | L.elem RAll os = srdisp y
                 | otherwise = [vdisp (S.minimum x) ++ " - " ++ vdisp (S.maximum y)]

instance (UDisp t,
          SigDisp s (v2 :> (v1 :> Nil)),
          SV.FromList v1 Val,
          SV.FromList v2 [Val],
          SV.FromList v2 (v1 Val),
          SV.Walker v2 (v1 Val) [Val]) => ToTable (TC s t (Data (v2 :> v1 :> Nil) Val)) where
      toTable _os (ti,xss) = [Table {tableTitle = ti ++ "   " ++ sigDisp xss ++ tdisp xss ++ udisp xss,
                         tableFormat = tf,
                         tableData = td,
                         tableSubTitle = ""}]
        where td = TableData {tableBody =  map (map (toDoc id .f) ) (S.toCells xss),
                              titleRow = [],
                              titleCols = [],
                              endCols = []}

              tf = autoFormat td
              f x = vdisp x
--              f x | otherwise = [(vdisp min) ++ " - " ++ (vdisp max)]

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
instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d, SV.Singleton v1 d, D.FromList (Data Nil) d)
         => SDisplay (TC Scalar t (Data Nil d)) where
  sdisp x@(TC (Data v))  = "Sig-D0 " -- ++ tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
--    where dtyp = getDisplayType x
--          dmin = SV.minimum v
--          dmax = SV.maximum v

-}
instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d)
         => SDisplay (TC Scalar t (Data Nil d)) where
  sdisp x@(TC (Data v))  = "Sig-D0 " ++ dispSingle v dtyp --  ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = getDisplayType x


instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d, SV.Singleton v1 d, SV.FromList v1 d)
         => SDisplay (TC Signal t (Data (v1 :> Nil) d)) where
  sdisp x@(TC (Data v))  = "Sig-D1 " ++ tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = getDisplayType x
          _dmin = SV.minimum v
          _dmax = SV.maximum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d, SV.Singleton v1 d, SV.FromList v1 d)
         => SDisplay (TC FSignal t (Data (v1 :> Nil) d)) where
  sdisp x@(TC (Data v))  = "FSig-D1 " ++ tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = getDisplayType x
          _dmin = SV.minimum v
          _dmax = SV.maximum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d, SV.Singleton v1 d, SV.FromList v1 d)
         => SDisplay (TC TestRow t (Data (v1 :> Nil) d)) where
  sdisp x@(TC (Data v))  = "TestRow-D1 " ++ tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = getDisplayType x
          _dmin = SV.minimum v
          _dmax = SV.maximum v

{-
instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d, SV.Singleton v1 d, SV.Singleton v2 (v1 d), D.FromList (v2 :> (v1 :> Nil)) d) => SDisplay (TC Signal t (Data (v2 :> v1 :> Nil) d)) where
  sdisp x@(TC(Data v))  = "Sig-D2 " ++ tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = getDisplayType x
          dmin = SV.minimum $ SV.minimum v
          dmax = SV.maximum $ SV.maximum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d, SV.Singleton v1 d, SV.Singleton v2 (v1 d), D.FromList (v2 :> (v1 :> Nil)) d) => SDisplay (TC FSignal t (Data (v2 :> v1 :> Nil) d)) where
  sdisp x@(TC(Data v))  = "Sig-D2 " ++ tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = getDisplayType x
          dmin = SV.minimum $ SV.minimum v
          dmax = SV.maximum $ SV.maximum v

instance (DeltaDisp t, DisplayTyp t, PartDisp t, Disp d, SV.Singleton v1 d, SV.Singleton v2 (v1 d), D.FromList (v2 :> (v1 :> Nil)) d) => SDisplay (TC TestRow t (Data (v2 :> v1 :> Nil) d)) where
  sdisp x@(TC(Data v))  = "Test-D2 " ++ tdisp x ++ ": " ++ dispAll (S.toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = getDisplayType x
          dmin = SV.minimum $ SV.minimum v
          dmax = SV.maximum $ SV.maximum v
-}
