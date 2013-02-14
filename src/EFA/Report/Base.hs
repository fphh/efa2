{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
module EFA.Report.Base where

import EFA.Signal.Data (Data, (:>), Nil, subData)
import EFA.Signal.Base (Sign)

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

import Text.Printf (printf)


-- | Central Place for basic Unit & Display settings

-- | Variables for DisplayUnit and Display Scale
data DisplayUnit = Unit_kWh | Unit_Joule | Unit_kJoule | Unit_kW | Unit_Percent | Unit_None | Unit_Sec | Unit_UT
newtype UnitScale = UnitScale Rational


-- | Unit Show Instance
instance Show DisplayUnit where
  show Unit_kWh  = "kWh"
  show Unit_Joule  = "J"
  show Unit_kJoule  = "kJ"
  show Unit_kW   = "kW"
  show Unit_Percent = "%"
  show Unit_None = "/"
  show Unit_Sec = "s"
  show Unit_UT = "UT"

-- | get display scale per Unit
getUnitScale :: DisplayUnit -> UnitScale
getUnitScale Unit_kWh = UnitScale (1/1000/3600)
getUnitScale Unit_Joule = UnitScale 1
getUnitScale Unit_kJoule = UnitScale (1/1000)
getUnitScale Unit_None = UnitScale 1
getUnitScale Unit_Percent = UnitScale 100
getUnitScale Unit_Sec = UnitScale 1
getUnitScale Unit_kW = UnitScale (1/1000)
getUnitScale Unit_UT = UnitScale 1
-- getUnitScale u = error ("Error in getUnitScale -- no scale defined - Unit: " ++ show u)

 -- ============ Setting - Switch global display length =============

-- | Central Place to switch Display Formats througght the system
data DisplayLength = Short | Middle | Long | Float

dispLength :: DisplayLength
-- dispLength = Short
-- dispLength = Middle
--dispLength = Middle
dispLength = Float

-- ============ Setting - Switch global display length =============

-- | Generic Display Format Variable
newtype DisplayFormat = DisplayFormat String

getDefaultFormat :: DisplayFormat
getDefaultFormat =
   case dispLength of
      Short -> DisplayFormat "%3.2f"
      Middle -> DisplayFormat "%5.3f"
      Long -> DisplayFormat "%6.7f"
      Float -> DisplayFormat "%6.7e"


-- | Simple Display Function
xdisp :: Disp a => a -> String
xdisp x = disp getDefaultFormat (UnitScale 1) x

-- | Display Class using Format & Scale
class Disp a where
  disp :: DisplayFormat -> UnitScale -> a -> String

instance Disp Double where
  disp _ _ 0 = "null"
  disp (DisplayFormat f) (UnitScale s) x = printf f $ x * fromRational s

instance Disp Bool where
  disp (DisplayFormat f) _ x = printf f (show x)

instance Disp Int where
  disp (DisplayFormat f) _ x = printf f (show x)

instance Disp Sign where
  disp (DisplayFormat _f) _ x = show x


-- | Display Signal Type
class DispStorage (c :: * -> *) where
   dispStorage :: Data c d -> String

instance DispStorage1 v => DispStorage (v :> Nil) where
   dispStorage s = "1" ++ dispStorage1 s

instance
   (DispStorage1 v1, StorageCollection v1 ~ v2) =>
      DispStorage (v2 :> v1 :> Nil) where
   dispStorage s = "2" ++ dispStorage1 (subData s (error "DispStorage.subData"))


class DispStorage1 v where
   type StorageCollection v :: * -> *
   dispStorage1 :: Data (v :> Nil) d -> String

instance DispStorage1 [] where
   type StorageCollection [] = []
   dispStorage1 _ = "L"

instance DispStorage1 UV.Vector where
   type StorageCollection UV.Vector = V.Vector
   dispStorage1 _ = "U"

instance DispStorage1 V.Vector where
   type StorageCollection V.Vector = V.Vector
   dispStorage1 _ = "V"
