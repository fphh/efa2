{-# LANGUAGE TypeSynonymInstances  #-}

module EFA2.Display.DispBase (module EFA2.Display.DispBase) where


import Text.Printf


import EFA2.Signal.Base


-- | Central Place for basic Unit & Display settings

-- | Variables for DisplayUnit and Display Scale 
data DisplayUnit = Unit_kWh | Unit_kW | Unit_Percent | Unit_None | Unit_Sec
data UnitScale = UnitScale Val


-- | Unit Show Instance
instance Show DisplayUnit where
  show Unit_kWh  = "kWh" 
  show Unit_kW   = "kW"
  show Unit_Percent = "%"
  show Unit_None = "/"
  show Unit_Sec = "s"

-- | get display scale per Unit 
getUnitScale :: DisplayUnit -> UnitScale  
getUnitScale Unit_kWh = UnitScale (1/1000/3600)
getUnitScale Unit_None = UnitScale 1
getUnitScale Unit_Percent = UnitScale 100
getUnitScale Unit_Sec = UnitScale 1
getUnitScale Unit_kW = UnitScale (1/1000)
getUnitScale u = error ("Error in getUnitScale -- no scale defined - Unit: " ++ show u)

 -- ============ Setting - Switch global display length =============

-- | Central Place to switch Display Formats througght the system
data DisplayLength = Short | Middle | Long | Float 
-- dispLength = Short
dispLength = Middle
-- dispLength = Long
-- dispLength = Float

-- ============ Setting - Switch global display length =============

-- | Generic Display Format Variable
data DisplayFormat = DisplayFormat String

getDefaultFormat :: DisplayFormat
getDefaultFormat = f dispLength
  where f Long = DisplayFormat "%3.2f"
        f Middle = DisplayFormat "%5.3f"
        f Short = DisplayFormat "%6.7f"
        f Float = DisplayFormat "%6.7e"


-- | Simple Display Function
sdisp x = disp x getDefaultFormat  (UnitScale 1)

-- | Display Class using Format & Scale
class Disp a where 
  disp :: a -> DisplayFormat -> UnitScale -> String

instance Disp Val where
  disp 0 _ _ = "Null"
  disp x (DisplayFormat f) (UnitScale s) = printf f $ x*s

instance Disp Bool where 
  disp x (DisplayFormat f) _ = printf f (show x)

instance Disp Int where
  disp x (DisplayFormat f) _ = printf f (show x)

instance Disp Sign where
  disp x (DisplayFormat f) _ = show x