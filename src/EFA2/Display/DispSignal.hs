module EFA2.Display.Signal (module EFA2.Display.Signal) where

-- | Module for Display of the typed Data Container DC containing Signals and other stuff

-- | Function to choose display Unit per Type
getDisplayUnit :: DisplayType -> DisplayUnit
getDisplayUnit ETyp = Unit_kWh
getDisplayUnit MTyp = Unit_Percent
getDisplayUnit TTyp = Unit_sec
getDisplayUnit XTyp = Unit_Percent
getDisplayUnit DETyp = Unit_kWh
getDisplayUnit DMTyp = Unit_Percent
getDisplayUnit DTTyp = Unit_sec
getDisplayUnit DXTyp = Unit_Percent
getDisplayUnit _ = Unit_none


-- | Define Display Format for each unit depending on selected display length
getDisplayFormat ::  DisplayLength -> DisplayType -> DisplayUnit -> DisplayFormat
getDisplayFormat Middle ETyp Unit_kWh = DisplayFormat "%6.7f"
getDisplayFormat Short  _ _ = DisplayFormat "%3.2f"
getDisplayFormat Middle _ _ = DisplayFormat "%5.3f"
getDisplayFormat Long _ _ = DisplayFormat "%6.7f"
getDisplayFormat Float _ _ = DisplayFormat "%6.7e"

-- ##################### ^ Settings are above ^ #####################

-- | Typ variable to control display  
data DisplayType = BTyp  -- boolean state 
                 | ETyp 
                 | ITyp  -- integer state
                 | NTyp 
                 | MTyp 
                 | PTyp 
                 | XTyp 
                 | TTyp 
                 | DBTyp  -- boolean state
                 | DETyp 
                 | DITyp -- delta integert state  
                 | DNTyp 
                 | DMTyp 
                 | DPTyp 
                 | DXTyp 
                 | DTTyp
                   


instance Show DisplayType where
  show ETyp = "E"
  show MTyp = "M"
  show NTyp = "N"
  show TTyp = "T"
  show XTyp = "X"
  show DETyp = "DE"
  show DMTyp = "DM"
  show DNTyp = "DN"
  show DTTyp = "DT"
  show DXTyp = "DX"


-- | display a single value  
dispSingle :: Disp a => a -> DisplayType -> String
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
 
 
class DataDisplay a where
  ddisp :: a -> DisplayType -> String

instance DataDisplay Sig  where 
  ddisp (Signal v) typ = dispRange (UV.minimum v) (UV.maximum v) typ


instance DataDisplay DVal  where 
  ddisp (Value v) typ = dispSingle v typ

class TypedDisplay (typ :: * -> *) a where
  tdisp :: typ a -> String

instance (DataDisplay a) => TypedDisplay P a where tdisp (P d) = ddisp d PTyp 
instance (DataDisplay a) => TypedDisplay E a where tdisp (E d) = ddisp d ETyp 


-- | Display for DC Vector Data Container
class DataDisplay s t h e where
  ddisp :: TC s t h e -> DisplayType -> String
  
 