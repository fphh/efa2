{-# LANGUAGE FlexibleInstances #-}

module EFA2.Display.DispTyp (module EFA2.Display.DispTyp) where

import EFA2.Signal.Typ
import EFA2.Signal.Signal
import EFA2.Display.DispBase

data DisplayType = Typ_BZ
                | Typ_E  
                | Typ_F 
                | Typ_IZ  
                | Typ_N  
                | Typ_M  
                | Typ_P   
                | Typ_T  
                | Typ_X  
                | Typ_Y  
                | Typ_UZ  deriving Show

-- | Convert Type Information ADT
class DisplayTyp t where 
  getDisplayType :: TC s t d -> DisplayType
  
instance DisplayTyp (Typ d P p) where getDisplayType x = Typ_P
instance DisplayTyp (Typ d E p) where getDisplayType x = Typ_E
instance DisplayTyp (Typ d F p) where getDisplayType x = Typ_F
instance DisplayTyp (Typ d N p) where getDisplayType x = Typ_N
instance DisplayTyp (Typ d M p) where getDisplayType x = Typ_M
instance DisplayTyp (Typ d T p) where getDisplayType x = Typ_T
instance DisplayTyp (Typ d X p) where getDisplayType x = Typ_X
instance DisplayTyp (Typ d Y p) where getDisplayType x = Typ_Y
instance DisplayTyp (Typ d BZ p) where getDisplayType x = Typ_BZ
instance DisplayTyp (Typ d IZ p) where getDisplayType x = Typ_IZ
instance DisplayTyp (Typ d UZ p) where getDisplayType x = Typ_UZ

-- | Function to choose display Unit per Type
getDisplayUnit :: DisplayType -> DisplayUnit
getDisplayUnit Typ_E = Unit_kWh
getDisplayUnit Typ_M = Unit_Percent
getDisplayUnit Typ_T = Unit_Sec
getDisplayUnit Typ_X = Unit_Percent
getDisplayUnit Typ_Y = Unit_Percent
getDisplayUnit _ = Unit_None


-- | Define Display Format for each unit depending on selected display length
getDisplayFormat ::  DisplayLength -> DisplayType -> DisplayUnit -> DisplayFormat
getDisplayFormat Middle Typ_E Unit_kWh = DisplayFormat "%6.7f"
getDisplayFormat _ _ _ = getDefaultFormat


tdisp :: (DeltaDisp t,PartDisp t,  DisplayTyp t) => TC s t d -> String 
tdisp x = tddisp x ++ "_" ++ dispPhTyp (getDisplayType x) ++ "_" ++ tpdisp x
  
udisp :: (DeltaDisp t,PartDisp t,  DisplayTyp t) => TC s t d -> String 
udisp x = show $ getDisplayUnit (getDisplayType x)


-- Class to Display Partial Flag
class DeltaDisp t where   tddisp :: TC s t d -> String 
instance DeltaDisp (Typ d t Tt) where tddisp x = "Total"
instance DeltaDisp (Typ d t Pt) where tddisp x = "Partial"

-- Class to Display Delta Flag
class PartDisp t where tpdisp :: TC s t d -> String 
instance PartDisp (Typ A t p) where tpdisp x = "A"
instance PartDisp (Typ D t p) where tpdisp x = "D"
instance PartDisp (Typ DD t p) where tpdisp x = "DD"
instance PartDisp (Typ DDD t p) where tpdisp x = "DDD"
  
  
dispPhTyp ::  DisplayType -> String
dispPhTyp x = drop 4 (show x) 