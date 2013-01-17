module EFA.Report.Typ where

import EFA.Signal.Typ as Typ
import EFA.Report.Base


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
                 | Typ_UZ
                 | Typ_UT deriving Show

-- | Convert Type Information ADT
class DisplayTyp t where
  getDispType :: t -> DisplayType

instance DisplayTyp P where getDispType _ = Typ_P
instance DisplayTyp E where getDispType _ = Typ_E
instance DisplayTyp F where getDispType _ = Typ_F
instance DisplayTyp N where getDispType _ = Typ_N
instance DisplayTyp M where getDispType _ = Typ_M
instance DisplayTyp T where getDispType _ = Typ_T
instance DisplayTyp X where getDispType _ = Typ_X
instance DisplayTyp Y where getDispType _ = Typ_Y
instance DisplayTyp BZ where getDispType _ = Typ_BZ
instance DisplayTyp IZ where getDispType _ = Typ_IZ
instance DisplayTyp UZ where getDispType _ = Typ_UZ
instance DisplayTyp UT where getDispType _ = Typ_UT


getDisplayTypName :: DisplayType -> String
getDisplayTypName Typ_P = "Power"
getDisplayTypName Typ_E = "Energy"
getDisplayTypName Typ_X = "Split-Share"
getDisplayTypName Typ_Y = "Collection-Share"
getDisplayTypName Typ_N = "Efficiency"
getDisplayTypName Typ_F = "Energy Flow"
getDisplayTypName Typ_UT = "Ungetypt"
getDisplayTypName Typ_T = "Time"


getDisplayTypName t = error ("Error in EFA.Report.Typ - getDisplayTypName - no Pattern Match on Typ: " ++ show  t)



-- | Function to choose display Unit per Type
getDisplayUnit :: DisplayType -> DisplayUnit
--getDisplayUnit Typ_E = Unit_kWh
--getDisplayUnit Typ_E = Unit_kJoule
getDisplayUnit Typ_E = Unit_Joule

getDisplayUnit Typ_M = Unit_Percent
getDisplayUnit Typ_T = Unit_Sec
getDisplayUnit Typ_X = Unit_Percent
getDisplayUnit Typ_Y = Unit_Percent
getDisplayUnit Typ_P = Unit_kW
--getDisplayUnit Typ_F = Unit_kWh
--getDisplayUnit Typ_F = Unit_kJoule
getDisplayUnit Typ_F = Unit_Joule

getDisplayUnit Typ_UT = Unit_UT
getDisplayUnit Typ_N = Unit_Percent

getDisplayUnit ty = error $ "getDisplayUnit: not implemented " ++ show ty

-- | Define Display Format for each unit depending on selected display length
getDisplayFormat ::  DisplayLength -> DisplayType -> DisplayUnit -> DisplayFormat
getDisplayFormat Middle Typ_E Unit_kWh = DisplayFormat "%6.7f"
getDisplayFormat Middle Typ_P Unit_kWh = DisplayFormat "%6.7f"


getDisplayFormat _ _ _ = getDefaultFormat


tdisp :: TDisp t => t -> String
tdisp x =
   tddisp x ++
   dispPhTyp (getDisplayType x) ++
   tpdisp x

udisp :: TDisp t => t -> String
udisp x = show $ getDisplayUnit $ getDisplayType x

class TDisp t where
   getDisplayType :: t -> DisplayType
   tddisp :: t -> String
   tpdisp :: t -> String

instance (DeltaDisp d, DisplayTyp t, PartDisp p) => TDisp (Typ d t p) where
   getDisplayType = getDispType . Typ.getType
   tddisp = deltaDisp . Typ.getDelta
   tpdisp = partialDisp . Typ.getPartial

-- Class to Display Partial Flag
class PartDisp p where partialDisp :: p -> String
instance PartDisp Tt where partialDisp _ = "_total"
instance PartDisp Pt where partialDisp _ = "_partial"
instance PartDisp UT where partialDisp _ = "_x"

-- Class to Display Delta Flag
class DeltaDisp d where deltaDisp :: d -> String
instance DeltaDisp A where deltaDisp _ = ""
instance DeltaDisp D where deltaDisp _ = "d"
instance DeltaDisp DD where deltaDisp _ = "dd"
instance DeltaDisp DDD where deltaDisp _ = "ddd"
instance DeltaDisp UT where deltaDisp _ = "x"


dispPhTyp ::  DisplayType -> String
dispPhTyp x = drop 4 (show x)
