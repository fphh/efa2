{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F

import qualified Data.Map as M
import Text.Printf

-----------------------------------------------------------------------------------
-- Basic Data structures used 

type UVec = UV.Vector
type GVec = GV.Vector
type LVec = []
type Val = Double
type IState = Int
type State = Bool

-----------------------------------------------------------------------------------
-- Data Formats containing the data
  
newtype Signal a = Signal a deriving (Show, Eq)
newtype Distrib a = Distrib a deriving (Show, Eq)
newtype Value a = Value a  deriving (Show, Eq)
newtype Curve a = Curve a  deriving (Show, Eq)


--------------------------------------------------------------------------------------------
-- Helpful Type Synoymns
type Sig = Signal (UV.Vector Val)
-- type LSig = Signal [Val]
-- type SignSig = Signal (GV.Vector Sign)


type BSig = Signal (UV.Vector Bool)
-- type LBSig = Signal [Bool]

type ISig = Signal (UV.Vector Int)
-- type LISig = Signal [Int]



type DVal = Value Double
type BVal = Value Bool
type IVal = Value Int

type Dist = Distrib (UV.Vector Val)

type Cur = Curve (UV.Vector Val) 

-- ?? Do we need a signal for user defined types here as well ?? --
-- type VISig a = (Num a) ... 
-- no hand-code specifically

--------------------------------------------------------------------------------------------
-- Data Container Type
-- ## TODO -- include automatic length check

class Data (cont :: * -> *) b where
   fromData :: cont a -> a
   toData :: a -> cont a
  
instance Data Signal b where fromData (Signal x) = x; toData x = Signal x                               
instance Data Distrib  b where fromData (Distrib x) = x; toData x = Distrib x                               
instance Data Value  b where fromData (Value x) = x; toData x = Value x                               
instance Data Curve  b where fromData (Curve x) = x; toData x = Curve x                               

-- Sample Calculation Class
class DProd a b c | a b -> c , b c -> a where
  (.*) :: a -> b -> c
  (./) :: c -> b -> a
  
instance DProd Sig Sig Sig where
  (.*) x y = toData (UV.zipWith (*) (fromData x) (fromData y))
  (./) x y = toData (UV.zipWith (/) (fromData x) (fromData y))

-- instance DProd VSig VSig VSig where
--   (.*) x y = toData (GV.zipWith (*) (fromData x) (fromData y))
--   (./) x y = toData (GV.zipWith (/) (fromData x) (fromData y))

instance DProd Sig DVal Sig where
  (.*) x y = toData (UV.map (*(fromData y)) (fromData x))
  (./) x y = toData (UV.map (/(fromData y)) (fromData x))

-- Generating Curves & Calculating with curves 

instance DProd Dist Cur Dist where
  (.*) x y = toData (UV.zipWith (*) (fromData x) (fromData y))
  (./) x y = toData (UV.zipWith (/) (fromData x) (fromData y))

{- use interp1 here to lookup efficiency in curve
instance DProd USig Curve USig  where
  (.*) x y = toData (GV.zipWith interp1 (fromData x) (fromData y))
  (./) x y = toData (GV.zipWith (*) (fromData x) (fromData y))
-}

class DSum a b c | a b -> c , b c -> a where
  (.+) :: a -> b -> c
  (.-) :: c -> b -> a
  
instance DSum Sig Sig Sig where
  (.+) x y = toData (UV.zipWith (+) (fromData x) (fromData y))
  (.-) x y = toData (UV.zipWith (-) (fromData x) (fromData y))

--------------------------------------------------------------------------------------------
-- Data Sample Type

data P a = P a deriving (Show)
data E a = E a deriving (Show)
data T a = T a deriving (Show)
data X a = X a deriving (Show)
data M a = M a deriving (Show)
data N a = N a deriving (Show)

data DP a = DP a deriving (Show)
data DE a = DE a deriving (Show)
data DT a = DT a deriving (Show)
data DX a = DX a deriving (Show)
data DM a = DM a deriving (Show)
data DN a = DN a deriving (Show)


-- Type Class

class Type (typ :: * -> *) x where
  toType :: x -> typ x
  fromType :: typ x -> x

instance Type E a where  
  fromType (E x) = x
  toType x = E x

instance Type M a where
  toType x = (M x)
  fromType (M x) = x

instance Type N a where
  toType x = (N x)
  fromType (N x) = x

instance Type P a where
  toType x = (P x)
  fromType (P x) = x
  
instance Type T a where  
  fromType (T x) = x
  toType x = T x

instance Type X a where  
  fromType (X x) = x
  toType x = X x

instance Type DE a where  
  fromType (DE x) = x
  toType x = DE x

instance Type DM a where
  toType x = (DM x)
  fromType (DM x) = x

instance Type DN a where
  toType x = (DN x)
  fromType (DN x) = x

instance Type DP a where
  toType x = (DP x)
  fromType (DP x) = x
  
instance Type DT a where  
  fromType (DT x) = x
  toType x = DT x

instance Type DX a where  
  fromType (DX x) = x
  toType x = DX x

-- Product Class
class   (Type t1 a, Type t2 b, Type t3 c, DProd a b c)  => TProd t1 a t2 b t3 c | t1 t2 -> t3  where 
 (~*) ::  t1 a -> t2 b -> t3 c
 (~*) x y = toType ((fromType x) .* (fromType y)) 
 (~/) ::  t3 c -> t2 b -> t1 a
 (~/) x y = toType ((fromType x) ./ (fromType y)) 

instance  DProd a b c => TProd P a DT b  E c where 
instance  DProd a b c => TProd DT a P b  E c where 

instance  DProd a b c => TProd P a N b  P c where 
instance  DProd a b c => TProd N a P b  P c where 

instance  DProd a b c => TProd P a M b  P c where 
instance  DProd a b c => TProd M a P b  P c where 

instance  DProd a b c => TProd X a P b  P c where 
instance  DProd a b c => TProd P a X b  P c where 

-- Sum Class  
class   (Type t1 a, Type t2 b, Type t3 c, DSum a b c)  => TSum t1 a t2 b t3 c | t1 t2 -> t3  where 
 (~+) ::  t1 a -> t2 b -> t3 c
 (~+) x y = toType ((fromType x) .+ (fromType y)) 
 (~-) ::  t3 c -> t2 b -> t1 a
 (~-) x y = toType ((fromType x) .- (fromType y)) 

-- Absolute addition
instance  DSum a b c => TSum E a E  b E c where 
instance  DSum a b c => TSum P a P  b P c where 
instance  DSum a b c => TSum M a M  b M c where 
instance  DSum a b c => TSum N a N  b N c where
instance  DSum a b c => TSum T a T  b T c where 
instance  DSum a b c => TSum X a X  b X c where 

-- Abs + Delta  
instance  DSum a b c => TSum DE a E  b E c where 
instance  DSum a b c => TSum E a DE  b E c where 

instance  DSum a b c => TSum DP a P  b P c where 
instance  DSum a b c => TSum P a DP  b P c where 

instance  DSum a b c => TSum DT a T  b T c where 
instance  DSum a b c => TSum T a DT  b T c where 

-- Delta + Delta  
instance  DSum a b c => TSum DE a DE  b DE c where 
instance  DSum a b c => TSum DP a DP  b DP c where
instance  DSum a b c => TSum DM a DM  b DM c where 
instance  DSum a b c => TSum DN a DN  b DN c where 
instance  DSum a b c => TSum DT a DT  b DT c where 
instance  DSum a b c => TSum DX a DX  b DX c where 

-- ############################ Display #############################
  
  
  
  
-- | Typ variable to control display  
-- class (Show a, PrintfArg a,Fractional a) => TShow a

data DisplayLength = Short | Middle | Long | Float 
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
                   
                   
data DisplayUnit = Unit_kWh | Unit_kW | Unit_Percent | Unit_none | Unit_sec
data DisplayFormat = DisplayFormat String
data UnitScale = UnitScale Val


-- Central Display Class to Controll skaling and display of NULL 
class Disp a where 
  disp :: a -> DisplayFormat -> UnitScale -> String

instance Disp Val where
  disp 0 _ _ = "Null"
  disp x (DisplayFormat f) (UnitScale s) = printf f $ x*s

instance Disp Bool where 
  disp x (DisplayFormat f) _ = printf f (show x)

instance Disp Int where
  disp x (DisplayFormat f) _ = printf f (show x)

-- ##################### Settings are here #####################
dispLength = Short

-- | Function to choose display Unit 
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


-- | getDisplay formats
getDisplayFormats :: DisplayType -> DisplayUnit -> (DisplayFormat,DisplayFormat,DisplayFormat,DisplayFormat)
getDisplayFormats ETyp Unit_kWh = (DisplayFormat "%6.7f",DisplayFormat "%6.7f",DisplayFormat "%6.7f",DisplayFormat "%6.7f")
getDisplayFormats _ _ = (DisplayFormat "%6.7f",DisplayFormat "%6.7f",DisplayFormat "%6.7f",DisplayFormat "%6.7f")


-- ##################### Settings are above #####################

instance Show DisplayUnit where
  show Unit_kWh  = "kWh" 
  show Unit_kW   = "kW"
  show Unit_Percent = "%"
  show Unit_none = "/"

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



-- | get display scale per Unit 
getUnitScale :: DisplayUnit -> UnitScale  
getUnitScale Unit_kWh = UnitScale (1/1000/3600)
getUnitScale Unit_none = UnitScale 1
getUnitScale Unit_Percent = UnitScale 100
getUnitScale Unit_sec = UnitScale 1
getUnitScale u = error ("Error in getUnitScale -- no scale defined - Unit: " ++ show u)


-- | choose display format
formatSelect :: DisplayLength -> (DisplayFormat,DisplayFormat,DisplayFormat,DisplayFormat) -> DisplayFormat
formatSelect Short (f1,f2,f3,f4) = f1
formatSelect Middle (f1,f2,f3,f4) = f2
formatSelect Long (f1,f2,f3,f4) = f3
formatSelect Float (f1,f2,f3,f4) = f4

-- | display a single value  
dispSingle :: Disp a => a -> DisplayType -> String
dispSingle x t = disp x f s  ++ " " ++ show u
           where u = getDisplayUnit t
                 s = getUnitScale u
                 f = formatSelect dispLength $ getDisplayFormats t u 

-- | display a single value  
dispRange :: Disp a => a -> a -> DisplayType -> String
dispRange x y t = disp x f s  ++ " - " ++ disp y f s  ++ " " ++ show u
           where u = getDisplayUnit t
                 s = getUnitScale u
                 f = formatSelect dispLength $ getDisplayFormats t u 
 
                               
class DataDisplay a where
  ddisp :: a -> DisplayType -> String
          
instance DataDisplay Sig  where 
  ddisp (Signal v) typ = dispRange (UV.minimum v) (UV.maximum v) typ


class TypedDisplay (typ :: * -> *) a where
  tdisp :: typ a -> String

instance (DataDisplay a) => TypedDisplay P a where
  tdisp (P d) = ddisp d PTyp 
  
instance (DataDisplay a) => TypedDisplay E a where
  tdisp (E d) = ddisp d ETyp 

-- ########################## Signal Plotting #####################


  
  
  
  
  
  
s1 = Signal (UV.fromList [0.5,0.3::Val]) 
s2 = Signal (UV.fromList [0.8,0.2::Val])
v = Value 2 :: Value Val
-- d = Signal (UV.fromList [0.5,0.3])


s =  (s1.*s2) :: Signal (UV.Vector Val)
s'=s.*v

p = P s1
t = DT s2
e = p~*t :: E  (Signal (UV.Vector Val))

main = do
  putStrLn $ show (s)
  putStrLn $ show (s')
  putStrLn $ show (e)
  putStrLn $ tdisp (e)
  
  
--  putStrLn $ show (d.*s2)

