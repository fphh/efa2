{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F

import qualified Data.Map as M
import Text.Printf
-- import Graphics.Gnuplot.Simple

-----------------------------------------------------------------------------------
-- Basic Data structures used 

type UVec = UV.Vector
type GVec = GV.Vector
type LVec = []
type Val = Double

-----------------------------------------------------------------------------------
-- Data Formats containing the data
  
newtype Signal a = Signal a deriving (Show, Eq)
newtype FSignal a = FSignal a deriving (Show, Eq)
newtype Distrib a = Distrib a deriving (Show, Eq)
newtype Value a = Value a  deriving (Show, Eq)
-- newtype Curve a = Curve a  deriving (Show, Eq)


--------------------------------------------------------------------------------------------
-- Helpful Type Synoymns
type DVal = Value Double
type BVal = Value Bool
type IVal = Value Int

type Sig = Signal (UV.Vector Val)
type FSig = FSignal (UV.Vector Val)
type BSig = Signal (UV.Vector Bool)
type ISig = Signal (UV.Vector Int)

type Dist = Distrib (UV.Vector Val)

-- type Cur = Curve (UV.Vector Val) 

--------------------------------------------------------------------------------------------
-- Data Container Type
-- ## TODO -- include automatic length check

class Data (cont :: * -> *) b where
   fromData :: cont a -> a
   toData :: a -> cont a
  
instance Data Signal b where 
  fromData (Signal x) = x
  toData x = Signal x                               

instance Data FSignal b where 
  fromData (FSignal x) = x
  toData x = FSignal x                               

instance Data Distrib  b where 
  fromData (Distrib x) = x 
  toData x = Distrib x                               

instance Data Value  b where 
  fromData (Value x) = x
  toData x = Value x                               

-- instance Data Curve  b where 
--   fromData (Curve x) = x
--   toData x = Curve x                               

-- Sample Calculation Class
class DProd a b c | a b -> c where
  (.*) :: a -> b -> c
  (./) :: a -> b -> c
  
instance DProd Sig Sig Sig where
  (.*) x y = toData (UV.zipWith (*) (fromData x) (fromData y))
  (./) x y = toData (UV.zipWith (/) (fromData x) (fromData y))
  
instance DProd Sig DVal Sig where
  (.*) x y = toData (UV.map (*(fromData y)) (fromData x))
  (./) x y = toData (UV.map (/(fromData y)) (fromData x))

instance DProd DVal Sig Sig where
  (.*) x y = toData (UV.map (*(fromData x)) (fromData y))
  (./) x y = toData (UV.map (/(fromData x)) (fromData y))

instance DProd FSig FSig FSig where
  (.*) x y = toData (UV.zipWith (*) (fromData x) (fromData y))
  (./) x y = toData (UV.zipWith (/) (fromData x) (fromData y))
  
instance DProd FSig DVal FSig where
  (.*) x y = toData (UV.map (*(fromData y)) (fromData x))
  (./) x y = toData (UV.map (/(fromData y)) (fromData x))

instance DProd DVal FSig FSig where
  (.*) x y = toData (UV.map (*(fromData x)) (fromData y))
  (./) x y = toData (UV.map (/(fromData x)) (fromData y))
  
instance DProd DVal DVal DVal where
  (.*) x y = toData ((*) (fromData x) (fromData x))
  (./) x y = toData ((/) (fromData x) (fromData y))

instance DProd DVal Dist Dist where
  (.*) x y = toData (UV.map (*(fromData x)) (fromData y))
  (./) x y = toData (UV.map (/(fromData x)) (fromData y))

instance DProd Dist DVal Dist where
  (.*) x y = toData (UV.map (*(fromData y)) (fromData x))
  (./) x y = toData (UV.map (/(fromData y)) (fromData x))

instance DProd Dist Dist Dist where
  (.*) x y = toData (UV.zipWith (*) (fromData x) (fromData y))
  (./) x y = toData (UV.zipWith (/) (fromData x) (fromData y))
  
{- use interp1 here to lookup efficiency in curve
instance DProd UFSig Curve UFSig  where
  (.*) x y = toData (GV.zipWith interp1 (fromData x) (fromData y))
  (./) x y = toData (GV.zipWith (*) (fromData x) (fromData y))
-}

class DSum a b c | a b -> c where
  (.+) :: a -> b -> c
  (.-) :: a -> b -> c

instance DSum Sig Sig Sig where
  (.+) x y = toData (UV.zipWith (+) (fromData x) (fromData y))
  (.-) x y = toData (UV.zipWith (-) (fromData x) (fromData y))

instance DSum Sig DVal Sig where
  (.+) x y = toData (UV.map (+(fromData y)) (fromData x))
  (.-) x y = toData (UV.map (+(fromData $ dneg y)) (fromData x))
  
instance DSum DVal Sig Sig where
  (.+) x y = toData (UV.map (+(fromData x)) (fromData y))
  (.-) x y = toData (UV.map (+(fromData $ dneg x)) (fromData y))
  
instance DSum FSig FSig FSig where
  (.+) x y = toData (UV.zipWith (+) (fromData x) (fromData y))
  (.-) x y = toData (UV.zipWith (-) (fromData x) (fromData y))

instance DSum FSig DVal FSig where
  (.+) x y = toData (UV.map (+(fromData y)) (fromData x))
  (.-) x y = toData (UV.map (+(fromData $ dneg y)) (fromData x))
  
instance DSum DVal FSig FSig where
  (.+) x y = toData (UV.map (+(fromData x)) (fromData y))
  (.-) x y = toData (UV.map (+(fromData $ dneg x)) (fromData y))

instance DSum DVal DVal DVal where
  (.+) x y = toData ((+) (fromData x) (fromData y))
  (.-) x y = toData ((-) (fromData x) (fromData y))

instance DSum DVal Dist Dist where
  (.+) x y = toData (UV.map (+(fromData x)) (fromData y))
  (.-) x y = toData (UV.map (+(fromData $ dneg x)) (fromData y))

instance DSum Dist DVal Dist where
  (.+) x y = toData (UV.map (+(fromData y)) (fromData x))
  (.-) x y = toData (UV.map (+(fromData $ dneg y)) (fromData x))

instance DSum Dist Dist Dist where
  (.+) x y = toData (UV.zipWith (+) (fromData x) (fromData y))
  (.-) x y = toData (UV.zipWith (-) (fromData x) (fromData y))


class DSingleton a where
  dneg :: a -> a
  drezip :: a -> a
  
instance DSingleton FSig where
  dneg x = toData (UV.map negate $ fromData x)
  drezip x =  toData (UV.map (1/) $ fromData x)
  
instance DSingleton DVal where
  dneg x = toData (negate $ fromData x)
  drezip x = toData (1/fromData x)


{-
instance DSum Val FSig FSig where
  (.+) x y = toData (UV.zipWith (+) (fromData x) (fromData y))
  (.-) x y = toData (UV.zipWith (-) (fromData x) (fromData y))
-}

--------------------------------------------------------------------------------------------
-- Data Sample Type

newtype P a = P a deriving (Show)
newtype E a = E a deriving (Show)
newtype T a = T a deriving (Show)
newtype X a = X a deriving (Show)
newtype M a = M a deriving (Show)
newtype N a = N a deriving (Show)

newtype DP a = DP a deriving (Show)
newtype DE a = DE a deriving (Show)
newtype DT a = DT a deriving (Show)
newtype DX a = DX a deriving (Show)
newtype DM a = DM a deriving (Show)
newtype DN a = DN a deriving (Show)


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
class   (Type t1 a, Type t2 b, Type t3 c, DProd a b c, DProd c b a)  => TProd t1 a t2 b t3 c | t1 t2 -> t3  where 
 (~*) ::  t1 a -> t2 b -> t3 c
 (~*) x y = toType ((fromType x) .* (fromType y)) 
 (~/) ::  t3 c -> t2 b -> t1 a
 (~/) x y = toType ((fromType x) ./ (fromType y)) 

instance  (DProd a b c,  DProd c b a) => TProd P a DT b  E c 
instance  (DProd a b c,  DProd c b a) => TProd DT a P b  E c   

instance  (DProd a b c,  DProd c b a) => TProd P a N b  P c  
instance  (DProd a b c,  DProd c b a) => TProd N a P b  P c  

instance  (DProd a b c,  DProd c b a) => TProd P a M b  P c  
instance  (DProd a b c,  DProd c b a) => TProd M a P b  P c  

instance  (DProd a b c,  DProd c b a) => TProd X a P b  P c  
instance  (DProd a b c,  DProd c b a) => TProd P a X b  P c  

-- Sum Class  
class   (Type t1 a, Type t2 b, Type t3 c,DSum a b c,DSum c b a)  => TSum t1 a t2 b t3 c | t1 t2 -> t3  where 
 (~+) ::  t1 a -> t2 b -> t3 c
 (~+) x y = toType ((fromType x) .+ (fromType y)) 
 (~-) ::  t3 c -> t2 b -> t1 a
 (~-) x y = toType ((fromType x) .- (fromType y)) 

-- Absolute addition
instance  (DSum a b c,DSum c b a) => TSum E a E  b E c  
instance  (DSum a b c,DSum c b a) => TSum P a P  b P c  
instance  (DSum a b c,DSum c b a) => TSum M a M  b M c  
instance  (DSum a b c,DSum c b a) => TSum N a N  b N c 
instance  (DSum a b c,DSum c b a) => TSum T a T  b T c  
instance  (DSum a b c,DSum c b a) => TSum X a X  b X c  

-- Abs + Delta  
instance  (DSum a b c,DSum c b a) => TSum DE a E  b E c  
instance  (DSum a b c,DSum c b a) => TSum E a DE  b E c  

instance  (DSum a b c,DSum c b a) => TSum DP a P  b P c  
instance  (DSum a b c,DSum c b a) => TSum P a DP  b P c  

instance  (DSum a b c,DSum c b a) => TSum DT a T  b T c  
instance  (DSum a b c,DSum c b a) => TSum T a DT  b T c  

-- Delta + Delta  
instance  (DSum a b c,DSum c b a) => TSum DE a DE  b DE c  
instance  (DSum a b c,DSum c b a) => TSum DP a DP  b DP c 
instance  (DSum a b c,DSum c b a) => TSum DM a DM  b DM c  
instance  (DSum a b c,DSum c b a) => TSum DN a DN  b DN c  
instance  (DSum a b c,DSum c b a) => TSum DT a DT  b DT c  
instance  (DSum a b c,DSum c b a) => TSum DX a DX  b DX c  

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
dispLength = Middle

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
getDisplayFormat ::  DisplayLength -> DisplayType -> DisplayUnit -> DisplayFormat
-- normal formats
getDisplayFormat Middle ETyp Unit_kWh = DisplayFormat "%6.7f"


getDisplayFormat Short  _ _ = DisplayFormat "%3.2f"
getDisplayFormat Middle _ _ = DisplayFormat "%5.3f"
getDisplayFormat Long _ _ = DisplayFormat "%6.7f"
getDisplayFormat Float _ _ = DisplayFormat "%6.7e"


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
          
instance DataDisplay FSig  where 
  ddisp (FSignal v) typ = dispRange (UV.minimum v) (UV.maximum v) typ

instance DataDisplay Dist  where 
  ddisp (Distrib v) typ = dispRange (UV.minimum v) (UV.maximum v) typ

-- instance DataDisplay Cur  where 
--   ddisp (Curve v) typ = dispRange (UV.minimum v) (UV.maximum v) typ

instance DataDisplay DVal  where 
  ddisp (Value v) typ = dispSingle v typ

class TypedDisplay (typ :: * -> *) a where
  tdisp :: typ a -> String

instance (DataDisplay a) => TypedDisplay P a where tdisp (P d) = ddisp d PTyp 
instance (DataDisplay a) => TypedDisplay E a where tdisp (E d) = ddisp d ETyp 

-- ############################
-- Testing the functionality
  
data State = ON | OFF
type StateSig = Signal (GV.Vector State)
instance Disp State where
  disp ON _ _ = "ON"
  disp OFF _ _ = "OFF"

-- lists % Vectors of different data types
l1 = [0.3,0.5]                                                  
l2 = [0.2,0.8]                                                 
l3 = [True,False] 
l4 = [1,2]
l5 = [ON,OFF]

-- signals
s1 = FSignal (UV.fromList l1) :: FSig 
s2 = FSignal (UV.fromList l2) :: FSig
s3 = Signal (UV.fromList l3) :: BSig
s4 = Signal (UV.fromList l4) :: ISig
s5 = Signal (GV.fromList l5) :: StateSig

-- Calculate with Signals
s6 = s1.*s2
s7 = s6./s2
s8 = s7.-s1
s13 = s1.+s2
s9 = s1.*v2
s10 = v2.*s1 
s11 = s1.+v1
s12 = v2.+s1
sList = [s1,s2,s6,s7,s8,s13,s9,s10,s11,s12]

-- distributions
d1 = Distrib (UV.fromList l1)
d2 = Distrib (UV.fromList l2)
d6 = d1.*d2
d7 = d6./d2
d8 = d7.-d1
d13 = d1.+d2
d9 = d1.*v2
d10 = v2.*d1 
d11 = d1.+v1
d12 = v2.+d1

dList =[d1,d2,d6,d7,d8,d13,d10,d11,d12]

-- values
v1 = Value 1.1 :: DVal
v2 = Value 2 :: DVal
v3 =v1.*v2
v4 = v1.+v2
v5 = v1.-v2

vList = [v1,v2,v3,v4,v5]

--Calculate with values



p = P s1
t = DT s2
e = p~*t 

main = do
  putStrLn "Signals"
  putStrLn $ unlines $ map (tdisp . P) sList
  putStrLn "Values"  
  putStrLn $ unlines $ map (tdisp . P) vList
  putStrLn "Distribtions"  
  putStrLn $ unlines $ map (tdisp . P) dList

  
  putStrLn $ show (e)
  putStrLn $ tdisp (e)
  

