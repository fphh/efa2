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

class Container (cont :: * -> *) b where
   fromContainer :: cont a -> a
   toContainer :: a -> cont a
  
instance Container Signal b where 
  fromContainer (Signal x) = x
  toContainer x = Signal x                               

instance Container FSignal b where 
  fromContainer (FSignal x) = x
  toContainer x = FSignal x                               

instance Container Distrib  b where 
  fromContainer (Distrib x) = x 
  toContainer x = Distrib x                               

instance Container Value  b where 
  fromContainer (Value x) = x
  toContainer x = Value x                               

-- instance Container Curve  b where 
--   fromContainer (Curve x) = x
--   toContainer x = Curve x                               

-- Sample Calculation Class
class DProd a b c | a b -> c where
  (.*) :: a -> b -> c
  (./) :: a -> b -> c
  
instance DProd Sig Sig Sig where
  (.*) x y = toContainer (UV.zipWith (*) (fromContainer x) (fromContainer y))
  (./) x y = toContainer (UV.zipWith (/) (fromContainer x) (fromContainer y))
  
instance DProd Sig DVal Sig where
  (.*) x y = toContainer (UV.map (*(fromContainer y)) (fromContainer x))
  (./) x y = toContainer (UV.map (/(fromContainer y)) (fromContainer x))

instance DProd DVal Sig Sig where
  (.*) x y = toContainer (UV.map (*(fromContainer x)) (fromContainer y))
  (./) x y = toContainer (UV.map (/(fromContainer x)) (fromContainer y))
  
instance DProd BSig Sig Sig where
  (.*) x y = toContainer (UV.zipWith (f) (fromContainer x) (fromContainer y)) 
    where f True y = y
          f False y = 0
          
  (./) x y = toContainer (UV.zipWith (f) (fromContainer x) (fromContainer y))
    where f True y = 0
          f False y = y

instance DProd Sig BSig Sig where
  (.*) x y = toContainer (UV.zipWith (f) (fromContainer x) (fromContainer y)) 
    where f y True = y
          f y False = 0
          
  (./) x y = toContainer (UV.zipWith (f) (fromContainer x) (fromContainer y))
    where f y True = 0
          f y False = y

instance DProd FSig FSig FSig where
  (.*) x y = toContainer (UV.zipWith (*) (fromContainer x) (fromContainer y))
  (./) x y = toContainer (UV.zipWith (/) (fromContainer x) (fromContainer y))
  
instance DProd FSig DVal FSig where
  (.*) x y = toContainer (UV.map (*(fromContainer y)) (fromContainer x))
  (./) x y = toContainer (UV.map (/(fromContainer y)) (fromContainer x))

instance DProd DVal FSig FSig where
  (.*) x y = toContainer (UV.map (*(fromContainer x)) (fromContainer y))
  (./) x y = toContainer (UV.map (/(fromContainer x)) (fromContainer y))
  
instance DProd DVal DVal DVal where
  (.*) x y = toContainer ((*) (fromContainer x) (fromContainer x))
  (./) x y = toContainer ((/) (fromContainer x) (fromContainer y))

instance DProd DVal Dist Dist where
  (.*) x y = toContainer (UV.map (*(fromContainer x)) (fromContainer y))
  (./) x y = toContainer (UV.map (/(fromContainer x)) (fromContainer y))

instance DProd Dist DVal Dist where
  (.*) x y = toContainer (UV.map (*(fromContainer y)) (fromContainer x))
  (./) x y = toContainer (UV.map (/(fromContainer y)) (fromContainer x))

instance DProd Dist Dist Dist where
  (.*) x y = toContainer (UV.zipWith (*) (fromContainer x) (fromContainer y))
  (./) x y = toContainer (UV.zipWith (/) (fromContainer x) (fromContainer y))
  
{- use interp1 here to lookup efficiency in curve
instance DProd UFSig Curve UFSig  where
  (.*) x y = toContainer (GV.zipWith interp1 (fromContainer x) (fromContainer y))
  (./) x y = toContainer (GV.zipWith (*) (fromContainer x) (fromContainer y))
-}

class DSum a b c | a b -> c where
  (.+) :: a -> b -> c
  (.-) :: a -> b -> c

instance DSum Sig Sig Sig where
  (.+) x y = toContainer (UV.zipWith (+) (fromContainer x) (fromContainer y))
  (.-) x y = toContainer (UV.zipWith (-) (fromContainer x) (fromContainer y))

instance DSum Sig DVal Sig where
  (.+) x y = toContainer (UV.map (+(fromContainer y)) (fromContainer x))
  (.-) x y = toContainer (UV.map (+(fromContainer $ cneg y)) (fromContainer x))
  
instance DSum DVal Sig Sig where
  (.+) x y = toContainer (UV.map (+(fromContainer x)) (fromContainer y))
  (.-) x y = toContainer (UV.map (+(fromContainer $ cneg x)) (fromContainer y))
  
instance DSum FSig FSig FSig where
  (.+) x y = toContainer (UV.zipWith (+) (fromContainer x) (fromContainer y))
  (.-) x y = toContainer (UV.zipWith (-) (fromContainer x) (fromContainer y))

instance DSum FSig DVal FSig where
  (.+) x y = toContainer (UV.map (+(fromContainer y)) (fromContainer x))
  (.-) x y = toContainer (UV.map (+(fromContainer $ cneg y)) (fromContainer x))
  
instance DSum DVal FSig FSig where
  (.+) x y = toContainer (UV.map (+(fromContainer x)) (fromContainer y))
  (.-) x y = toContainer (UV.map (+(fromContainer $ cneg x)) (fromContainer y))

instance DSum DVal DVal DVal where
  (.+) x y = toContainer ((+) (fromContainer x) (fromContainer y))
  (.-) x y = toContainer ((-) (fromContainer x) (fromContainer y))

instance DSum DVal Dist Dist where
  (.+) x y = toContainer (UV.map (+(fromContainer x)) (fromContainer y))
  (.-) x y = toContainer (UV.map (+(fromContainer $ cneg x)) (fromContainer y))

instance DSum Dist DVal Dist where
  (.+) x y = toContainer (UV.map (+(fromContainer y)) (fromContainer x))
  (.-) x y = toContainer (UV.map (+(fromContainer $ cneg y)) (fromContainer x))

instance DSum Dist Dist Dist where
  (.+) x y = toContainer (UV.zipWith (+) (fromContainer x) (fromContainer y))
  (.-) x y = toContainer (UV.zipWith (-) (fromContainer x) (fromContainer y))


class DSingleton a where
  cneg :: a -> a
  crezip :: a -> a
--  cnull:: a -> a 
  
instance DSingleton FSig where
  cneg x = toContainer (UV.map negate $ fromContainer x)
  crezip x =  toContainer (UV.map (1/) $ fromContainer x)
--  cnull x = toContainer . (UV.map (0)) . (fromContainer x)
  
instance DSingleton DVal where
  cneg x = toContainer (negate $ fromContainer x)
  crezip x = toContainer (1/fromContainer x)
--  cmap x =  toContainer . id . (fromContainer x)

class ContainerMap (cont :: * -> *) (vec :: * -> *) a b where
  cmap :: (a -> b) -> cont (vec a) -> cont (vec b)
{-  
instance (UV.Unbox a, UV.Unbox b, Container cont (UV.Vector a),Container cont (UV.Vector b)) => ContainerMap cont UV.Vector a b where  
  cmap f x = toContainer $ UV.map f $ fromContainer x
-}

instance (UV.Unbox a, UV.Unbox b) => ContainerMap Signal UV.Vector a b where  
  cmap f x = toContainer $ UV.map f $ fromContainer x

instance (UV.Unbox a, UV.Unbox b) => ContainerMap FSignal UV.Vector a b where  
  cmap f x = toContainer $ UV.map f $ fromContainer x

instance (UV.Unbox a, UV.Unbox b) => ContainerMap Distrib UV.Vector a b where  
  cmap f x = toContainer $ UV.map f $ fromContainer x


class ContainerZip (cont :: * -> *) (vec :: * -> *) a b c where
  czipWith :: (a -> b -> c) -> cont (vec a) -> cont (vec b) -> cont (vec c)
  
instance (UV.Unbox a, UV.Unbox b, UV.Unbox c, ContainerMisc Signal UV.Vector a,  ContainerMisc Signal UV.Vector b, ContainerMisc Signal UV.Vector c) => ContainerZip Signal UV.Vector a b c where  
  czipWith f x y | (clen x)== (clen y) = toContainer $ UV.zipWith f (fromContainer x) (fromContainer y)
  czipWith f x y | otherwise = error "Error in  ContainerZip -- lengthCheck failed" 
{-
instance (UV.Unbox a, UV.Unbox b, UV.Unbox c) => ContainerZip FSignal UV.Vector a b c where  
  czipWith f x y  | clengthCheck x y = toContainer $ UV.zipWith f (fromContainer x) (fromContainer y)
  czipWith f x y | otherwise = error "Error in  ContainerZip -- lengthCheck failed" 

instance (UV.Unbox a, UV.Unbox b, UV.Unbox c) => ContainerZip Distrib UV.Vector a b c where  
  czipWith f x y  | clengthCheck x y = toContainer $ UV.zipWith f (fromContainer x) (fromContainer y)
  czipWith f x y | otherwise = error "Error in  ContainerZip -- lengthCheck failed" 
-}

class ContainerMisc (cont :: * -> *) (vec :: * -> *) a where
  clen :: cont (vec a) -> Int
  
{-
instance (UV.Unbox a, Container cont (UV.Vector a)) => ContainerMisc cont UV.Vector a where
  clen x = UV.length (fromContainer x)
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

instance DataDisplay Sig  where 
  ddisp (Signal v) typ = dispRange (UV.minimum v) (UV.maximum v) typ

instance DataDisplay BSig  where 
  ddisp (Signal v) typ = dispRange (UV.minimum v) (UV.maximum v) typ

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

-- Calculate with Signals
s1 = Signal (UV.fromList l1) :: Sig 
s2 = Signal (UV.fromList l2) :: Sig
s3 = Signal (UV.fromList l3) :: BSig
s4 = Signal (UV.fromList l4) :: ISig
s5 = Signal (GV.fromList l5) :: StateSig


s6 = s1.*s3 -- Multiply with Boolean Signal
s7 = cmap abs s6
sList = [s1,s2,s6]

-- Calculate with FSignals
fs1 = FSignal (UV.fromList l1) :: FSig 
fs2 = FSignal (UV.fromList l2) :: FSig
fs6 = fs1.*fs2
fs7 = fs6./fs2
fs8 = fs7.-fs1
fs13 = fs1.+fs2
fs9 = fs1.*v2
fs10 = v2.*fs1 
fs11 = fs1.+v1
fs12 = v2.+fs1
fsList = [fs1,fs2,fs6,fs7,fs8,fs13,fs9,fs10,fs11,fs12]

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



p = P fs1
t = DT fs2
e = p~*t 

main = do
  putStrLn "Signals"
  putStrLn $ unlines $ map (tdisp . P) sList
  putStrLn $ (tdisp . P) s3  
  putStrLn "FSignals"
  putStrLn $ unlines $ map (tdisp . P) fsList
  putStrLn "Values"  
  putStrLn $ unlines $ map (tdisp . P) vList
  putStrLn "Distribtions"  
  putStrLn $ unlines $ map (tdisp . P) dList

  
  putStrLn $ show (e)
  putStrLn $ tdisp (e)
  

