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

-- =================== Data   
   
{-  
class DataInfo a b where 
  dlength:: a -> b 
  
  
instance DataInfo Sig where  
  dlength (Signal v) = UV.length v  
  dmaximum (Signal v)= UV.maxiumum v 
  dminimum (Signal v)= UV.maxiumum v 
-}  
    
{-  
-- Sub Class  
class   (Type t1 a, Type t2 b, Type t3 c, DSum a b c)  => TSub t1 a t2 b t3 c | t1 t2 -> t3  where 
 (~-) ::  t1 a -> t2 b -> t3 c
 (~-) x y = toType ((fromType x) .- (fromType y)) 

-- Delta Generation
instance  DSum a b c => TSub P a P  b DP c where 
instance  DSum a b c => TSub X a X  b DX c where 
instance  DSum a b c => TSub E a E  b DE c where 
instance  DSum a b c => TSub N a N  b DN c where 
instance  DSum a b c => TSub T a T  b DT c where 
-} 

-- ############################ Display #############################
  
class (Show a, PrintfArg a,Fractional a) => TShow a

data DispLength = Short | Middle | Long | Float 



class Disp a where 
  disp :: a -> String -> Val -> String

instance Disp Val where
  disp 0 _ _ = "Null"
  disp x format scale = printf format $ x*scale

instance Disp Bool where
  disp x format scale = printf format (show x)

instance Disp Int where
  disp x format scale = printf format (show x)

-- | choose a Form of Display
dispLength = Short

-- | Map to choose display Unit per type
dispUnitMap = M.fromList [("P","kW"),
                      ("E","kWh"),
                      ("X","%"),
                      ("M","%"),
                      ("N","/"), 
                      ("T","s")]; 

-- | Map to lookup display scale per Unit 
unitMap = M.fromList [("kW",1/1000),
                      ("kWh",1/1000/3600),
                      ("%",100),
                      ("s",1),
                      ("/",1)];

-- | Map to choose displayFormat for a Kombination of Type and Unit -- three Options -- short, middle, long, float                 
displayFormatMap = M.fromList [(("P","kW"),("%6.7f","%6.7f","%6.7f","%6.7f")),
                               (("E","kWh"),("%6.7f","%6.7f","%6.7f","%6.7f")),
                               (("T","s"),("%6.7f","%6.7f","%6.7f","%6.7f")),
                               (("X","%"),("%6.7f","%6.7f","%6.7f","%6.7f")),
                               (("N","%"),("%6.7f","%6.7f","%6.7f","%6.7f")),
                               (("M","/"),("%6.7f","%6.7f","%6.7f","%6.7f"))]

formatSelect :: DispLength -> (String,String,String,String) -> String
formatSelect Short (f1,f2,f3,f4) = f1
formatSelect Middle (f1,f2,f3,f4) = f2
formatSelect Long (f1,f2,f3,f4) = f3
formatSelect Float (f1,f2,f3,f4) = f4

  
dispSingle :: Disp a => a -> String -> String
dispSingle x typ = disp x format scale ++ " " ++ dunit
           where dunit = dispUnitMap M.! typ
                 scale = unitMap M.! dunit
                 format = formatSelect dispLength $ displayFormatMap M.! (typ,dunit)

dispRange :: Disp a => a -> a -> String -> String
dispRange x y typ = disp x format scale ++ "-" ++ disp y format scale ++ " " ++ dunit
           where dunit = dispUnitMap M.! typ
                 scale = unitMap M.! dunit
                 format = formatSelect dispLength $ displayFormatMap M.! (typ,dunit)
  
                               
class DataDisplay a where
  ddisp :: a -> (String) -> String
          
instance DataDisplay Sig  where 
  ddisp (Signal v) (typString) = dispRange (UV.minimum v) (UV.maximum v) typString


class TypedDisplay (typ :: * -> *) a where
  tdisp :: typ a -> String

instance (DataDisplay a) => TypedDisplay P a where
  tdisp (P d) = ddisp d "P" 
  
instance (DataDisplay a) => TypedDisplay E a where
  tdisp (E d) = ddisp d "E" 

  
  
  
  
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

