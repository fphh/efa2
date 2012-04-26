{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F

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
  
-- -- VERSION I -- hart getypt vermeide Dopped Parametrierung von class Data
-- newtype SignaluD  = Signal $ UVec Val
-- newtype SignaluD  = Signal $ UVec Val -- problem -- viele Instanzen

-- -- VERSION II -- parametriere suf sample datentyp
-- newtype USignal a = Signal UVec a
-- newtype VSignal a = Signal GVec a
-- newtype Dist a = Dist UVec a
-- newtype Value a = Value a

-- VERSION III -- parametriere auf Inhalt
newtype Signal a = Signal a deriving (Show, Eq)
newtype Dist a = Dist a deriving (Show, Eq)
newtype Value a = Value a  deriving (Show, Eq)

--------------------------------------------------------------------------------------------
-- Data Format

class Data a b where
   fromData :: a -> b
   toData :: b -> a
  
instance Data (Signal b) b where fromData (Signal x) = x; toData x = Signal x                               
instance Data (Dist b) b where fromData (Dist x) = x; toData x = Dist x                               
                                  

-- Sample Calculation Class
class DProd a b c | a b -> c where
  (.*) :: a -> b -> c
  
instance DProd (Signal (UVec Val)) (Signal (UVec Val)) (Signal (UVec Val)) where 
  (.*) (Signal v1) (Signal v2) = Signal $ UV.zipWith (*) v1 v2

instance DProd (Dist (UVec Val)) (Dist (UVec Val)) (Dist (UVec Val)) where 
  (.*) (Dist v1) (Dist v2) = Dist $ UV.zipWith (*) v1 v2

instance DProd (Signal (UVec Val)) (Value Val) (Signal (UVec Val)) where 
  (.*) (Signal v1) (Value v2) = Signal $ UV.map (*v2) v1

--------------------------------------------------------------------------------------------
-- Datatypes

data P a = P a deriving (Show)
data E a = E a deriving (Show)
data T a = T a deriving (Show)


-- Type Class

class Type (typ :: * -> *) a where
  toType :: x -> typ x
  fromType :: typ x -> x

instance Type P a where
  toType x = (P x)
  fromType (P x) = x
  
instance Type E a where  
  fromType (E x) = x
  toType x = E x
  
instance Type T a where  
  fromType (T x) = x
  toType x = T x


class  TProd t1 a t2 b t3 c  | t1 a t2 b -> t3 c where 
 (~*) :: (Type t1 a, Type t2 b, Type t3 c, DProd a b c)  => t1 a -> t2 b -> t3 c
 (~*) x y = toType ((fromType x) .* (fromType y)) 

instance  TProd P (Signal (UVec Val)) T (Signal (UVec Val))  E (Signal (UVec Val)) where 



s1 = Signal (UV.fromList [0.5,0.3::Val]) 
s2 = Signal (UV.fromList [0.8,0.2::Val])
v = Value 2 :: Value Val
-- d = Signal (UV.fromList [0.5,0.3])


s =  (s1.*s2) :: Signal (UV.Vector Val)
s'=s.*v

main = do
  putStrLn $ show (s)
  putStrLn $ show (s')
  
  
--  putStrLn $ show (d.*s2)

