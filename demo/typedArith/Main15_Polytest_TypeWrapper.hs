{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F


type UVec = UV.Vector
type Val = Double

-----------------------------------------------------------------------------------
-- Flow Signal
  
newtype Signal = Signal (UVec Double) deriving Show
newtype Hist = Hist (UVec Double) deriving Show
newtype Value = Value (UVec Double) deriving Show


--------------------------------------------------------------------------------------------
-- Sample Class

class (Show a) => Data a where
  fromData :: a -> UVec Double 
  toData :: UVec Double -> a
  
instance Data Hist where fromData (Hist x) = x; toData x = Hist x    
instance Data Value where fromData (Value x) = x; toData x = Value x 
instance Data Signal where fromData (Signal x) = x; toData x = Signal x                               

--------------------------------------------------------------------------------------------
-- Sample Calculation Class

class (Data a, Data b, Data c) => DataTypes a b c  | a b -> c where
  (.*.) :: a -> b -> c
  (.*.) x y = toData (fromData x * fromData y) 

instance DataTypes Signal Signal Signal


data P a = P a deriving (Show)
data E a = E a deriving (Show)
data T a = T a deriving (Show)


--------------------------------------------------------------------------------------------
-- Vector Class

class Type a b where
  toType :: a -> b
  fromType :: b -> a

instance Type (P b)  b where
  toType (P x) = x
  fromType x = P x
  
instance Type (E b) b where  
  toType (E x) = x
  fromType x = E x
  

--------------------------------------------------------------------------------------------
-- Generic Product Class with Instances

class (Type x a, Type y b, Type z c) => Prod x a y b z c where
 (~*) :: x -> y ->  z


instance (SigProd a b c) => Prod (Signal a) a (Signal b) b (Signal c) c where (~*) x y = x .* y 
instance (HSigProd a b c) => Prod (Hist a) a (Signal b) b (Signal c) c where (~*) x y = x *~ y


--------------------------------------------------------------------------------------------
-- Implementation Classes

class (DataTypes a b c) => SigProd a b c where
  (.*) :: Signal a -> Signal b -> Signal c

class (DataTypes a b c) => HSigProd a b c where
  (*~) :: Hist a -> Signal b -> Signal c

instance (Types a b c) => SigProd a b c where
  (.*) (DataSignal v1)  (Signal v2) = (Signal (UV.zipWith (.*.) v1 v2)) 

instance (DataTypes a b c) => HSigProd a b c where
  (*~) (Hist v1)  (Signal v2) = (Signal (UV.zipWith (.*.) v1 v2)) 


s1 = Signal (UV.fromList [Signal 0.5 , Signal 0.3]) 
s2 = Signal (UV.fromList [Value 0.1 , Value 0.1])
h1 = Signal (UV.fromList [Signal 0.5 , Signal 0.3])

main = do
  putStrLn $ show (s1.*s2)
  putStrLn $ show (h1.*s2)

