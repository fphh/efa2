{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F


type UVec = UV.Vector Val
type Val = Double

-- Verschiedene Datenformate mit verschiedenen Inhalten
newtype Signal = Signal UVec deriving Show
newtype Sample = Sample UVec  deriving Show
newtype Hist = Hist UVec deriving Show

class Data a where
  fromData :: a -> UVec 
  toData :: UVec -> a
  
instance Data Signal where fromData (Signal x) = x; toData x = Signal x
instance Data Sample where fromData (Sample x) = x; toData x = Sample x
instance Data Hist where fromData (Hist x) = x; toData x = Hist x
  
class DataTyping x y z | x y -> z 

instance DataTyping Signal Signal Signal
  
class (Data x, Data y, Data z, DataTyping x y z) => DMult x y z where
   (~*) :: x -> y -> z
 
instance (Data x, Data y, Data z, DataTyping x y z ) => DMult x y z where
   (~*) x y = toData (UV.zipWith (*) (fromData x)  (fromData y))



-- Multiplication der verschiedenen Datenformate
-- class (DMultCalc x y z) => DMult x y z | x y -> z
-- instance (DMultCalc x y z) => DMult x y z 

-- Verschiedene Physikalische Typen
data P a = P a deriving Show
data E a = E a deriving Show
data T a = T a deriving Show

class Typ t d where
  fromTyp :: t -> d
  toTyp :: d -> t
    
instance Typ (P d) d where fromTyp (P x) = x; toTyp x = (P x)      
instance (Show d) => Typ (E d) d where fromTyp (E x) = x; toTyp x = (E x)      
instance Typ (T d) d where fromTyp (T x) = x; toTyp x = (T x)      


class (Typ a x, Typ b y, Typ c z, DMult x y z) => Typing a x b y c z | a b -> c

instance (Typ a x, Typ b y, Typ c z, DMult x y z, Show z) => Typing (P x) x (T y) y (E z) z


class  (Typing a x b y c z) => TMult a x b y c z where
  (.*) :: a -> b -> c
  (.*) x y = toTyp ((fromTyp x) ~* (fromTyp y))

instance  (Typing a x b y c z) => TMult a x b y c z 

s1 = P (Signal (UV.fromList [0.5 , 0.3])) 
s2 = T (Signal (UV.fromList [0.1 , 0.1]))


main = do
  let s3 = s1.*s2
  putStrLn $ show (s3)


