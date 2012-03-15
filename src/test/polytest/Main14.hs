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

-- Daten klasse zum einheitlichen extrahieren der Daten
class Data a where
  fromData :: a -> UVec 
  toData :: UVec -> a
  
instance Data Signal where fromData (Signal x) = x; toData x = Signal x
instance Data Sample where fromData (Sample x) = x; toData x = Sample x
instance Data Hist where fromData (Hist x) = x; toData x = Hist x
  

-- Klasse zur Multiplikation der Daten
class (Data x, Data y, Data z) => DMult x y z | x y -> z where
   (~*) :: x -> y -> z

instance DMult Signal Signal Signal where (~*) x y = toData (UV.zipWith (*) (fromData x)  (fromData y))  


-- Verschiedene Physikalische Typen
data P a = P a deriving Show
data E a = E a deriving Show
data T a = T a deriving Show

class Typ t g where
  fromTyp :: t -> g
  toTyp :: g -> t
  
instance Typ (P g) g where fromTyp (P x) = x; toTyp x = (P x)      
instance Typ (E g) g where fromTyp (E x) = x; toTyp x = (E x)      
instance Typ (T g) g where fromTyp (T x) = x; toTyp x = (T x)      


class (Typ a x, Typ b y, Typ c z, DMult x y z, Show z) => TypMult  a x b y c z  | a b -> c, x y -> z, a -> x, b -> y, c -> z where 
    (.*) :: a -> b -> c 

instance (DMult x y z, Typ a x, Typ b y, Typ c z, Show z) => TypMult a x b y c z  where  
    (.*) x y = toTyp ((fromTyp x) ~* (fromTyp y))


class (TypMult a x b y c z) => TMult a x b y c z


instance TMult (P Signal) Signal (T Signal) Signal (E Signal) Signal


s1 = P (Signal (UV.fromList [0.5 , 0.3])) 
s2 = T (Signal (UV.fromList [0.1 , 0.1]))


main = do
  let s3 = s1.*s2 :: (E Signal)
  putStrLn $ show (s3)


