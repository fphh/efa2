{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F



type UVec = UV.Vector Val
type Val = Double

diffVect :: UVec -> UVec
diffVect v = (UV.tail v) - (UV.init v)

dzipWith :: (Val -> Val -> Val ) -> UVec -> UVec
dzipWith f v = UV.zipWith f (UV.tail v) (UV.init v)

instance Num UVec where
  (+) v1 v2 = UV.zipWith (+) v1 v2
  (-) v1 v2 = UV.zipWith (-) v1 v2
  (*) v1 v2 = UV.zipWith (*) v1 v2
  abs v1  = UV.map abs v1
  signum v1  = UV.map signum v1
--  fromInteger v1 = UV.map fromInteger v1

instance Fractional UVec where
  (/) v1 v2 = UV.zipWith (/) v1 v2



-----------------------------------------------------------------------------------
-- Flow Signal
  
data P = PH (UVec) | PSig (UVec) | PVal (Val) 
data E = EH (UVec) | ESig (UVec) | EVal (Val)
data T = TH (UVec) | TSig (UVec) | TVal (Val)

class TNum a b c  | a b -> c where
  (.*) :: a -> b -> c

instance TNum P T E where
  (.*) (PH v1) (TVal v2) = EH (v1*v2)   -- Energy = Power * DTime
  (.*) (PSig v1) (TSig v2) = ESig (v1*v2)   -- Energy = Power * DTime
  

s1 = PSig (UV.fromList [(0.5) , (0.3)]) 
s2 = TSig (UV.fromList [(0.1) , (0.1)])

main = do
--  putStrLn $ show (p1.*n1)
  
  putStrLn $ show (s1*s2)

