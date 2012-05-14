{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs #-}

module EFA2.Signal.Data (module EFA2.Signal.Data) where
import EFA2.Signal.Vector
import EFA2.Signal.Base

----------------------------------------------------------
-- | EFA data containers

newtype Data a b = Data (a b) deriving (Show, Eq, Ord)
--newtype Signal a b = Signal (a b) deriving (Show, Eq, Ord)

data ((a :: * -> *) :> (b :: * -> *)) :: * -> * where
     D0 :: v0 -> Nil v0 
     D1 :: v1 v0 -> (v1 :> Nil) v0
     D2 :: v2 (v1 v0) -> (v2 :> v1 :> Nil) v0
     D3 :: v3 (v2 (v1 v0)) -> (v3 :> v2 :> v1 :> Nil) v0

infixr 9 :>
data Nil' c = Nil' deriving (Show)
type Nil = Nil' :> Nil'

instance (Show v0) => Show (Nil v0) where
         show (D0 x) = show x

instance (Show (v1 v0)) => Show ((v1 :> Nil) v0) where
         show (D1 x) = show x

instance (Show (v2 (v1 v0))) => Show ((v2 :> v1 :> Nil) v0) where
         show (D2 x) = show x

instance (Show (v3 (v2 (v1 v0)))) => Show ((v3 :> v2 :> v1 :> Nil) v0) where
         show (D3 x) = show x

----------------------------------------------------------
-- Zipping for normal Arithmetics 

class DZipWith c1 c2 d1 d2 d3  where
  dzipWith :: (d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c2 d3

-- 0d - 0d
instance DZipWith (Data Nil) (Data Nil) d1 d2 d3 where
         dzipWith f (Data (D0 x)) (Data (D0 y)) = Data $ D0 $ f x y 

-- 0d - 1d
instance (VFunctor d2 d3 v1) => DZipWith (Data Nil) (Data (v1 :> Nil))  d1 d2 d3  where
         dzipWith f  (Data (D0 x)) (Data (D1 y)) = Data $ D1 $ vmap (f x) y 

-- 0d - 2d
instance (VFunctor d2 d3 v1,VFunctor (v1 d2) (v1 d3) v2) => DZipWith (Data Nil) (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         dzipWith f  (Data (D0 x)) (Data (D2 y)) = Data $ D2 $ vmap (vmap (f x)) y 

-- 1d - 1d
instance (VZipper d1 d2 d3 v1) => DZipWith (Data (v1 :> Nil)) (Data (v1 :> Nil))  d1 d2 d3  where
         dzipWith f  (Data (D1 x)) (Data (D1 y)) = Data $ D1 $ vzipWith f x y 

-- 1d - 2d
instance (VZipper d1 d2 d3 v1, VFunctor (v1 d2) (v1 d3) v2) => DZipWith (Data (v1 :> Nil)) (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         dzipWith f  (Data (D1 x)) (Data (D2 y)) = Data $ D2 $ vmap (vzipWith f x) y 

-- 2d - 2d
instance (VZipper d1 d2 d3 v1, VZipper (v1 d1) (v1 d2) (v1 d3) v2) => DZipWith (Data (v2 :> v1 :> Nil)) (Data (v2 :> v1 :> Nil))  d1 d2 d3  where
         dzipWith f  (Data (D2 x)) (Data (D2 y)) = Data $ D2 $ vzipWith (vzipWith f) x y 


----------------------------------------------------------
-- Zipping for cross Arithmetics 
{-
class CrossWith c1 c2 c3 d1 d2 d3  where
  crossWith :: (d1 -> d2 -> d3) -> c1 d1 -> c2 d2 -> c3 d3

-- 1d - 1d
instance CrossWith (Data (v1 :> Nil)) (Data (v1 :> Nil))  (Data (v1 :> v1 :> Nil))  d1 d2 d3  where
         crossWith f  (Data (D1 x)) (Data (D1 y)) = Data $ D2 $ (vmap g) y
           where g y' = vmap (f y') x  
-}

