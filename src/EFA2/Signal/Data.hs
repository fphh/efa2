{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module EFA2.Signal.Data (module EFA2.Signal.Data) where

import qualified EFA2.Signal.Vector as SV
import Data.Monoid (Monoid(mempty, mappend, mconcat))

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

import qualified Data.List as L

import Data.Eq (Eq((==), (/=)))
import Data.Ord (Ord, (<), (>), (<=), (>=))
import Data.Function ((.), ($), id, flip)
import Prelude (Bool, Int, (+))
import qualified Prelude as P


----------------------------------------------------------
-- | EFA data containers

newtype Data ab c = Data {getData :: Apply ab c}

data Nil a
data ((a :: * -> *)  :>  (b :: * -> *)) c

infixr 9 :>

-- | apply nested functor to the functor argument
type family Apply (a :: * -> *) c

type instance Apply Nil a = a
type instance Apply (a :> b) c = a (Apply b c)

{-
Data [['a']] :: Data ([] :> [] :> Nil) Char
-}

{- |
Smart 'Data' constructor
that uses the first argument only for type inference.
-}
subData :: Data (v2 :> v1) a -> Apply v1 a -> Data v1 a
subData _ = Data

{- |
Smart deconstructor
-}
getSubData :: Data (v2 :> v1) a -> Data v1 a -> Apply v1 a
getSubData _ = getData


instance P.Show (Apply ab c) => P.Show (Data ab c) where
   showsPrec n (Data x) =
      P.showParen (n>=10)
         (P.showString "Data " . P.shows x)


---------------------------------------------------------
-- | Type Synonym Convenience

type DVal = Data Nil
type UVec = Data (UV.Vector :> Nil)
type UVec2 = Data (V.Vector :> UV.Vector :> Nil)
type UVec3 = Data (V.Vector :> V.Vector :> UV.Vector :> Nil)

type UVec2L = Data ([] :> UV.Vector :> Nil)

type Vec = Data (V.Vector :> Nil)
type Vec2 = Data (V.Vector :> V.Vector :> Nil)
type Vec3 = Data (V.Vector :> V.Vector :> V.Vector :> Nil)

type List = Data ([] :> Nil)
type List2 = Data ([] :> [] :> Nil)
type List3 = Data ([] :> [] :> [] :> Nil)


----------------------------------------------------------
-- | mapping

class Functor v where
   fmap :: (d1 -> d2) -> Data v d1 -> Data v d2

instance Functor Nil where
   fmap f (Data x) = Data $ f x

instance (P.Functor v2, Functor v1) => Functor (v2 :> v1) where
   fmap f xd@(Data x) =
      Data $ P.fmap (getData . fmap f . subData xd) x

instance (Functor v) => P.Functor (Data v) where
   fmap = fmap


class Map c d1 d2 where
   map :: (d1 -> d2) -> Data c d1 -> Data c d2

instance Map Nil d1 d2 where
   map f (Data x) = Data $ f x

instance
   (SV.Walker v2 (Apply v1 d1) (Apply v1 d2), Map v1 d1 d2) =>
      Map (v2 :> v1) d1 d2 where
   map f xd@(Data x) =
      Data $ SV.map (getData . map f . subData xd) x


----------------------------------------------------------
-- | Zipping for normal Arithmetics

{- |
needed for ZipWith and Append class
-}
type family Zip (c1 :: * -> *) (c2 :: * -> *) :: (* -> *)
type instance Zip Nil Nil = Nil
type instance Zip Nil (v2 :> v1) = v2 :> v1
type instance Zip (v2 :> v1) Nil = v2 :> v1
type instance Zip (v :> v1) (v :> v2) = v :> Zip v1 v2


class ZipWith c1 c2 d1 d2 d3 where
   zipWith :: (d1 -> d2 -> d3) -> Data c1 d1 -> Data c2 d2 -> Data (Zip c1 c2) d3

-- 0d - 0d
instance ZipWith Nil Nil d1 d2 d3 where
   zipWith f (Data x) (Data y) = Data $ f x y

-- 0d - (n+1)d
instance
   (SV.Walker v2 (Apply v1 d2) (Apply v1 d3), Map v1 d2 d3) =>
      ZipWith Nil (v2 :> v1) d1 d2 d3 where
   zipWith f (Data x) y = map (f x) y

-- (n+1)d - 0d
instance
   (SV.Walker v2 (Apply v1 d1) (Apply v1 d3), Map v1 d1 d3) =>
      ZipWith (v2 :> v1) Nil d1 d2 d3 where
   zipWith f x (Data y) = map (flip f y) x

-- (n+1)d - (n+1)d
instance
   (SV.Zipper v2 (Apply v0 d1) (Apply v1 d2) (Apply (Zip v0 v1) d3),
    ZipWith v0 v1 d1 d2 d3) =>
      ZipWith (v2 :> v0) (v2 :> v1) d1 d2 d3 where
   zipWith f xd@(Data x) yd@(Data y) =
      Data $ SV.zipWith (\xc yc -> getData $ zipWith f (subData xd xc) (subData yd yc)) x y


----------------------------------------------------------
-- | Tensor products

class TensorProduct c1 c2 d1 d2 d3 where
   type Stack c1 c2 :: (* -> *)
   tensorProduct :: (d1 -> d2 -> d3) -> Data c1 d1 -> Data c2 d2 -> Data (Stack c1 c2) d3

-- 0d - nd
instance
   (Map v d2 d3) =>
      TensorProduct Nil v d1 d2 d3 where
   type Stack Nil v = v
   tensorProduct f (Data x) y = map (f x) y

-- (m+1)d - nd
instance
   (SV.Walker v1 (Apply v2 d1) (Apply (Stack v2 v) d3),
    TensorProduct v2 v d1 d2 d3) =>
      TensorProduct (v1 :> v2) v d1 d2 d3 where
   type Stack (v1 :> v2) v = v1 :> Stack v2 v
   tensorProduct f xd@(Data x) yd =
      Data $ SV.map (\xc -> getData $ tensorProduct f (subData xd xc) yd) x


----------------------------------------------------------
-- Zipping for cross Arithmetics

-- this class does not scale well to arbitrary nesting depths
class CrossWith c1 c2 d1 d2 d3 where
   type Cross c1 c2 :: (* -> *)
   crossWith ::
      (d1 -> d2 -> d3) ->
      Data c1 d1 -> Data c2 d2 -> Data (Cross c1 c2) d3

instance
   (SV.Walker v1 d1 (v2 d3), SV.Walker v2 d2 d3) =>
      CrossWith (v1 :> Nil) (v2 :> Nil) d1 d2 d3 where
   type Cross (v1 :> Nil) (v2 :> Nil) = v1 :> v2 :> Nil
   crossWith = tensorProduct

instance
   (SV.Zipper v2 d1 (v1 d2) (v1 d3), SV.Walker v1 d2 d3) =>
      CrossWith (v2 :> Nil) (v2 :> v1 :> Nil) d1 d2 d3 where
   type Cross (v2 :> Nil) (v2 :> v1 :> Nil) = v2 :> v1 :> Nil
   crossWith = zipWith

instance
   (SV.Zipper v2 (v1 d1) d2 (v1 d3), SV.Walker v1 d1 d3) =>
      CrossWith (v2 :> v1 :> Nil) (v2 :> Nil) d1 d2 d3 where
   type Cross (v2 :> v1 :> Nil) (v2 :> Nil) = v2 :> v1 :> Nil
   crossWith = zipWith


----------------------------------------------------------
-- fold Functions

class Fold c d1 d2 where
   foldl :: (d1 -> d2 -> d1) -> d1 -> Data c d2 -> d1
   foldr :: (d2 -> d1 -> d1) -> d1 -> Data c d2 -> d1

instance Fold Nil d1 d2 where
   foldl f x (Data y) = f x y
   foldr f x (Data y) = f y x

instance
   (SV.Walker v2 (Apply v1 d2) d1, Fold v1 d1 d2) =>
      Fold (v2 :> v1) d1 d2 where
   foldl f x yd@(Data y) = SV.foldl (\xc yc -> foldl f xc (subData yd yc)) x y
   foldr f x yd@(Data y) = SV.foldr (flip (foldr f) . subData yd) x y


foldl1d ::
   (SV.Walker v2 (Apply v1 d2) (Apply v1 d1)) =>
   (Apply v1 d1 -> Apply v1 d2 -> Apply v1 d1) ->
   Apply v1 d1 ->
   Data (v2 :> v1) d2 ->
   Apply v1 d1
foldl1d f x (Data y) = SV.foldl f x y

foldr1d ::
   (SV.Walker v2 (Apply v1 d1) (Apply v1 d2)) =>
   (Apply v1 d1 -> Apply v1 d2 -> Apply v1 d2) ->
   Apply v1 d2 ->
   Data (v2 :> v1) d1 ->
   Apply v1 d2
foldr1d f x (Data y) = SV.foldr f x y

----------------------------------------------------------
-- Monoid

instance (SV.Singleton v2 (Apply v1 d)) => Monoid (Data (v2 :> v1) d) where
   mempty = Data SV.empty
   mappend (Data x) (Data y) = Data $ SV.append x y
   mconcat = Data . SV.concat . L.map getData


class Append c1 c2 d  where
   append :: Data c1 d -> Data c2 d -> Data (Zip c1 c2) d

instance
   (SV.Singleton v2 (Apply v1 d), v1 ~ Zip v1 v1) =>
      Append (v2 :> v1) (v2 :> v1) d where
   append (Data x) (Data y) = Data $ SV.append x y

instance (SV.Singleton v1 d) => Append (v1 :> Nil) Nil d where
   append (Data x) (Data y) = Data $ SV.append x (SV.singleton y)

instance (SV.Singleton v1 d) => Append Nil (v1 :> Nil) d where
   append (Data x) (Data y) = Data $ SV.append (SV.singleton x) y


----------------------------------------------------------
-- get data Range

class Maximum c d where
   maximum :: Data c d -> d
   minimum :: Data c d -> d

instance Maximum Nil d where
   maximum (Data x) = x
   minimum (Data x) = x

instance
   (SV.Singleton v2 d, SV.Walker v2 (Apply v1 d) d, Maximum v1 d) =>
      Maximum (v2 :> v1) d where
   maximum xd@(Data x) = SV.maximum $ SV.map (maximum . subData xd) x
   minimum xd@(Data x) = SV.minimum $ SV.map (minimum . subData xd) x


----------------------------------------------------------
-- From / To List

class FromList c d where
   type NestedList c d :: *
   fromList :: NestedList c d -> Data c d
   toList :: Data c d -> NestedList c d

instance FromList Nil d where
   type NestedList Nil d = d
   fromList x = Data x
   toList (Data x) = x

instance
   (SV.FromList v2 (Apply v1 d), FromList v1 d) =>
      FromList (v2 :> v1) d where
   type NestedList (v2 :> v1) d = [NestedList v1 d]
   fromList x =
      let y = Data $ SV.fromList $ L.map (getSubData y . fromList) x
      in  y
   toList xd@(Data x) = L.map (toList . subData xd) $ SV.toList x


----------------------------------------------------------
-- All

class All c d where
   all :: (d -> Bool) -> Data c d -> Bool
   any :: (d -> Bool) -> Data c d -> Bool

instance All Nil d where
   all f (Data x) = f x
   any f (Data x) = f x

instance (SV.Singleton v2 (Apply v1 d), All v1 d) => All (v2 :> v1) d where
   all f xd@(Data x) = SV.all (all f . subData xd) x
   any f xd@(Data x) = SV.any (any f . subData xd) x

----------------------------------------------------------
-- Transpose

transpose1 :: Data (v1 :> Nil) d -> Data (v1 :> Nil) d
transpose1 x = x

transpose2 ::
   (SV.Transpose v1 v2 d) =>
   Data (v2 :> v1 :> Nil) d -> Data (v2 :> v1 :> Nil) d
transpose2 (Data x) = Data $ SV.transpose x


----------------------------------------------------------
-- Head & Tail

head, last ::
   (SV.Singleton v2 (Apply v1 d)) => Data (v2 :> v1) d -> Data v1 d
head (Data x) = Data $ SV.head x
last (Data x) = Data $ SV.last x

tail, init ::
   (SV.Singleton v2 (Apply v1 d)) => Data (v2 :> v1) d -> Data (v2 :> v1) d
tail (Data x) = Data $ SV.tail x
init (Data x) = Data $ SV.init x


----------------------------------------------------------
-- Singleton

singleton :: (SV.Singleton v2 (Apply v1 d)) => Data v1 d -> Data (v2 :> v1) d
singleton (Data x) = Data $ SV.singleton x


----------------------------------------------------------
-- Sort

sort :: (SV.Sort v1 d) => Data (v1 :> Nil) d -> Data (v1 :> Nil) d
sort (Data x) = Data $ SV.sort x


----------------------------------------------------------
-- Filter

class Filter c d where
   filter :: (d -> Bool) -> Data c d -> Data c d

instance Filter1 v2 v1 d => Filter (v2 :> v1) d where
   filter = filter1


class Filter1 c2 c1 d where
   filter1 :: (d -> Bool) -> Data (c2 :> c1) d -> Data (c2 :> c1) d

instance SV.Filter v d => Filter1 v Nil d where
   filter1 f (Data x) = Data $ SV.filter f x

instance
   (SV.Walker v3 (v2 (Apply v1 d)) (v2 (Apply v1 d)), Filter1 v2 v1 d) =>
      Filter1 v3 (v2 :> v1) d where
   filter1 f xd@(Data x) = Data $ SV.map (getData . filter f . subData xd) x


----------------------------------------------------------
-- Eq

instance Eq (Apply v d) => Eq (Data v d) where
   (==) (Data x) (Data y)  =  x == y
   (/=) (Data x) (Data y)  =  x /= y

instance Ord d => Ord (Data Nil d) where
   (>)  (Data x) (Data y)  =  x > y
   (<)  (Data x) (Data y)  =  x < y
   (>=) (Data x) (Data y)  =  x >= y
   (<=) (Data x) (Data y)  =  x <= y


----------------------------------------------------------
-- Convert

class Convert c1 c2 d where
   convert :: Data c1 d -> Data c2 d

instance Convert Nil Nil d where
   convert = id

instance
   (SV.Convert v1 v2 (Apply v2r d),
    SV.Walker v1 (Apply v1r d) (Apply v2r d),
    Convert v1r v2r d) =>
      Convert (v1 :> v1r) (v2 :> v2r) d where
   convert xd@(Data x) =
      let yd = Data $ SV.convert $ SV.map (getSubData yd . convert . subData xd) x
      in  yd


----------------------------------------------------------
-- Length


length :: SV.Length (Apply c d) => Data c d -> Int
length (Data x) = SV.len x


class Size c d where
   size :: Data c d -> Int

instance Size Nil d where
   size (Data _) = 1

instance
   (SV.Walker v2 Int Int, SV.Walker v2 (Apply v1 d) Int, Size v1 d) =>
      Size (v2 :> v1) d where
   size xd@(Data x) = SV.foldl (+) 0 $ SV.map (size . subData xd) x


----------------------------------------------------------
-- Reverse

class Reverse c d where
   reverse :: Data c d -> Data c d

instance SV.Reverse v2 (Apply v1 d) => Reverse (v2 :> v1) d where
   reverse (Data x) = Data $ SV.reverse x
