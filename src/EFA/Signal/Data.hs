{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeFamilies #-}

module EFA.Signal.Data (module EFA.Signal.Data) where

import qualified EFA.Signal.Vector as SV
import Data.Monoid (Monoid(mempty, mappend, mconcat))

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/))

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

import qualified Data.List as L
import Data.Tuple.HT (mapPair)

import Data.Eq (Eq((==), (/=)))
import Data.Ord (Ord, (<), (>), (<=), (>=))
import Data.Function ((.), ($), id, flip)
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Control.Monad (mplus)
import Prelude (Bool, Int, (+), error)
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


type family Value dat :: *
type instance Value (Data ab c) = c


instance P.Show (Apply ab c) => P.Show (Data ab c) where
   showsPrec n (Data x) =
      P.showParen (n>=10)
         (P.showString "Data " . P.shows x)


---------------------------------------------------------
-- | Type Synonym Convenience

type Scalar = Data Nil

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
-- | handling of storage specific constraints on the element type

class Storage (c :: * -> *) a where
   data Constraints c a :: *
   constraints :: Data c a -> Constraints c a

instance Storage Nil a where
   data Constraints Nil a = NilConstraints
   constraints _ = NilConstraints

instance (SV.Storage v2 (Apply v1 a), Storage v1 a) => Storage (v2 :> v1) a where
   data Constraints (v2 :> v1) a =
           (SV.Storage v2 (Apply v1 a), Storage v1 a) => ComposeConstraints
   constraints _ = ComposeConstraints


readNested ::
   ((SV.Storage v2 (Apply v1 a), Storage v1 a) => Data (v2 :> v1) a -> b) ->
   (Storage (v2 :> v1) a => Data (v2 :> v1) a -> b)
readNested f x = case constraints x of ComposeConstraints -> f x

writeNested ::
   ((SV.Storage v2 (Apply v1 a), Storage v1 a) => Data (v2 :> v1) a) ->
   (Storage (v2 :> v1) a => Data (v2 :> v1) a)
writeNested x =
   let z = case constraints z of ComposeConstraints -> x
   in  z


infixl 0 `withNestedData`

withNestedData ::
   ((SV.Storage v2 (Apply v1 a), Storage v1 a) => v2 (Apply v1 a) -> b) ->
   (Storage (v2 :> v1) a => Data (v2 :> v1) a -> b)
withNestedData f = readNested (f . getData)

nestedData ::
   ((SV.Storage v2 (Apply v1 a), Storage v1 a) => v2 (Apply v1 a)) ->
   (Storage (v2 :> v1) a => Data (v2 :> v1) a)
nestedData x = writeNested (Data x)


{-
Allow 'Data' to be also instance of classes from "EFA.Signal.Vector".
-}
instance Storage c a => SV.Storage (Data c) a where
   data Constraints (Data c) a = Storage c a => DataConstraints
   constraints _ = DataConstraints

infixl 0 `readData`

readData ::
   (Storage c a => Data c a -> b) ->
   (SV.Storage (Data c) a => Data c a -> b)
readData f x = case SV.constraints x of DataConstraints -> f x

writeData ::
   (Storage c a => Data c a) ->
   (SV.Storage (Data c) a => Data c a)
writeData x =
   let z = case SV.constraints z of DataConstraints -> x
   in  z



class Format c where
   format ::
      (Format.Format output, FormatValue a, Storage c a) =>
      Data c a -> output

instance Format Nil where
   format (Data x) = formatValue x

instance (SV.FromList v2, Format v1) => Format (v2 :> v1) where
   format xd =
      withNestedData (Format.list . P.map (format . subData xd) . SV.toList) xd


instance (Format c, FormatValue a, Storage c a) => FormatValue (Data c a) where
   formatValue = format


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


class Map c where
   map :: (Storage c d1, Storage c d2) => (d1 -> d2) -> Data c d1 -> Data c d2

instance Map Nil where
   map f (Data x) = Data $ f x

instance (SV.Walker v2, Map v1) => Map (v2 :> v1) where
   map f xd =
      nestedData
         (withNestedData (SV.map (getData . map f . subData xd)) xd)


instance (Fold c, Equal c, Map c) => SV.Walker (Data c) where
   map f x = writeData (map f `readData` x)
   foldr f b as = foldr f b `readData` as
   foldl f b as = foldl f b `readData` as
   equalBy f as bs = equalBy f `readData` as `readData` bs


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


{- |
It should hold:

> map f xs = zipWith (const f) xs xs
> map f xs = zipWith (const . f) xs xs
-}
class (Map c) => ZipWith c where
   zipWith ::
      (Storage c d1, Storage c d2, Storage c d3) =>
      (d1 -> d2 -> d3) -> Data c d1 -> Data c d2 -> Data c d3

-- 0d - 0d
instance ZipWith Nil where
   zipWith f (Data x) (Data y) = Data $ f x y

-- (n+1)d - (n+1)d
instance
   (SV.Walker v2, SV.Zipper v2, ZipWith v1) =>
      ZipWith (v2 :> v1) where
   zipWith f xd yd =
      nestedData (
         SV.zipWith (\xc yc -> getData $ zipWith f (subData xd xc) (subData yd yc))
            `withNestedData` xd
            `withNestedData` yd)


instance ZipWith c => SV.Zipper (Data c) where
   zipWith f x y =
      writeData (zipWith f `readData` x `readData` y)

zip ::
   (ZipWith c, Storage c d1, Storage c d2, Storage c (d1, d2)) =>
   Data c d1 -> Data c d2 -> Data c (d1,d2)
zip = zipWith (,)

unzip ::
   (Map c, Storage c a, Storage c b, Storage c (a, b)) =>
   Data c (a, b) -> (Data c a, Data c b)
unzip x = (map P.fst x, map P.snd x)


instance (ZipWith c, Storage c a, Sum a) => Sum (Data c a) where
   (~+) = zipWith (~+)
   (~-) = zipWith (~-)
   negate = map Arith.negate

instance (ZipWith c, Storage c a, Product a) => Product (Data c a) where
   (~*) = zipWith (~*)
   (~/) = zipWith (~/)
   recip = map Arith.recip


{- |
When the structure of @xs@ and @ys@ matches,
then it should hold:

> zipWith f xs ys = zipWithFill f xs ys
-}
class (Map c1, Map c2) => ZipWithFill c1 c2 where
   zipWithFill ::
      (Storage c1 d1, Storage c2 d2, Storage (Zip c1 c2) d3) =>
      (d1 -> d2 -> d3) -> Data c1 d1 -> Data c2 d2 -> Data (Zip c1 c2) d3

-- 0d - 0d
instance ZipWithFill Nil Nil where
   zipWithFill f (Data x) (Data y) = Data $ f x y

-- 0d - (n+1)d
instance (SV.Walker v2, Map v1) => ZipWithFill Nil (v2 :> v1) where
   zipWithFill f (Data x) y = map (f x) y

-- (n+1)d - 0d
instance (SV.Walker v2, Map v1) => ZipWithFill (v2 :> v1) Nil where
   zipWithFill f x (Data y) = map (flip f y) x

-- (n+1)d - (n+1)d
instance
   (SV.Walker v2, SV.Zipper v2, ZipWithFill v0 v1) =>
      ZipWithFill (v2 :> v0) (v2 :> v1) where
   zipWithFill f xd yd =
      nestedData (
         SV.zipWith
            (\xc yc ->
               getData $ zipWithFill f (subData xd xc) (subData yd yc))
            `withNestedData` xd
            `withNestedData` yd)


----------------------------------------------------------
-- | Tensor products

class TensorProduct c1 c2 where
   type Stack c1 c2 :: (* -> *)
   tensorProduct ::
      (Storage c1 d1, Storage c2 d2, Storage (Stack c1 c2) d3) =>
      (d1 -> d2 -> d3) -> Data c1 d1 -> Data c2 d2 -> Data (Stack c1 c2) d3

-- 0d - nd
instance (Map v) => TensorProduct Nil v where
   type Stack Nil v = v
   tensorProduct f (Data x) y = map (f x) y

-- (m+1)d - nd
instance (SV.Walker v1, TensorProduct v2 v) => TensorProduct (v1 :> v2) v where
   type Stack (v1 :> v2) v = v1 :> Stack v2 v
   tensorProduct f xd yd =
      nestedData
         (SV.map (\xc -> getData $ tensorProduct f (subData xd xc) yd)
             `withNestedData` xd)


----------------------------------------------------------
-- Zipping for cross Arithmetics

-- this class does not scale well to arbitrary nesting depths
class CrossWith c1 c2 where
   type Cross c1 c2 :: (* -> *)
   crossWith ::
      (Storage c1 d1, Storage c2 d2, Storage (Cross c1 c2) d3) =>
      (d1 -> d2 -> d3) ->
      Data c1 d1 -> Data c2 d2 -> Data (Cross c1 c2) d3

instance
   (SV.Walker v1, SV.Walker v2) =>
      CrossWith (v1 :> Nil) (v2 :> Nil) where
   type Cross (v1 :> Nil) (v2 :> Nil) = v1 :> v2 :> Nil
   crossWith = tensorProduct

instance
   (SV.Zipper v2, SV.Walker v1, SV.Walker v2) =>
      CrossWith (v2 :> Nil) (v2 :> v1 :> Nil) where
   type Cross (v2 :> Nil) (v2 :> v1 :> Nil) = v2 :> v1 :> Nil
   crossWith = zipWithFill

instance
   (SV.Zipper v2, SV.Walker v1, SV.Walker v2) =>
      CrossWith (v2 :> v1 :> Nil) (v2 :> Nil) where
   type Cross (v2 :> v1 :> Nil) (v2 :> Nil) = v2 :> v1 :> Nil
   crossWith = zipWithFill


----------------------------------------------------------
-- fold Functions

class Fold c where
   foldl :: Storage c d2 => (d1 -> d2 -> d1) -> d1 -> Data c d2 -> d1
   foldr :: Storage c d2 => (d2 -> d1 -> d1) -> d1 -> Data c d2 -> d1

instance Fold Nil where
   foldl f x (Data y) = f x y
   foldr f x (Data y) = f y x

instance (SV.Walker v2, Fold v1) => Fold (v2 :> v1) where
   foldl f x yd = withNestedData (vecFoldlMap (foldl f) x (subData yd)) yd
   foldr f x yd = withNestedData (SV.foldr (flip (foldr f) . subData yd) x) yd


foldl1d ::
   (SV.Walker v2, SV.Storage v2 (Apply v1 d1), SV.Storage v2 (Apply v1 d2)) =>
   (Apply v1 d1 -> Apply v1 d2 -> Apply v1 d1) ->
   Apply v1 d1 ->
   Data (v2 :> v1) d2 ->
   Apply v1 d1
foldl1d f x (Data y) = SV.foldl f x y

foldr1d ::
   (SV.Walker v2, SV.Storage v2 (Apply v1 d1), SV.Storage v2 (Apply v1 d2)) =>
   (Apply v1 d1 -> Apply v1 d2 -> Apply v1 d2) ->
   Apply v1 d2 ->
   Data (v2 :> v1) d1 ->
   Apply v1 d2
foldr1d f x (Data y) = SV.foldr f x y


{- |
vecFoldlMap f x g = foldl f x . map g
but this function requires no storage constraint for the result of 'map'.
-}
vecFoldlMap ::
   (SV.Walker vec, SV.Storage vec a) =>
   (c -> b -> c) -> c -> (a -> b) -> vec a -> c
vecFoldlMap f x0 g = SV.foldl (\acc x -> f acc (g x)) x0

foldlMap ::
   (Fold vec, Storage vec a) =>
   (c -> b -> c) -> c -> (a -> b) -> Data vec a -> c
foldlMap f x0 g = foldl (\acc x -> f acc (g x)) x0


class Integrate v c where
   type Integrated v c :: * -> *
   integrate ::
      (SV.Storage v (Apply c a), Storage c a,
       Storage (Integrated v c) a, Arith.Constant a) =>
      Data (v :> c) a -> Data (Integrated v c) a

instance (SV.Walker v) => Integrate v Nil where
   type Integrated v Nil = Nil
   integrate = Data . SV.foldl (~+) Arith.zero . getData

instance
   (SV.Walker v2, Integrate v1 c) =>
      Integrate v2 (v1 :> c) where
   type Integrated v2 (v1 :> c) = v2 :> Integrated v1 c
   integrate xd =
      nestedData
         (withNestedData
            (SV.map (getData . readNested integrate . subData xd)) xd)


instance
   (SV.Storage v (Apply c a), Storage c a,
    Storage (Integrated v c) a, Integrate v c, Arith.Constant a) =>
      Arith.Integrate (Data (v :> c) a) where
   type Scalar (Data (v :> c) a) = Data (Integrated v c) a
   integrate = integrate

----------------------------------------------------------
-- Monoid

instance
   (SV.Singleton v2, SV.Storage v2 (Apply v1 d)) =>
      Monoid (Data (v2 :> v1) d) where
   mempty = Data SV.empty
   mappend (Data x) (Data y) = Data $ SV.append x y
   mconcat = Data . SV.concat . L.map getData


class Append c1 c2  where
   append ::
      (Storage c1 d, Storage c2 d) =>
      Data c1 d -> Data c2 d -> Data (Zip c1 c2) d

instance
   (SV.Singleton v2, v1 ~ Zip v1 v1) =>
      Append (v2 :> v1) (v2 :> v1) where
   append xd yd =
      Data (SV.append `withNestedData` xd `withNestedData` yd)

instance (SV.Singleton v1) => Append (v1 :> Nil) Nil where
   append xd (Data y) =
      nestedData $
      withNestedData SV.append xd (SV.singleton y)

instance (SV.Singleton v1) => Append Nil (v1 :> Nil) where
   append (Data x) yd =
      nestedData $
      withNestedData (SV.append (SV.singleton x)) yd


----------------------------------------------------------
-- get data Range

_maximum, _minimum :: (Storage c d, Fold c, Ord d) => Data c d -> d
_maximum =
   fromMaybe (error "Data.maximum: empty data") .
   foldlMap (liftOrd P.max) Nothing Just

_minimum =
   fromMaybe (error "Data.minimum: empty data") .
   foldlMap (liftOrd P.min) Nothing Just


class Maximum c where
   maximum, minimum :: (Storage c d, Ord d) => Data c d -> d

instance Maximum Nil where
   maximum (Data x) = x
   minimum (Data x) = x

instance (Maximum1 v2 v1) => Maximum (v2 :> v1) where
   maximum = fromMaybe (error "Data.maximum: empty vector") . maximum1
   minimum = fromMaybe (error "Data.minimum: empty vector") . minimum1


class Maximum1 v2 v1 where
   maximum1, minimum1 ::
      {-
      We do not resolve the Storage constraint here
      and thus omit the readNested call when calling maximum1 and minimum1.
      -}
      (Storage (v2 :> v1) d, Ord d) =>
      Data (v2 :> v1) d -> Maybe d

instance SV.Singleton v => Maximum1 v Nil where
   maximum1 = Just . withNestedData SV.maximum
   minimum1 = Just . withNestedData SV.minimum

instance (SV.Walker v3, Maximum1 v2 v1) => Maximum1 v3 (v2 :> v1) where
   maximum1 xd = withNestedData (vecFoldlMap (liftOrd P.max) Nothing (maximum1 . subData xd)) xd
   minimum1 xd = withNestedData (vecFoldlMap (liftOrd P.min) Nothing (minimum1 . subData xd)) xd


liftOrd :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
liftOrd f (Just x) (Just y) = Just (f x y)
liftOrd _ mx my = mplus mx my

{-
class Maximum c where
   maximum, minimum :: (Storage c d, Ord d) => Data c d -> d

instance Maximum Nil where
   maximum (Data x) = x
   minimum (Data x) = x

instance (Maximum1 v2 v1) => Maximum (v2 :> v1) where
   maximum = readNested (fromMaybe (error "Data.maximum: empty vector") . maximum1)
   minimum = readNested (fromMaybe (error "Data.minimum: empty vector") . minimum1)


class Maximum1 v2 v1 where
   maximum1, minimum1 ::
      {-
      here we use the resolved (Storage (v2 :> v1) d) constraint,
      thus we must use readNested when calling maximum1 or minimum1.
      -}
      (SV.Storage v2 (Apply v1 d), Storage v1 d, Ord d) =>
      Data (v2 :> v1) d -> Maybe d

instance SV.Singleton v => Maximum1 v Nil where
   maximum1 = Just . withNestedData SV.maximum
   minimum1 = Just . withNestedData SV.minimum

instance (SV.Walker v3, Maximum1 v2 v1) => Maximum1 v3 (v2 :> v1) where
   maximum1 xd = withNestedData (SV.foldl (\acc y -> liftOrd P.max acc (readNested maximum1 $ subData xd y)) Nothing) xd
   minimum1 xd = withNestedData (SV.foldl (\acc y -> liftOrd P.min acc (readNested minimum1 $ subData xd y)) Nothing) xd
-}

----------------------------------------------------------
-- From / To List

class FromList c where
   type NestedList c d :: *
   fromList :: Storage c d => NestedList c d -> Data c d
   toList :: Storage c d => Data c d -> NestedList c d

instance FromList Nil where
   type NestedList Nil d = d
   fromList x = Data x
   toList (Data x) = x

instance (SV.FromList v2, FromList v1) => FromList (v2 :> v1) where
   type NestedList (v2 :> v1) d = [NestedList v1 d]
   fromList x =
      let y = nestedData $ SV.fromList $ L.map (getSubData y . fromList) x
      in  y
   toList xd =
      withNestedData (L.map (toList . subData xd) . SV.toList) xd


----------------------------------------------------------
-- All

class All c where
   all :: Storage c d => (d -> Bool) -> Data c d -> Bool
   any :: Storage c d => (d -> Bool) -> Data c d -> Bool

instance All Nil where
   all f (Data x) = f x
   any f (Data x) = f x

instance (SV.Singleton v2, All v1) => All (v2 :> v1) where
   all f xd = withNestedData (SV.all (all f . subData xd)) xd
   any f xd = withNestedData (SV.any (any f . subData xd)) xd


----------------------------------------------------------
-- Equal

class Equal c where
   equalBy ::
      (Storage c a, Storage c b) =>
      (a -> b -> Bool) -> Data c a -> Data c b -> Bool

instance Equal Nil where
   equalBy f (Data x) (Data y) = f x y

instance (SV.Walker v2, Equal v1) => Equal (v2 :> v1) where
   equalBy f xd yd =
      SV.equalBy
         (\xc yc -> equalBy f (subData xd xc) (subData yd yc))
         `withNestedData` xd
         `withNestedData` yd


----------------------------------------------------------
-- Transpose

transpose1 :: Data (v1 :> Nil) d -> Data (v1 :> Nil) d
transpose1 x = x

transpose2 ::
   (SV.Transpose v1 v2, SV.Storage v1 d) =>
   Data (v2 :> v1 :> Nil) d -> Data (v2 :> v1 :> Nil) d
transpose2 (Data x) = Data $ SV.transpose x


----------------------------------------------------------
-- Head & Tail
{-

{-# DEPRECATED head, tail "use viewL instead" #-}
{-# DEPRECATED last, init "use viewR instead" #-}

head, last ::
   (SV.Singleton v2, SV.Storage v2 (Apply v1 d)) =>
   Data (v2 :> v1) d -> Data v1 d
head (Data x) = Data $ SV.head x
last (Data x) = Data $ SV.last x

tail, init ::
   (SV.Singleton v2, SV.Storage v2 (Apply v1 d)) =>
   Data (v2 :> v1) d -> Data (v2 :> v1) d
tail (Data x) = Data $ SV.tail x
init (Data x) = Data $ SV.init x
-}

viewL ::
   (SV.Singleton v2, SV.Storage v2 (Apply v1 d)) =>
   Data (v2 :> v1) d -> Maybe (Data v1 d, Data (v2 :> v1) d)
viewL (Data x) = P.fmap (mapPair (Data, Data)) $ SV.viewL x

viewR ::
   (SV.Singleton v2, SV.Storage v2 (Apply v1 d)) =>
   Data (v2 :> v1) d -> Maybe (Data (v2 :> v1) d, Data v1 d)
viewR (Data x) = P.fmap (mapPair (Data, Data)) $ SV.viewR x

deltaMap ::
   (SV.Singleton v2, ZipWith (v2 :> v1),
    SV.Storage v2 (Apply v1 d1), SV.Storage v2 (Apply v1 d2),
    Storage v1 d1, Storage v1 d2) =>
   (d1 -> d1 -> d2) ->
   Data (v2 :> v1) d1 ->
   Data (v2 :> v1) d2
deltaMap f x = P.maybe mempty (zipWith f x . P.snd) $ viewL x



----------------------------------------------------------
-- Singleton

singleton ::
   (SV.Singleton v2, SV.Storage v2 (Apply v1 d)) =>
   Data v1 d -> Data (v2 :> v1) d
singleton (Data x) = Data $ SV.singleton x


----------------------------------------------------------
-- Sort

sort ::
   (Ord d, SV.Sort v, SV.Storage v d) =>
   Data (v :> Nil) d -> Data (v :> Nil) d
sort (Data x) = Data $ SV.sort x

sortBy ::
   (SV.SortBy v, SV.Storage v d) =>
   (d -> d -> P.Ordering) ->
   Data (v :> Nil) d ->
   Data (v :> Nil) d
sortBy f (Data x) = Data $ SV.sortBy f x

----------------------------------------------------------
-- Filter

class Filter c where
   filter :: (Storage c d) => (d -> Bool) -> Data c d -> Data c d

instance Filter1 v2 v1 => Filter (v2 :> v1) where
   filter = filter1


class Filter1 c2 c1 where
   filter1 :: (Storage (c2 :> c1) d) => (d -> Bool) -> Data (c2 :> c1) d -> Data (c2 :> c1) d

instance SV.Filter v => Filter1 v Nil where
   filter1 f xd = nestedData $ withNestedData (SV.filter f) xd

instance (SV.Walker v3, Filter1 v2 v1) => Filter1 v3 (v2 :> v1) where
   filter1 f xd =
      nestedData $ withNestedData (SV.map (getData . filter f . subData xd)) xd


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

{- |
Most simple implementation
but it will not take advantage of specialised conversions
at the most-inner vector.
-}
_convert ::
   (NestedList c1 d ~ NestedList c2 d,
    Storage c2 d, FromList c2,
    Storage c1 d, FromList c1) =>
   Data c1 d -> Data c2 d
_convert = fromList . toList

class Convert c1 c2 where
   convert :: (Storage c1 d, Storage c2 d) => Data c1 d -> Data c2 d

instance Convert Nil Nil where
   convert = id

instance (Convert1 v1 c1 v2 c2) => Convert (v1 :> c1) (v2 :> c2) where
   convert = convert1


class Convert1 v1 c1 v2 c2 where
   convert1 ::
      (Storage (v1 :> c1) d, Storage (v2 :> c2) d) =>
      Data (v1 :> c1) d -> Data (v2 :> c2) d

instance (SV.Convert v1 v2) => Convert1 v1 Nil v2 Nil where
   convert1 xd = nestedData (withNestedData SV.convert xd)

instance
   (SV.FromList w1, SV.FromList w2, Convert1 v1 c1 v2 c2) =>
      Convert1 w1 (v1 :> c1) w2 (v2 :> c2) where
   convert1 xd =
      let yd = nestedData (withNestedData (SV.fromList . L.map (getSubData yd . convert1 . subData xd) . SV.toList) xd)
      in  yd


----------------------------------------------------------
-- Length


len :: SV.Len (Apply c d) => Data c d -> Int
len (Data x) = SV.len x

length :: (SV.Length v, SV.Storage v (Apply c d)) => Data (v :> c) d -> Int
length (Data x) = SV.length x


{- |
Most simple implementation
but inefficient since it counts the elements one by one.
-}
_size :: (Fold c, Storage c d) => Data c d -> Int
_size = foldlMap (+) 0 (P.const 1)

class Size c where
   size :: (Storage c d) => Data c d -> Int

instance Size Nil where
   size (Data _) = 1

instance (Size1 v2 v1) => Size (v2 :> v1) where
   size = size1


class Size1 v2 v1 where
   size1 :: (Storage (v2 :> v1) d) => Data (v2 :> v1) d -> Int

instance SV.Length v => Size1 v Nil where
   size1 = withNestedData SV.length

instance (SV.Walker v3, Size1 v2 v1) => Size1 v3 (v2 :> v1) where
   size1 xd =
      withNestedData (vecFoldlMap (+) 0 (size1 . subData xd)) xd


instance Size c => SV.Length (Data c) where
   length = readData size


----------------------------------------------------------
-- Reverse

class Reverse c where
   reverse :: Storage c d => Data c d -> Data c d

instance (SV.Reverse v2) => Reverse (v2 :> v1) where
   reverse =
      withNestedData (Data . SV.reverse)


----------------------------------------------------------
-- Find

findIndex ::
   (SV.Find v, SV.Storage v d) =>
   (d -> Bool) -> Data (v :> Nil) d -> Maybe Int
findIndex f = withNestedData (SV.findIndex f)

slice ::
   (SV.Slice v, SV.Storage v d) =>
   Int -> Int -> Data (v :> Nil) d -> Data (v :> Nil) d
slice idx n = withNestedData (Data . SV.slice idx n)


concat :: (SV.Storage v1 (Apply c d),
           SV.Singleton v1,
           SV.Storage v2 (v1 (Apply c d)),
           SV.FromList v2)
          => Data (v2 :> v1 :> c) d -> Data (v1 :> c) d
concat (Data x) = Data $ SV.concat $ SV.toList x


