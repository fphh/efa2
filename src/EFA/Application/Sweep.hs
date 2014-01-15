{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Application.Sweep where

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~-), (~*), (~/))

import qualified EFA.Report.FormatValue as FormatValue

import qualified Data.Vector.Unboxed as UV

import Data.Monoid(Monoid, mempty, mappend)

import Prelude hiding (map)



data Sweep vec a = Sweep { unSweep :: (vec a) } deriving (Show)

instance (UV.Unbox a, Eq a) => Eq (Sweep UV.Vector a) where
  _ == _ = error $ "EFA.Application.Sweep: Eq undefined: probable cause: "
                   ++ "use of more than one storage forcing"

instance (UV.Unbox a, Eq a, Ord a) => Ord (Sweep UV.Vector a) where
  compare _ _ = error $ "EFA.Application.Sweep: Ord undefined: probable cause: "
                        ++ " use of more than one storage forcing"


instance Monoid (Sweep UV.Vector Bool) where
  mappend x y = toSweep $ UV.zipWith (&&) (fromSweep x) (fromSweep y)
  mempty = error $ "EFA.Application.DoubleSweep: "
                   ++ "yeah, this monoid isn't fully defined. Sorry!"


data List (sweep :: (* -> *) -> * -> *) vec a =
  List { unList :: ![sweep vec a] } deriving (Show)

class SweepClass sweep vec a where
  length :: sweep vec a -> Int
  toSweep :: vec a -> sweep vec a
  fromSweep :: sweep vec a -> vec a
  replicate :: sweep vec a -> a -> sweep vec a
  fromRational :: Int -> a -> sweep vec a
  (!!!) :: sweep vec a -> Int -> a

instance (UV.Unbox a) => SweepClass Sweep UV.Vector a where
  length (Sweep x) = UV.length x
  {-# INLINE length #-}

  toSweep = Sweep
  {-# INLINE toSweep #-}

  fromSweep (Sweep x) = x
  {-# INLINE fromSweep #-}

  replicate (Sweep u) x =
    Sweep (UV.replicate (UV.length u) x)
  {-# INLINE replicate #-}

  fromRational len x =
    Sweep (UV.replicate len x)
  {-# INLINE fromRational #-}

  (Sweep u) !!! n = u UV.! n
  {-# INLINE (!!!) #-}

class SweepVector vec a where
  fromSweepVector :: vec a -> [a]
  toSweepVector :: [a] -> vec a

instance (UV.Unbox a) => SweepVector UV.Vector a where
  fromSweepVector = UV.toList
  {-# INLINE fromSweepVector #-}

  toSweepVector = UV.fromList
  {-# INLINE toSweepVector #-}

fromList ::
  (UV.Unbox a, SweepClass sweep vec a, SweepVector vec a) =>
  [a] -> sweep vec a
fromList = toSweep . toSweepVector
{-# INLINE fromList #-}

toList ::
  (UV.Unbox a, SweepClass sweep vec a, SweepVector vec a) =>
  sweep vec a -> [a]
toList = fromSweepVector . fromSweep
{-# INLINE toList #-}

class SweepMap (sweep :: (* -> *) -> * -> *) vec a b where
  map :: (a -> b) -> sweep vec a -> sweep vec b

instance (UV.Unbox a, UV.Unbox b, SweepClass sweep UV.Vector a,
          SweepClass sweep UV.Vector b) =>
         SweepMap sweep UV.Vector a b where
  map f = toSweep . UV.map f . fromSweep
  {-# INLINE map #-}


instance (Arith.Sum a, UV.Unbox a) => Arith.Sum (Sweep UV.Vector a) where
  (Sweep x) ~+ (Sweep y) = Sweep $ UV.zipWith (~+) x y
  {-# INLINE (~+) #-}

  (Sweep x) ~- (Sweep y) = Sweep $ UV.zipWith (~-) x y
  {-# INLINE (~-) #-}

  negate (Sweep x) = Sweep $ UV.map Arith.negate x
  {-# INLINE negate #-}


instance (Arith.Product a, Arith.Constant a, UV.Unbox a) =>
         Arith.Product (Sweep UV.Vector  a) where
  (Sweep x) ~* (Sweep y) = Sweep $ UV.zipWith (~*) x y
  {-# INLINE (~*) #-}

  (Sweep x) ~/ (Sweep y) = Sweep $ UV.zipWith (~/) x y
  {-# INLINE (~/) #-}

  recip (Sweep x) = Sweep $ UV.map Arith.recip x
  {-# INLINE recip #-}

  constOne (Sweep x) = Sweep $ UV.replicate (UV.length x) Arith.one
  {-# INLINE constOne #-}

instance Arith.Integrate (Sweep vec a) where
  type Scalar (Sweep vec a) = (Sweep vec a)
  integrate = id
  {-# INLINE integrate #-}


instance (UV.Unbox a, Eq a, Num a, Arith.Constant a) =>
         Arith.ZeroTestable (Sweep UV.Vector a) where
  allZeros (Sweep x) = UV.and (UV.map (Arith.zero ==) x)
  {-# INLINE allZeros #-}

  coincidingZeros (Sweep x) (Sweep y) =
    UV.or $ UV.zipWith (\a b -> a == Arith.zero && b == Arith.zero) x y

instance (FormatValue.FormatValue a, UV.Unbox a) =>
         FormatValue.FormatValue (Sweep UV.Vector a) where
  formatValue = FormatValue.formatValue . toList
