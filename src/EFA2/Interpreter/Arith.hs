{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables, MultiParamTypeClasses, KindSignatures #-}

module EFA2.Interpreter.Arith where

import qualified Data.Vector.Unboxed as UV

import Data.Ratio

--type Val = Ratio Integer
type Val = Double  

type Container = []
--type Container = UV.Vector

{-
-- ATTENTION on operator presedence: TODO!!!

class Arith a where
      zero :: a
      cst :: Val -> a
      neg :: a -> a
      rec :: a -> a
      (.+) :: a -> a -> a
      (.*) :: a -> a -> a
      (./) :: a -> a -> a
      (.-) :: a -> a -> a
      x .- y = x .+ (neg y)
      absol :: (Ord a, Num a) => a -> a
      absol x | x < 0 = -x
      absol x = x

instance Arith Val where
         zero = 0.0
         cst = id
         neg = negate
         rec = recip
         (.+) = (+)
         (.*) = (*)
         x ./ 0 = 0
         x ./ y = x / y

instance (Arith a) => Arith [a] where
         zero = repeat (zero :: a)
         cst x = repeat (cst x :: a)
         neg = map neg
         rec = map rec
         (.+) = zipWith (.+)
         (.*) = zipWith (.*)
         (./) = zipWith (./)

instance (Arith a, UV.Unbox a) => Arith (UV.Vector a) where
         zero = UV.singleton (zero :: a)  -- UV.repeat (zero :: a)
         cst x = UV.singleton (cst x :: a) -- UV.repeat (cst x :: a)
         neg = UV.map neg
         rec = UV.map rec
         (.+) = UV.zipWith (.+)
         (.*) = UV.zipWith (.*)
         (./) = UV.zipWith (./)

-}