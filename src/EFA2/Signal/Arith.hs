{-# LANGUAGE TypeSynonymInstances #-}

module EFA2.Signal.Arith where

type Val = Double  
type VSignal = [Val]
type Power = VSignal
type Time = VSignal

class Arith a where
      zero :: a
      cst :: Double -> a
      neg :: a -> a
      rec :: a -> a
      (.+) :: a -> a -> a
      (.*) :: a -> a -> a
      (./) :: a -> a -> a

instance Arith Val where
         zero = 0.0
         cst = id
         neg = negate
         rec = recip
         (.+) = (+)
         (.*) = (*)
         (./) = (/)

instance Arith VSignal where
  zero = repeat 0
  cst x = repeat x
  neg = map negate
  rec = map recip
  (.+) = zipWith (+)
  (.*) = zipWith (*)
  (./) = zipWith (/)
