{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}

module EFA2.Interpreter.Arith where


type Val = Double  
type PSample = Val
type TSample = Val
type DTSample = Val

type VSignal = [Val]
type Power = VSignal
type Time = VSignal

type SignalIdx = Int

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

instance (Arith a) => Arith [a] where
         zero = repeat (zero :: a)
         cst x = repeat (cst x :: a)
         neg = map neg
         rec = map rec
         (.+) = zipWith (.+)
         (.*) = zipWith (.*)
         (./) = zipWith (./)

