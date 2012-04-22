{-# LANGUAGE TypeSynonymInstances, ScopedTypeVariables #-}

module EFA2.Interpreter.Arith where

type Val = Double  

-- Time Signal & Samples
type Signal = [Val]

type Power = Signal
type Time = Signal

type PSample = Val
type TSample = Val

-- Flow Signals and Samples
type FSignal = [Val]

type Flow = FSignal 
type DTime = FSignal  
  
type DTSample = Val -- Time step
type FPSample = Val -- Flow Power



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
