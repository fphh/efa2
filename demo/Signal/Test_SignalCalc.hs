{-# LANGUAGE FlexibleContexts, 
TypeOperators, 
ExistentialQuantification, 
RankNTypes #-}

{- Skript to demonstrate Calculations and other Operations on Signals -} 

-- module Demo_Signal where

import qualified EFA2.Signal.Signal as S
import EFA2.Display.DispSignal 
import EFA2.Display.Plot
import EFA2.Signal.Typ
import EFA2.Signal.Base
import EFA2.Signal.Data

import EFA2.Signal.Signal
          (TC, Scalar, Signal, FSignal, FSamp, PFSamp, PSigL, UTFSig, Test1, Test2,
           (.-), (.+), (./), (.*), (.++), (&-), (&+), (&/), (&*), (&++))
          
import EFA2.Signal.Data (Data, Nil)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- Generate objects to work with
scal = S.toScalar 0.02 :: TC Scalar (Typ A T U) (DVal Val) 
sig1 = S.fromList [0..10]  :: TC Signal (Typ UT UT UT) (UVec Val) -- Power Signal1
sampleVec = S.fromList [1,3,2,1,3]  :: TC Samp (Typ UT UT UT) (UVec Val) -- Power Signal1

-- Scalar Calculations
test1 = scal .+ scal
test2 = scal./scal

test2 = sig1.*scal.-scal.*sig :: TC Signal (Typ UT UT UT) (UVec Val) -- Power Signal1
test3 = sig1.*scal./scal-sig1 :: TC Signal (Typ UT UT UT) (UVec Val) -- Power Signal1

sig3 = (S.map sin tSig2).+scal

main = do  
  
  xyplot "Sinus" tSig2 pSig1  
  surfPlot sig2D1
  putStrLn("")
