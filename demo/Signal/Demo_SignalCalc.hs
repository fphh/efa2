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
           PSig,TSig, Scal, FSig,  FSig1,
           (.-), (.+), (./), (.*), (.++)) -- (&-), (&+), (&/), (&*), (&++))
          
import EFA2.Signal.Data (Data, Nil)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

-- Generate objects to work with
offset = S.toScalar 0 :: Scal (Typ D P Tt) Val

-- Time Vector
time = S.fromList ([0,0.1..pi]++[pi])  :: TSig

-- time step midth vector
mtime = S.avSig time :: TC FSignal (Typ A T Tt) (UVec Val) 

-- time step vector
dtime = S.deltaSig time :: TC FSignal (Typ D T Tt) (UVec Val) 

-- Constanter Wirkungsgrad
n1 = S.toScalar 0.8 :: Scal (Typ A N Tt) Val

-- Generate two Power Signals
pSig1 = ((S.changeType (S.map sin time)).-offset).*((S.toScalar 1000):: Scal (Typ A N Tt) Val) ::  PSig
pSig2 = pSig1.*n1

-- Make Time-Step-Integration to get 1D energy flow signals
fSig1 = S.sigPartInt time pSig1  :: FSig
fSig2 = S.sigPartInt time pSig2  :: FSig

{-
-- Calculate 1D average Power Signal 
pavSig1 = fSig1./dtime :: FSig1 (Typ A P Tt) Val 
-}

{-
-- Calculate 1D efficiency signal
nSig = fSig2./fSig1 :: FSig1 (Typ A N Tt)  Val
-}

{-
-- calculate effektive efficiency value in two ways
nVal1 = (S.sigFullInt time pSig2)./(S.sigFullInt time pSig1) ::  Scal (Typ A N Tt) Val
-}

nVal2 = (S.sigSum fSig2)./S.sigSum(fSig1) ::  Scal (Typ A N Tt) Val

main = do  
  
  xyplot "Power" time pSig1  
  
{-  
-- !! - doesn't compile, can't plot fsignal
  xyplot "Energie" time eSig 
-}

  putStrLn(sdisp nVal2)  
