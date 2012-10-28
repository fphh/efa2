{- | Skript to demonstrate Calculations and other Operations on Signals -}

-- module Demo_Signal where

import qualified EFA2.Signal.Plot as Plot
import qualified EFA2.Signal.Signal as S
import EFA2.Report.Signal (sdisp)
import EFA2.Signal.SignalFill ((.-), (./), (.*))
import EFA2.Signal.Signal
          (TC, Scalar, Signal, FSignal, FSamp, PFSamp, PSigL, UTFSig, Test1, Test2,
           PSig,TSig, Scal, FFSig,  FSig1,
           (.++)) -- (&-), (&+), (&/), (&*), (&++))

import EFA2.Signal.Typ (Typ, A, D, P, N, T, Tt)
import EFA2.Signal.Data (UVec)
import EFA2.Signal.Base (Val)


-- Generate objects to work with
offset :: Scal (Typ D P Tt) Val
offset = S.toScalar 0

-- Time Vector
time :: TSig
time = S.fromList ([0,0.1..pi]++[pi])

-- time step midth vector
mtime :: TC FSignal (Typ A T Tt) (UVec Val)
mtime = S.avSig time

-- time step vector
dtime :: TC FSignal (Typ D T Tt) (UVec Val)
dtime = S.deltaSig time

-- constant efficiency
n1 :: Scal (Typ A N Tt) Val
n1 = S.toScalar 0.8

-- Generate two Power Signals
pSig1, pSig2 :: PSig
pSig1 =
   ((S.changeType (S.map sin time)) .- offset)
   .*
   (S.toScalar 1000 :: Scal (Typ A N Tt) Val)
pSig2 = pSig1.*n1

-- Make Time-Step-Integration to get 1D energy flow signals
fSig1, fSig2 :: FFSig
fSig1 = S.sigPartInt time pSig1
fSig2 = S.sigPartInt time pSig2

-- Calculate 1D average Power Signal
pavSig1 :: FSig1 (Typ A P Tt) Val
pavSig1 = fSig1 ./ dtime

-- Calculate 1D efficiency signal
nSig :: FSig1 (Typ A N Tt) Val
nSig = fSig2 ./ fSig1

-- calculate effective efficiency value in two ways
nVal1 :: FSig1 (Typ A N Tt) Val
nVal1 = S.sigFullInt time pSig2 ./ S.sigFullInt time pSig1

nVal2 :: Scal (Typ A N Tt) Val
nVal2 = S.sigSum fSig2 ./ S.sigSum fSig1

main :: IO ()
main = do

  Plot.xyplot "Power" time pSig1

{-
-- !! - doesn't compile, can't plot fsignal
  Plot.xyplot "Energie" time eSig
-}

  putStrLn (sdisp nVal2)
