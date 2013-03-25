{- | Skript to demonstrate Calculations and other Operations on Signals -}

-- module Demo_Signal where

import qualified EFA.Signal.Plot as Plot
import qualified EFA.Signal.Signal as S
import EFA.Signal.SignalFill ((.-), (./), (.*))
import EFA.Signal.Signal
  (PSig, TSig, Scal, FFSig)

import EFA.Signal.Typ (Typ, A, D, P, N, Tt)
import EFA.Signal.Base (Val)


-- Generate objects to work with
offset :: Scal (Typ D P Tt) Val
offset = S.toScalar 0

-- Time Vector
time :: TSig
time = S.fromList ([0,0.1..pi]++[pi])

-- constant efficiency
n1 :: Scal (Typ A N Tt) Val
n1 = S.toScalar 0.8

-- Generate two Power Signals
pSig1, pSig2 :: PSig
pSig1 =
   ((S.changeType (S.map sin time)) .- offset)
   .*
   (S.toScalar 1000 :: Scal (Typ A N Tt) Val)
pSig2 = pSig1 .* n1

-- Make Time-Step-Integration to get 1D energy flow signals
fSig1, fSig2 :: FFSig
fSig1 = S.partIntegrate time pSig1
fSig2 = S.partIntegrate time pSig2

nVal2 :: Scal (Typ A N Tt) Val
nVal2 = S.sigSum fSig2 ./ S.sigSum fSig1

main :: IO ()
main = do

  Plot.xyIO "Power" time pSig1
  Plot.xyIO "Power" time pSig2

  putStrLn (S.disp nVal2)

