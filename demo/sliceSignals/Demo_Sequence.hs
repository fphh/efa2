
-- | Demonstriert das Plotten von Signalen.

module Main where

import qualified EFA.Application.Index as XIdx

import qualified EFA.Signal.Signal as S
import qualified EFA.Signal.Sequence as Sequ

import EFA.Signal.SequenceData (SequData)
import EFA.Signal.Record (PowerRecord, Record(Record))



import EFA.Signal.Signal (PSigL, (.++))
import EFA.Signal.Base (Val)
import qualified EFA.Signal.PlotIO as PlotIO

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm


mkSig :: Int -> [Val] -> PSigL
mkSig m = S.fromList . concat . replicate m

mkSigEnd :: Int -> [Val] -> PSigL
mkSigEnd m s = mkSig m s  .++  S.fromList [head s]

time :: [Val]
time = take 13 [0 ..]

s01, s10, s12, s21, s13, s31 :: [Val]
s01 = [0, 2, 2, 0, 0, 0]
s10 = [0, 0.8, 0.8, 0, 0, 0]
s12 = [0.3, 0.3, 0.3, 0.3, 0.3, 0.3]
s21 = [0.2, 0.2, 0.2, 0.2, 0.2, 0.2]
s13 = [0, 0.5, 0.5, -0.3, -0.3, -0.3]
s31 = [0, 0.25, 0.25, 0, -0.6, -0.6]

n :: Int
n = 2

pPosIdx :: Int -> Int -> XIdx.PPos Int
pPosIdx x y = XIdx.ppos x y

pMap :: Map (XIdx.PPos Int) PSigL
pMap =
   Map.fromListWith (error "duplicate keys") $
      (pPosIdx 0 1, mkSigEnd n s01) :
      (pPosIdx 1 0, mkSigEnd n s10) :
      (pPosIdx 1 2, mkSigEnd n s12) :
      (pPosIdx 2 1, mkSigEnd n s21) :
      (pPosIdx 1 3, mkSigEnd n s13) :
      (pPosIdx 3 1, mkSigEnd n s31) :
      []


pRec, pRec0 :: (PowerRecord Int [] Val)
pRec = Record (S.fromList time) pMap
pRec0 = Sequ.addZeroCrossings pRec

sequRecA, sequRecB :: SequData (PowerRecord Int [] Val)
sequRecA = Sequ.genSequ pRec0

sequRecB = Sequ.chopAtZeroCrossingsPowerRecord pRec


main :: IO ()
main = do
  print time
  print pRec
  print pRec0

  PlotIO.record "PowerRecord" DefaultTerm.cons show id pRec
  PlotIO.sequence "SequA" DefaultTerm.cons show id sequRecA
  PlotIO.sequence "SequB" DefaultTerm.cons show id sequRecB

{-
  {-
  The result looks awful, because many parts overlap.
  -}
  void $ Plot.plot (PS.cons "sequence.eps") $
     (\xs ->
        MultiPlot.simpleFromPartArray $
        let width = 3
        in  Array.listArray ((1,1), (divUp (length xs) width, width)) $
            map MultiPlot.partFromFrame xs ++
            repeat (MultiPlot.partFromFrame Frame.empty)) $
     rPlotCore "Sequ" sequRecB
-}
