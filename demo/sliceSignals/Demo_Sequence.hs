module Main where

import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Sequence as Sequ
import EFA2.Signal.SequenceData
          (PPosIdx(PPosIdx), PowerRecord(PowerRecord), ListPowerRecord,
           Sequ, SequPwrRecord)
import EFA2.Signal.Signal (PSigL, (.++))
import EFA2.Signal.Base (Val)
import EFA2.Display.Plot (rPlot, rPlotCore)
import EFA2.Utils.Utils (divUp)

import qualified Graphics.Gnuplot.Advanced as Plot
import qualified Graphics.Gnuplot.Terminal.PostScript as PS
import qualified Graphics.Gnuplot.MultiPlot as MultiPlot
import qualified Graphics.Gnuplot.Frame as Frame

import qualified Data.Map as M
import qualified Data.Array as Array
import Control.Functor.HT (void)


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

pMap :: M.Map PPosIdx PSigL
pMap =
   M.fromList $
      (PPosIdx 0 1, mkSigEnd n s01) :
      (PPosIdx 1 0, mkSigEnd n s10) :
      (PPosIdx 1 2, mkSigEnd n s12) :
      (PPosIdx 2 1, mkSigEnd n s21) :
      (PPosIdx 1 3, mkSigEnd n s13) :
      (PPosIdx 3 1, mkSigEnd n s31) :
      []

pRec, pRec0 :: ListPowerRecord

pRec = PowerRecord (S.fromList time) pMap
pRec0 = Sequ.addZeroCrossings pRec

sequ :: Sequ
sequRecA, sequRecB :: SequPwrRecord
(sequ,sequRecA) = Sequ.genSequ pRec0

sequRecB = Sequ.chopAtZeroCrossingsPowerRecord pRec


main :: IO ()
main = do
  putStrLn (show time)
  putStrLn (show pRec)
  putStrLn (show pRec0)

  putStrLn (show sequ)

  rPlot ("PowerRecord", pRec)
  rPlot ("SequA", sequRecA)
  rPlot ("SequB", sequRecB)

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
