-- | Demonstriert das Hinzufügen von ZeroCrossings

module Main where

import qualified EFA.Application.Plot as PlotIO

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Signal.Signal as S
import EFA.Signal.Chop (addZeroCrossings)
import EFA.Signal.Record (Record(Record), PowerRecord)
import EFA.Signal.Signal (TSigL, PSigL)
import EFA.Signal.Base (Val)

import qualified Data.Map as Map
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm

data Node = Node0 | Node1 | Node2 deriving (Eq, Ord, Show)

t :: TSigL
t = S.fromList [0,1,2]

p1, p2, p3 :: PSigL
p1 = S.fromList [-1,1,1]
p2 = S.fromList [-1,3,3]
p3 = S.fromList [-1,6,-6]

pRec :: PowerRecord Node [] Val
pRec = Record t
         (Map.fromListWith
            (error "duplicate keys")
            [ (XIdx.ppos Node0 Node1, p1),
              (XIdx.ppos Node1 Node0, p2),
              (XIdx.ppos Node1 Node2, p3)])

pRec0 :: PowerRecord Node [] Val
pRec0 = addZeroCrossings pRec

main :: IO ()
main = do
  print pRec
  print pRec0
  PlotIO.record "pRec0" DefaultTerm.cons show id pRec0
