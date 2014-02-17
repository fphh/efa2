-- | Demonstriert das Hinzufügen von ZeroCrossings

module Main where

import EFA.Example.Topology.LinearTwo (Node(Source, Crossing, Sink))

import qualified EFA.Application.Plot as PlotIO

import qualified EFA.Flow.Topology.Index as XIdx

import qualified EFA.Signal.Signal as S
import EFA.Signal.Chop (addZeroCrossings)
import EFA.Signal.Record (Record(Record), PowerRecord)
import EFA.Signal.Signal (TSignal, PSignal)

import qualified Data.Map as Map
import qualified Graphics.Gnuplot.Terminal.Default as DefaultTerm


node0, node1, node2 :: Node
node0 = Source
node1 = Crossing
node2 = Sink

t :: TSignal [] Double
t = S.fromList [0,1,2]

p1, p2, p3 :: PSignal [] Double
p1 = S.fromList [-1,1,1]
p2 = S.fromList [-1,3,3]
p3 = S.fromList [-1,6,-6]

pRec :: PowerRecord Node [] Double
pRec = Record t
         (Map.fromListWith
            (error "duplicate keys")
            [ (XIdx.ppos node0 node1, p1),
              (XIdx.ppos node1 node0, p2),
              (XIdx.ppos node1 node2, p3)])

pRec0 :: PowerRecord Node [] Double
pRec0 = addZeroCrossings pRec

main :: IO ()
main = do
  print pRec
  print pRec0
  PlotIO.record "pRec0" DefaultTerm.cons show id pRec0
