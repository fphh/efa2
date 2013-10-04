{-# LANGUAGE TypeOperators #-}

module Main where

import EFA.Example.Topology.LinearOne (Node(Source, Sink))

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Signal.Signal as S
import EFA.Signal.Record (PowerRecord, Record(Record))
import EFA.Signal.Data (Data, Nil, (:>))
import EFA.Signal.Typ (Typ, A, P, Tt)

import EFA.Report.Report (ROpt(RVertical), report)

import qualified Data.Map as Map


node0, node1 :: Node
node0 = Source
node1 = Sink


l :: [Double]
l = [1..5]

ll :: [[Double]]
ll = [l,l]

p1, p2 :: S.TC S.Signal (Typ A P Tt) (Data ([] :> Nil) Double)
p1 = S.fromList l
p2 = p1

t :: S.TC s t (Data ([] :> Nil) Double)
t = S.fromList [0..4]

r :: PowerRecord Node [] Double
r = Record t
      (Map.fromListWith
         (error "duplicate keys")
         [(XIdx.ppos node0 node1, p1), (XIdx.ppos node1 node0, p2)])


main :: IO ()
main = do

  print t

  report [RVertical] ("TestMatrix", ll)
  report [RVertical] ("Power1", p1)
  report [RVertical] ("PowerRecord", r)
