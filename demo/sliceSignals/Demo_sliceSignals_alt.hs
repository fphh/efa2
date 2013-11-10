{-# LANGUAGE TypeOperators #-}

-- | Demonstriert das Schneiden von Signalen

module Main where

import EFA.Example.Topology.LinearOne (Node(Source, Sink))

import qualified EFA.Flow.Topology.Index as XIdx

import qualified EFA.Signal.Sequence as Sequ
import qualified EFA.Signal.Signal as S
import EFA.Signal.Chop (addZeroCrossings, genSequ)
import EFA.Signal.Record (PowerRecord, Record(Record))
import EFA.Signal.Data (Data, Nil, (:>))

import EFA.Report.Report (ROpt(RAll), report)

import qualified Data.Map as Map
import Data.Map (Map)


node0, node1 :: Node
node0 = Source
node1 = Sink

time :: S.TC s t (Data ([] :> Nil) Double)
time = S.fromList [0,10..50]

p1, p2 :: S.TC s t (Data ([] :> Nil) Double)
p1 = S.fromList [1,0,0,1,0,0]
p2 = S.fromList [1,0,1,1,1,0]

pmap :: Map (XIdx.Position Node) (S.TC s t (Data ([] :> Nil) Double))
pmap = Map.fromListWith
         (error "duplicate keys")
         [(XIdx.ppos node0 node1, p1),(XIdx.ppos node1 node0, p2)]

rec, rec0 :: PowerRecord Node [] Double
rec = Record time pmap
rec0 = addZeroCrossings rec

sequ :: Sequ.List (PowerRecord Node [] Double)
sequ = genSequ rec0

main :: IO ()
main = do
  report [] ("rec", rec)
  report [RAll] ("rec0",rec0)
  report [] ("sequ",sequ)
