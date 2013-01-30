{-# LANGUAGE TypeOperators #-}

-- | Demonstriert das Schneiden von Signalen

module Main where

import qualified Data.Map as M 

import qualified EFA.Signal.Signal as S
import EFA.Signal.Sequence
import EFA.Signal.SequenceData
import EFA.Signal.Record


import EFA.Report.Report

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import EFA.Signal.Data ((:>), Nil, Data)

import qualified EFA.Graph.Topology.Nodes as N

node0, node1 :: N.Nodes
node0 :~ node1 :~ _ = Stream.enumFrom $ N.Node 0

time :: S.TC s t (Data ([] :> Nil) Double)
time = S.fromList [0,10..50]

p1, p2 :: S.TC s t (Data ([] :> Nil) Double)
p1 = S.fromList [1,0,0,1,0,0]
p2 = S.fromList [1,0,1,1,1,0]

pmap :: M.Map (PPosIdx N.Nodes) (S.TC s t (Data ([] :> Nil) Double))
pmap = M.fromListWith
         (error "duplicate keys")
         [(PPosIdx node0 node1, p1),(PPosIdx node1 node0, p2)]

rec, rec0 :: PowerRecord N.Nodes [] Double
rec = PowerRecord time pmap
rec0 = addZeroCrossings rec

sqRec :: SequData (PowerRecord N.Nodes [] Double)
sequ :: Sequ
(sequ, sqRec) = genSequ rec0

main :: IO ()
main = do
  report [] ("rec", rec)
  report [RAll] ("rec0",rec0)
  report [] ("sequRec",sqRec)
