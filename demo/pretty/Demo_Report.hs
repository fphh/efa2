{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Map as M 

import qualified EFA.Signal.Signal as S
import EFA.Signal.Record
import EFA.Signal.Typ (Typ, A, P, Tt)

import EFA.Report.Report (report, ROpt(..))

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import EFA.Signal.Data ((:>), Nil, Data)

import qualified EFA.Graph.Topology.Index as Idx

node0, node1 :: Idx.Node
node0 :~ node1 :~ _ = Stream.enumFrom $ Idx.Node 0

l :: [Double]
l = [1..5]

ll :: [[Double]]
ll = [l,l]

p1, p2 :: S.TC S.Signal (Typ A P Tt) (Data ([] :> Nil) Double)
p1 = S.fromList l
p2 = p1

t :: S.TC s t (Data ([] :> Nil) Double)
t = S.fromList [0..4]

r :: PowerRecord [] Double
r = Record t 
      (M.fromListWith
         (error "duplicate keys")
         [(PPosIdx node0 node0, p1), (PPosIdx node0 node1, p2)])


main :: IO ()
main = do
  
  print t

  report [RVertical] ("TestMatrix", ll) 
  report [RVertical] ("Power1", p1) 
  report [RVertical] ("PowerRecord", r) 
