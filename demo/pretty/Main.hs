{-# LANGUAGE TypeOperators #-}

module Main where

import qualified EFA.Example.Index as XIdx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Signal.Signal as S
import EFA.Signal.Record
import EFA.Signal.Typ (Typ, A, P, Tt)
import EFA.Signal.Data ((:>), Nil, Data)

import EFA.Report.Report (report, ROpt(..))

import qualified Data.Map as M


data Node = Node0 | Node1 deriving (Eq, Ord, Enum, Show)

instance Node.C Node where
   display = Node.displayDefault
   subscript = Node.subscriptDefault
   dotId = Node.dotIdDefault


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
      (M.fromListWith
         (error "duplicate keys")
         [(XIdx.ppos Node0 Node1, p1), (XIdx.ppos Node1 Node0, p2)])


main :: IO ()
main = do

  print t

  report [RVertical] ("TestMatrix", ll)
  report [RVertical] ("Power1", p1)
  report [RVertical] ("PowerRecord", r)
