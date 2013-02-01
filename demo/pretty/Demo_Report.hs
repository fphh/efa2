{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Map as M 

import qualified EFA.Signal.Signal as S
import EFA.Signal.Record
import EFA.Signal.Typ (Typ, A, P, Tt)

import EFA.Report.Report (report, ROpt(..))

-- import qualified EFA.Utility.Stream as Stream
-- import EFA.Utility.Stream (Stream((:~)))

import EFA.Signal.Data ((:>), Nil, Data)

import qualified EFA.Graph.Topology.Node as Node
data Nodes = Node0 | Node1 deriving (Eq, Ord, Show)

instance Node.Show Nodes

l :: [Double]
l = [1..5]

ll :: [[Double]]
ll = [l,l]

p1, p2 :: S.TC S.Signal (Typ A P Tt) (Data ([] :> Nil) Double)
p1 = S.fromList l
p2 = p1

t :: S.TC s t (Data ([] :> Nil) Double)
t = S.fromList [0..4]

r :: PowerRecord Nodes [] Double
r = Record t 
      (M.fromListWith
         (error "duplicate keys")
         [(PPosIdx Node0 Node1, p1), (PPosIdx Node1 Node0, p2)])


main :: IO ()
main = do
  
  print t

  report [RVertical] ("TestMatrix", ll) 
  report [RVertical] ("Power1", p1) 
  report [RVertical] ("PowerRecord", r) 
