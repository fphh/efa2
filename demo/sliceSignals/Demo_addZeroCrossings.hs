
module Main where

-- | Demonstriert das Hinzufügen von ZeroCrossings

import EFA.Signal.Sequence
import EFA.Signal.Record
import EFA.Signal.Signal as S
import EFA.Signal.Plot
import EFA.Signal.Base (Val)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))


import qualified Data.Map as M 
import qualified EFA.Graph.Topology.Index as Idx

node0, node1, node2 :: Idx.Node
node0 :~ node1 :~ node2 :~ _ = Stream.enumFrom $ Idx.Node 0


t :: TSigL
t = S.fromList [0,1,2] 

p1, p2, p3 :: PSigL
p1 = S.fromList [-1,1,1]
p2 = S.fromList [-1,3,3]
p3 = S.fromList [-1,6,-6]

pRec :: PowerRecord [] Val
pRec = PowerRecord t
         (M.fromListWith
            (error "duplicate keys")
            [ (PPosIdx node0 node1, p1),
              (PPosIdx node1 node0, p2),
              (PPosIdx node1 node2, p3)])

pRec0 :: PowerRecord [] Val
pRec0 = addZeroCrossings pRec

main :: IO ()
main = do
  print pRec
  print pRec0
  rPlot ("pRec0", pRec0)
