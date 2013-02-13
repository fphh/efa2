-- | Demonstriert das Hinzufügen von ZeroCrossings

module Main where


import qualified Data.Map as M 

import EFA.Signal.Sequence
import EFA.Signal.Record
import EFA.Signal.Signal as S
import EFA.Signal.Plot
import EFA.Signal.Base (Val)


data Nodes = Node0 | Node1 | Node2 deriving (Eq, Ord, Show)

t :: TSigL
t = S.fromList [0,1,2] 

p1, p2, p3 :: PSigL
p1 = S.fromList [-1,1,1]
p2 = S.fromList [-1,3,3]
p3 = S.fromList [-1,6,-6]

pRec :: PowerRecord Nodes [] Val
pRec = Record t
         (M.fromListWith
            (error "duplicate keys")
            [ (PPosIdx Node0 Node1, p1),
              (PPosIdx Node1 Node0, p2),
              (PPosIdx Node1 Node2, p3)])

pRec0 :: PowerRecord Nodes [] Val
pRec0 = addZeroCrossings pRec

main :: IO ()
main = do
  print pRec
  print pRec0
  rPlot "pRec0" pRec0
