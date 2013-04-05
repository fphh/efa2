-- | Demonstriert das Hinzufügen von ZeroCrossings

module Main where

import qualified EFA.Example.Index as XIdx

import EFA.Signal.Sequence
import EFA.Signal.Record
import EFA.Signal.Signal as S
import qualified EFA.Signal.Plot as Plot
import EFA.Signal.Base (Val)

import qualified Data.Map as M


data Node = Node0 | Node1 | Node2 deriving (Eq, Ord, Show)

t :: TSigL
t = S.fromList [0,1,2]

p1, p2, p3 :: PSigL
p1 = S.fromList [-1,1,1]
p2 = S.fromList [-1,3,3]
p3 = S.fromList [-1,6,-6]

pRec :: PowerRecord Node [] Val
pRec = Record t
         (M.fromListWith
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
  Plot.recordIO "pRec0" pRec0
