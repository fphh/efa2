{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Data.Map as M 

import qualified EFA.Signal.Signal as S
import EFA.Signal.Record
import EFA.Signal.Typ (Typ, A, P, Tt)
import EFA.Report.Report (report, ROpt(..))
import EFA.Signal.Data ((:>), Nil, Data)


data Nodes = Node0 | Node1 deriving (Eq, Ord, Show)

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
r = PowerRecord t
      (M.fromListWith
         (error "duplicate keys")
         [(PPosIdx Node0 Node0, p1), (PPosIdx Node0 Node1, p2)])


main :: IO ()
main = do
  
  print t

  report [RVertical] ("TestMatrix", ll) 
  report [RVertical] ("Power1", p1) 
  report [RVertical] ("PowerRecord", r) 
