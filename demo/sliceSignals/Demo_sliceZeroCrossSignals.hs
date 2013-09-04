{-# LANGUAGE TypeOperators #-}

-- | Demonstriert das Schneiden von zero-crossing-Signalen

module Main where

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Signal.Signal as Signal
import EFA.Signal.Sequence (genSequ, addZeroCrossings)
import EFA.Signal.SequenceData (SequData)
import EFA.Signal.Record (Record(Record), PowerRecord)
import EFA.Signal.Data ((:>), Nil, Data)

import EFA.Utility (idxList)

import qualified Data.Map as Map
import Data.Map (Map)



data Node = Node0 | Node1 deriving (Eq, Ord, Show)

node0, node1 :: Node
node0 = Node0
node1 = Node1

time :: Signal.TC s t (Data ([] :> Nil) Double)
time = Signal.fromList [0, 10..50]

t :: String
t = "zero crossing"

p :: Signal.TC s t (Data ([] :> Nil) Double)
p = Signal.fromList [2, 2, 2, -2, -2]

pmap :: Map (XIdx.PPos Node) (Signal.TC s t (Data ([] :> Nil) Double))
pmap = Map.fromListWith
         (error "duplicate keys")
         [(XIdx.ppos node0 node1,  p)]


titleList :: [String]
titleList = [t]

pmapList :: [Map (XIdx.PPos Node) (Signal.TC s t (Data ([] :> Nil) Double))]
pmapList = [pmap]

recList :: [PowerRecord Node [] Double]
recList = map (Record time) pmapList

list ::
  [(Int, (String, (PowerRecord Node [] Double, SequData (PowerRecord Node [] Double))))]
list = idxList $
  zip titleList
      (zip recList (map (genSequ . addZeroCrossings) recList))

-- f ::
--   (Num a, Show a2, Show a1, Show a) =>
--   (a, ([Char], (SequData (PowerRecord Node [] Double), (a1, a2)))) -> IO ()

f ::
   (Num a, Ord nty, Show seq, Show nty, Show a) =>
   (a, (String, (PowerRecord nty [] Double, seq))) -> IO ()

f (idx, (title, (pRec, sqRec))) = do
  putStrLn ""
  putStrLn $ "Test " ++ show (idx + 1) ++ ": " ++ title
  putStrLn ("XList: \n" ++ (show pRec))
  putStrLn ("XList: \n" ++ (show (addZeroCrossings pRec)))
  putStrLn ("Sequence: " ++  show sqRec)

main :: IO ()
main = mapM_ f list


