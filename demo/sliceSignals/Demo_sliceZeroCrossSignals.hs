{-# LANGUAGE TypeOperators #-}

-- | Demonstriert das Schneiden von zero-crossing-Signalen

module Main where

import qualified EFA.Example.Index as XIdx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Signal.Signal as S
import EFA.Signal.Sequence
import EFA.Signal.SequenceData
import EFA.Signal.Record (Record(Record), PowerRecord)
import EFA.Signal.Data ((:>), Nil, Data)

import EFA.Utility (idxList)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified Data.Map as M



node0, node1 :: Node.Int
node0 :~ node1 :~ _ = Stream.enumFrom minBound

time :: S.TC s t (Data ([] :> Nil) Double)
time = S.fromList [0, 10..50]

t :: String
t = "zero crossing"

p :: S.TC s t (Data ([] :> Nil) Double)
p = S.fromList [2, 2, 2, -2, -2]

pmap :: M.Map (XIdx.PPos Node.Int) (S.TC s t (Data ([] :> Nil) Double))
pmap = M.fromListWith
         (error "duplicate keys")
         [(XIdx.ppos node0 node1,  p)]


titleList :: [String]
titleList = [t]

pmapList :: [M.Map (XIdx.PPos Node.Int) (S.TC s t (Data ([] :> Nil) Double))]
pmapList = [pmap]

recList :: [PowerRecord Node.Int [] Double]
recList = map (Record time) pmapList

list ::
  [(Int, (String, (PowerRecord Node.Int [] Double, SequData (PowerRecord Node.Int [] Double))))]
list = idxList $
  zip titleList
      (zip recList (map (genSequ . addZeroCrossings) recList))

-- f ::
--   (Num a, Show a2, Show a1, Show a) =>
--   (a, ([Char], (SequData (PowerRecord Node.Int [] Double), (a1, a2)))) -> IO ()

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


