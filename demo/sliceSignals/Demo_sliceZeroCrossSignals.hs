{-# LANGUAGE TypeOperators #-}

-- | Demonstriert das Schneiden von zero-crossing-Signalen

module Main where

import qualified Data.Map as M 

import qualified EFA.Signal.Signal as S
import EFA.Signal.Sequence
import EFA.Signal.SequenceData
import EFA.Signal.Record

import EFA.Utility (idxList)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import qualified EFA.Graph.Topology.Node as Node

import EFA.Signal.Data ((:>), Nil, Data)


node0, node1 :: Node.Node
node0 :~ node1 :~ _ = Stream.enumFrom $ Node.Node 0

time :: S.TC s t (Data ([] :> Nil) Double)
time = S.fromList [0, 10..50]

t :: String
t = "zero crossing"

p :: S.TC s t (Data ([] :> Nil) Double)
p = S.fromList [2, 2, 2, -2, -2]

pmap :: M.Map (PPosIdx Node.Node) (S.TC s t (Data ([] :> Nil) Double))
pmap = M.fromListWith
         (error "duplicate keys") 
         [(PPosIdx node0 node1,  p)]


titleList :: [String]
titleList = [t]

pmapList :: [M.Map (PPosIdx Node.Node) (S.TC s t (Data ([] :> Nil) Double))]
pmapList = [pmap]

recList :: [PowerRecord Node.Node [] Double]
recList = map (Record time) pmapList  

list ::
  [(Int, (String, (PowerRecord Node.Node [] Double, (Sequ, SequData (PowerRecord Node.Node [] Double)))))]
list = idxList $
  zip titleList 
      (zip recList (map  (genSequ . addZeroCrossings) recList))

-- f :: 
--   (Num a, Show a2, Show a1, Show a) =>
--   (a, ([Char], (SequData (PowerRecord Node.Node [] Double), (a1, a2)))) -> IO ()

f :: (Num a, Ord nty, Show a2, Show a1, Show nty, Show a) =>
     (a, ([Char], (PowerRecord nty [] Double, (a1, a2))))
     -> IO ()

f (idx, (title, (pRec, (sq, sqRec)))) = do
  putStrLn ""
  putStrLn $ "Test " ++ show (idx + 1) ++ ": " ++ title
  putStrLn ("XList: \n" ++ (show pRec))   
  putStrLn ("XList: \n" ++ (show (addZeroCrossings pRec)))   
  putStrLn ("Sequence: " ++  show sq) 
  putStrLn ("SequRec: " ++  show sqRec) 
  
main :: IO ()
main = mapM_ f list
  
    
