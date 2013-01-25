{-# LANGUAGE TypeOperators #-}

-- | Demonstriert das Schneiden von zero-crossing-Signalen

module Main where

import qualified Data.Map as M 

import qualified EFA.Signal.Signal as S
import EFA.Signal.Sequence
import EFA.Signal.SequenceData

import EFA.Utility (idxList)

import EFA.Report.Report

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import EFA.Signal.Data ((:>), Nil, Data)

import qualified EFA.Graph.Topology.Index as Idx


node0, node1 :: Idx.Node
node0 :~ node1 :~ _ = Stream.enumFrom $ Idx.Node 0

time :: S.TC s t (Data ([] :> Nil) Double)
time = S.fromList [0, 10..50]

t :: String
t = "zero crossing"

p :: S.TC s t (Data ([] :> Nil) Double)
p = S.fromList [2, 2, 2, -2, -2]

pmap :: M.Map PPosIdx (S.TC s t (Data ([] :> Nil) Double))
pmap = M.fromList [(PPosIdx node0 node1,  p)]


titleList :: [String]
titleList = [t]

pmapList :: [M.Map PPosIdx (S.TC s t (Data ([] :> Nil) Double))]
pmapList = [pmap]

recList = map (PowerRecord time) pmapList  

list = idxList $ zip titleList 
                     (zip recList (map  (genSequ) (map addZeroCrossings recList)))

f (idx, (title, (pRec, (sq, sqRec)))) = do
  putStrLn ""
  putStrLn $ "Test " ++ show (idx + 1) ++ ": " ++ title
  putStrLn ("XList: \n" ++ (show pRec))   
  putStrLn ("XList: \n" ++ (show (addZeroCrossings pRec)))   
  putStrLn ("Sequence: " ++  show sq) 
  putStrLn ("SequRec: " ++  show sqRec) 
  
main :: IO ()
main = mapM_ f list
  
    
