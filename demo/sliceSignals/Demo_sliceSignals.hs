{-# LANGUAGE TypeOperators #-}

module Main where

-- | Demonstriert das Schneiden von Signalen

import qualified Data.Map as M 

import qualified EFA.Signal.Signal as S
import EFA.Signal.Sequence
import EFA.Signal.SequenceData (Sequ, SequData)
import EFA.Signal.Record
import EFA.Report.Report (ToTable, ROpt(RAll), report)

import qualified EFA.Utility.Stream as Stream
import EFA.Utility.Stream (Stream((:~)))

import EFA.Utility (idxList)

import EFA.Signal.Data ((:>), Nil, Data)

import qualified EFA.Graph.Topology.Node as Node

node0, node1 :: Node.Int
node0 :~ node1 :~ _ = Stream.enumFrom minBound

time :: S.TC s t (Data ([] :> Nil) Double)
time = S.fromList [0,10..50]

--------------------------------------------------

t1 :: String
t1 = "left event"

pa1 :: [Double]
pa1 = [1,2,3,0,0,0]

pmap1 :: M.Map (PPosIdx Node.Int) [Double]
pmap1 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, pa1)]

--------------------------------------------------

t2 :: String
t2 = "right event"

pa2 :: [Double]
pa2 = [0,0,0,4,4,4]

pmap2 :: M.Map (PPosIdx Node.Int) [Double]
pmap2 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, pa2)]

--------------------------------------------------

t3 :: String
t3 = "only one start point"

pa3 :: [Double]
pa3 = [1,0,0,0,0,0]

pmap3 :: M.Map (PPosIdx Node.Int) [Double]
pmap3 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, pa3)]

--------------------------------------------------

t4 :: String
t4 = "only one end point"

pa4 :: [Double]
pa4 = [0,0,0,0,0,1]

pmap4 :: M.Map (PPosIdx Node.Int) [Double]
pmap4 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, pa4)]

--------------------------------------------------

t5 :: String
t5 = "only one middle Point"

pa5 :: [Double]
pa5 = [0,0,1,0,0,0]

pmap5 :: M.Map (PPosIdx Node.Int) [Double]
pmap5 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, pa5)]

--------------------------------------------------

t6 :: String
t6 = "two signals shifted to zero"

pa6 :: [Double]
pa6 = [1,2,0,0,0,0]

pb6 :: [Double]
pb6 = [3,4,3,0,0,0]

pmap6 :: M.Map (PPosIdx Node.Int) [Double]
pmap6 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, pa6), (PPosIdx node1 node0, pb6)]

--------------------------------------------------

t7 :: String
t7 = "two signals mixed event"

pa7 :: [Double]
pa7 = [2,2,0,0,0,0]

pb7 :: [Double]
pb7 = [0,0,3,3,3,3]

pmap7 :: M.Map (PPosIdx Node.Int) [Double]
pmap7 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, pa7), (PPosIdx node1 node0, pb7)]

--------------------------------------------------

t8 :: String
t8 = "zero crossing"

p8 :: [Double]
p8 = [2,2,2,-2,-2,-2]

pmap8 :: M.Map (PPosIdx Node.Int) [Double]
pmap8 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, p8)]

--------------------------------------------------

t9 :: String
t9 = "zero start Point"

p9 :: [Double]
p9 = [0,2,2,2,2,2]

pmap9 :: M.Map (PPosIdx Node.Int) [Double]
pmap9 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, p9)]

--------------------------------------------------

t10 :: String
t10 = "zero end Point"

p10 :: [Double]
p10 = [2,2,2,2,2,0]

pmap10 :: M.Map (PPosIdx Node.Int) [Double]
pmap10 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, p10)]

--------------------------------------------------

t11 :: String
t11 = "two events sametime"

p11a :: [Double]
p11a = [2,2,0,0,0,0]

p11b :: [Double]
p11b = [0,0,0,2,2,2]

pmap11 :: M.Map (PPosIdx Node.Int) [Double]
pmap11 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, p11a), (PPosIdx node1 node0, p11b)]

--------------------------------------------------

t12 :: String
t12 = "single Zero Point"

p12 :: [Double]
p12 = [2,2,0,2,2,2]

pmap12 :: M.Map (PPosIdx Node.Int) [Double]
pmap12 = M.fromListWith (error "duplicate keys") [(PPosIdx node0 node1, p12)]

--------------------------------------------------

titleList :: [String]
titleList = [t1, t2, t3, t4, t5, t6, t7, t9, t10, t8, t11, t12]

pmapList :: [M.Map (PPosIdx Node.Int) (S.TC s t (Data ([] :> Nil) Double))]
pmapList =
  map (M.map S.fromList)
      [ pmap1, pmap2, pmap3, pmap4, pmap5, pmap6,
        pmap7, pmap9, pmap10, pmap8, pmap11, pmap12]

recList :: [PowerRecord Node.Int [] Double]
recList = map (Record time) pmapList

list :: [(Int, (String, (PowerRecord Node.Int [] Double, (Sequ, SequData (PowerRecord Node.Int [] Double)))))]
list = idxList $ 
  zip titleList 
      (zip recList (map  (genSequ . addZeroCrossings) recList))

f :: (Num a, Show a, ToTable a2, ToTable a1) =>
  (a, ([Char], (PowerRecord Node.Int [] Double, (a1, a2)))) -> IO ()
f (idx, (title, (pRec, (sq, sqRec)))) = do
  putStrLn ""
  putStrLn $ "Test " ++ show (idx+1) ++ ": " ++ title
  report [RAll] (title, pRec)
  report [RAll] (title, addZeroCrossings pRec)
  report [] (title, sq)
  report [RAll] (title, sqRec)

main :: IO ()
main = mapM_ f list
  
    
