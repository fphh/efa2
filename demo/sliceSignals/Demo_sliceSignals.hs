{-# LANGUAGE TypeOperators #-}

module Main where

-- | Demonstriert das Schneiden von Signalen

import qualified EFA.Flow.Sequence.Index as XIdx

import qualified EFA.Signal.Signal as S
import EFA.Signal.Sequence
import EFA.Signal.SequenceData (SequData)
import EFA.Signal.Record
import EFA.Signal.Data ((:>), Nil, Data)
import EFA.Report.Report (ToTable, ROpt(RAll), report)

import EFA.Utility (idxList)

import qualified Data.Map as Map
import Data.Map (Map)


data Node = Node0 | Node1 deriving (Eq, Ord, Show)

node0, node1 :: Node
node0 = Node0
node1 = Node1

time :: S.TC s t (Data ([] :> Nil) Double)
time = S.fromList [0,10..50]

--------------------------------------------------

t1 :: String
t1 = "left event"

pa1 :: [Double]
pa1 = [1,2,3,0,0,0]

pmap1 :: Map (XIdx.PPos Node) [Double]
pmap1 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, pa1)]

--------------------------------------------------

t2 :: String
t2 = "right event"

pa2 :: [Double]
pa2 = [0,0,0,4,4,4]

pmap2 :: Map (XIdx.PPos Node) [Double]
pmap2 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, pa2)]

--------------------------------------------------

t3 :: String
t3 = "only one start point"

pa3 :: [Double]
pa3 = [1,0,0,0,0,0]

pmap3 :: Map (XIdx.PPos Node) [Double]
pmap3 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, pa3)]

--------------------------------------------------

t4 :: String
t4 = "only one end point"

pa4 :: [Double]
pa4 = [0,0,0,0,0,1]

pmap4 :: Map (XIdx.PPos Node) [Double]
pmap4 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, pa4)]

--------------------------------------------------

t5 :: String
t5 = "only one middle Point"

pa5 :: [Double]
pa5 = [0,0,1,0,0,0]

pmap5 :: Map (XIdx.PPos Node) [Double]
pmap5 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, pa5)]

--------------------------------------------------

t6 :: String
t6 = "two signals shifted to zero"

pa6 :: [Double]
pa6 = [1,2,0,0,0,0]

pb6 :: [Double]
pb6 = [3,4,3,0,0,0]

pmap6 :: Map (XIdx.PPos Node) [Double]
pmap6 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, pa6), (XIdx.ppos node1 node0, pb6)]

--------------------------------------------------

t7 :: String
t7 = "two signals mixed event"

pa7 :: [Double]
pa7 = [2,2,0,0,0,0]

pb7 :: [Double]
pb7 = [0,0,3,3,3,3]

pmap7 :: Map (XIdx.PPos Node) [Double]
pmap7 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, pa7), (XIdx.ppos node1 node0, pb7)]

--------------------------------------------------

t8 :: String
t8 = "zero crossing"

p8 :: [Double]
p8 = [2,2,2,-2,-2,-2]

pmap8 :: Map (XIdx.PPos Node) [Double]
pmap8 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, p8)]

--------------------------------------------------

t9 :: String
t9 = "zero start Point"

p9 :: [Double]
p9 = [0,2,2,2,2,2]

pmap9 :: Map (XIdx.PPos Node) [Double]
pmap9 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, p9)]

--------------------------------------------------

t10 :: String
t10 = "zero end Point"

p10 :: [Double]
p10 = [2,2,2,2,2,0]

pmap10 :: Map (XIdx.PPos Node) [Double]
pmap10 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, p10)]

--------------------------------------------------

t11 :: String
t11 = "two events sametime"

p11a :: [Double]
p11a = [2,2,0,0,0,0]

p11b :: [Double]
p11b = [0,0,0,2,2,2]

pmap11 :: Map (XIdx.PPos Node) [Double]
pmap11 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, p11a), (XIdx.ppos node1 node0, p11b)]

--------------------------------------------------

t12 :: String
t12 = "single Zero Point"

p12 :: [Double]
p12 = [2,2,0,2,2,2]

pmap12 :: Map (XIdx.PPos Node) [Double]
pmap12 = Map.fromListWith (error "duplicate keys") [(XIdx.ppos node0 node1, p12)]

--------------------------------------------------

titleList :: [String]
titleList = [t1, t2, t3, t4, t5, t6, t7, t9, t10, t8, t11, t12]

pmapList :: [Map (XIdx.PPos Node) (S.TC s t (Data ([] :> Nil) Double))]
pmapList =
  map (Map.map S.fromList)
      [ pmap1, pmap2, pmap3, pmap4, pmap5, pmap6,
        pmap7, pmap9, pmap10, pmap8, pmap11, pmap12]

recList :: [PowerRecord Node [] Double]
recList = map (Record time) pmapList


list :: [(Int, (String, (PowerRecord Node [] Double, SequData (PowerRecord Node [] Double))))]
list = idxList $
  zip titleList
      (zip recList (map  (genSequ . addZeroCrossings) recList))

f :: (Num a, Show a, ToTable sequ) =>
  (a, ([Char], (PowerRecord Node [] Double, sequ))) -> IO ()
f (idx, (title, (pRec, sq))) = do
  putStrLn ""
  putStrLn $ "Test " ++ show (idx+1) ++ ": " ++ title
  report [RAll] (title, pRec)
  report [RAll] (title, addZeroCrossings pRec)
  report [] (title, sq)

main :: IO ()
main = mapM_ f list


