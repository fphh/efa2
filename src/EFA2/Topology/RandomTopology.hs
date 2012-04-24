

module EFA2.Topology.RandomTopology where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.HT as LHT

import Data.Graph.Inductive
import System.Random

import EFA2.Interpreter.Env
import EFA2.Interpreter.Arith
import EFA2.Topology.Topology
import EFA2.Utils.Utils

import Debug.Trace

rand :: (RandomGen g, Random a) => (a, a) -> g -> a
rand range gen = head $ randomRs range gen

seed :: Int
seed = 3233333

ranges :: Int -> [(Int, Int)]
ranges n = zip (repeat 0) (take n $ iterate ((negate 1) +) (n-1))

indices :: Int -> Int -> [Int]
indices seed n = map (flip rand (mkStdGen seed)) (ranges n)

shuffle :: Int -> Int -> [Int]
shuffle seed n = fst $ L.foldl' f ([], ns) idx
  where f (acc, ns') i = (y:acc, xs++ys)
          where (xs, y:ys) = L.splitAt i ns'
        ns = [0..n-1]
        idx = indices seed n

nodesAndEdges :: Int -> Int -> ([Int], [(Int, Int)])
nodesAndEdges seed n = (nds, eds)
  where (_, eds, nds) = L.foldl' f (init idx, [], [a]) as
        a:as = [0..n-1]
        idx = reverse $ indices (17*seed - 1) n
        f (i:is, es, acc) s = (is, (s, acc !! i):es , s:acc)

removeCyclesOfTwo xs = S.toList $ L.foldl' f S.empty xs
  where f acc (x, y) = if (not $ S.member (y, x) acc) then S.insert (x, y) acc else acc

-- | Takes a rando seed, a number of nodes and the ratio edges/nodes. 
randomTopology :: Int -> Int -> Double -> Gr NLabel ()
randomTopology _ n ratio | cond = error str  
  where cond = ratio < (1.0 - (1 / fromIntegral n))
        str = "randomTopology: Impossible ratio (" ++ show ratio ++ ") for edges/nodes in a connected graph!"
randomTopology seed n ratio = g
  where g = insEdges edges $ insNodes ns' empty
        (ns, es) = nodesAndEdges seed n
        ns' = map (\x -> (x, NLabel 0 0 x Crossing)) ns

        edgeNumber = truncate $ ratio* (fromIntegral n) - (fromIntegral $ length es)
        xs' = pairs $ map (`mod` n) $ shuffle (19*seed-1) edgeNumber
        es'' = map (\(x, y) -> (x, y, ())) (removeCyclesOfTwo (xs' ++ es))

        edges = filter (\(x, y, _) -> x /= y) $ unique es'' --  (es' ++ es'')


-- | Takes a seed, the length of the value list and a graph.
randomEtaEnv :: Int -> Int -> Gr NLabel () -> EtaMap [Val]
randomEtaEnv seed len g = M.fromList (zip etas rs)
  where etas = concat $ mapGraph f g
        f (_, n, outs) = zipWith (EtaIdx 0 0) (repeat (nodeNLabel n)) (map nodeNLabel outs)
        numOfEtas = length etas
        rs = LHT.sliceHorizontal numOfEtas $ take (numOfEtas*len) (randomRs (0.1, 0.9) (mkStdGen seed))


randomXs :: (RandomGen g) => g -> Int -> [Val]
randomXs gen n = (1 - sum rs):rs
  where rs = map (\(x, _, _) -> x) xs
        xs = tail $ take n $ iterate f (0, 1.0, gen)
        f (_, s, g) = (s', s-s', g')
          where (s', g') = randomR (0, s) g

splitGens :: (RandomGen g) => g -> [g]
splitGens gen = iterate f gen
  where f = fst . split

-- | Takes a seed, the length of the value list and a graph.
randomXEnv :: Int -> Int -> Gr NLabel () -> XMap [Val]
randomXEnv seed len g = M.fromList $ concat zs
  where xs = concat $ mapGraph f g
        f (ins, n, outs) = [ zipWith (XIdx 0 0) (repeat (nodeNLabel n)) (map nodeNLabel outs) ] 

-- zipWith (XIdx 0 0) (repeat (nodeNLabel n)) (map nodeNLabel ins) ] -- , 
        ys = snd $ L.foldl' (h len) (gens, []) xs
        gens = splitGens (mkStdGen seed)
        h len (gs, acc) cs = (bs, (as, cs):acc)
          where (as, bs) = splitAt len gs
        zs = map j ys
        j (gs, cs) = zip cs (transpose ys)
          where ys = map (flip randomXs (length cs)) gs
