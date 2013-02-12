{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}


module Genetic where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Function
import System.Random
import Data.Random
import Control.Parallel.Strategies


type Seed = Int
type Gen = Int
type RandInt = Int
type Percent = Double


class Genetic term var where
      distance :: term var -> Double
      mkInitGeneration :: IO [term var]
      crossover :: (RandInt, term var) -> (RandInt, term var) -> [term var]

      mkGenerations :: Seed -> Gen -> [term var] -> [term var]
      mkGenerations = mkGenerations' 0.1



chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as:(chunk n bs)
  where (as,bs) = splitAt n xs

sort :: (Genetic term var) => [term var] -> [term var]
sort ts = map snd $ L.sortBy (compare `on` fst) $ (zip gs' ts)
  where gs = chunk 100 ts
        gs' = concat $ parMap rdeepseq (map distance) gs

generate ::
  (Genetic term var) =>
  Percent -> ([Int], [term var]) -> ([Int], [term var])
generate perc _ | perc <= 0 || perc > 1 =
  error "generate needs a percent value in (0, 1]"
generate perc (rs0:rs1:rs2:rs, ts) = (rs, res)
  where n = floor $ perc * (fromIntegral $ length ts)
        fittest = take n $ sort ts
        fm = M.fromList (zip [0..] fittest)
        rs0' = randData rs0
        rs1' = randData rs1
        rs2' = randData rs2
        xs = zip fittest (chunk 5 (zip3 rs0' rs1' rs2'))
        res = concatMap f xs
        f (eq, lst) = concatMap (g eq) lst
        g eq0 (a, b, c) = crossover (b, eq0) (c, eq1)
          where eq1 = fm M.! (a `mod` n)
generate _ _ = error " "

mkGenerations' ::
  (Genetic term var) =>
  Percent -> Seed -> Gen -> [term var] -> [term var]
mkGenerations' perc seed gen ts = sort (snd (ts' !! gen))
  where rs = randData seed
        ts' = iterate (generate perc) (rs, ts)

-- Helper


rollSeed :: IO Int
rollSeed = getStdRandom $ randomR (0, maxBound :: Int)

randData :: Int -> [Int]
randData seed = randomRs (0, maxBound) (mkStdGen seed)


shuf :: Int -> IO [Int]
shuf x = runRVar (shuffleN x [0..x-1]) StdRandom
