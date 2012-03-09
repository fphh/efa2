{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GeneralizedNewtypeDeriving, FlexibleContexts, NoMonomorphismRestriction, FunctionalDependencies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Graph.CGraph
-- Copyright   :  (c) Dr. Heinrich Hördegen
-- 
-- Maintainer  : hoerdegen@funktional.info
--
-----------------------------------------------------------------------------


module EFA2.Signal.SignalGeneration where


import qualified Data.List as L

import qualified Data.Vector.Fusion.Stream as S
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV

import System.Random

import EFA2.Signal.SignalData


type Stream = S.Stream

data Base = Base !Index !Time

newtype Time = Time { unTime :: Double } deriving (Show, Eq, Num, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

newtype DTime =  DTime { unDTime :: Double } deriving (Show, Eq, Num, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)


start :: Int
start = 0

sampleSize :: Int
sampleSize = 100

stepSize :: Time
stepSize = Time 0.1

defaultTimeLine :: Stream Time
defaultTimeLine = timeLine (fromIntegral start) (fromIntegral sampleSize) stepSize

defaultIndexLine :: Stream Index
defaultIndexLine = S.unfoldrN sampleSize (\x -> Just (Index x, x+1)) start

timeLine :: Time -> Int -> Time -> Stream Time
timeLine from n step = S.iterateN n (+step) from

xaxis = S.zipWith Base defaultIndexLine defaultTimeLine

makeDefaultSignal :: (GV.Vector UV.Vector a) => (Base -> a) -> UV.Vector a
makeDefaultSignal f = GV.unstream $ S.map f xaxis

constant :: (GV.Vector UV.Vector a, Sample a) => Double -> UV.Vector a
constant x =  makeDefaultSignal (\_ -> toSample x)

line :: (GV.Vector UV.Vector a, Sample a) => Double -> Double -> UV.Vector a
line m c = makeDefaultSignal (\(Base _ (Time x)) -> toSample $ m*x + c)

parallelLines :: (GV.Vector UV.Vector a, Sample a) => Double -> Double -> Double -> [UV.Vector a]
parallelLines m c c2 = map (line m) [c + x | x <- (map (c2*) [0..])]

plines :: (GV.Vector UV.Vector a, Sample a) => Double -> [UV.Vector a]
plines m = parallelLines m 2 1

neg_plines :: (GV.Vector UV.Vector a, Sample a) => Double -> [UV.Vector a]
neg_plines m = parallelLines m (-2) (-1)

sinus :: (GV.Vector UV.Vector a, Sample a) => Double -> Double -> UV.Vector a
sinus a m = makeDefaultSignal (\(Base _ (Time x)) -> toSample $ sin (m*x + a))

cosinus :: (GV.Vector UV.Vector a, Sample a) => Double -> Double -> UV.Vector a
cosinus a m = makeDefaultSignal (\(Base _ (Time x)) -> toSample $ cos (m*x + a))

root :: (GV.Vector UV.Vector a, Sample a) => Double -> Double -> UV.Vector a
root a m = makeDefaultSignal (\(Base _ (Time x)) -> toSample $ sqrt (m*x + a))


repeatMany :: (UV.Unbox a) => [UV.Vector a] -> UV.Vector a
repeatMany xs = UV.take sampleSize $ go t
  where xs' = UV.concat xs
        len = UV.length xs'
        t = ceiling (fromIntegral sampleSize / fromIntegral len)
        go 0 = UV.empty
        go n = xs' UV.++ go (n-1)

repeatOne :: (UV.Unbox a) =>  UV.Vector a -> UV.Vector a
repeatOne s = repeatMany [s]

repeatTwo :: (UV.Unbox a) => UV.Vector a -> UV.Vector a -> UV.Vector a
repeatTwo s t = repeatMany [s, t]

randomWalk :: (UV.Unbox a, Sample a) => Int -> Double -> UV.Vector a
randomWalk seed step = UV.reverse $ UV.map toSample $ UV.fromList $ fst rs'
  where rs :: [Int]
        rs = take sampleSize $ randomRs (0, 1) (mkStdGen seed)
        rs' = foldl f ([], 0) rs
        f (xs, acc) 0 = (acc:xs, acc - step)
        f (xs, acc) 1 = (acc:xs, acc + step)

infixl 7  .*, ./
infixl 6  .+, .-

(.+) :: (UV.Unbox a, Num a) => UV.Vector a -> UV.Vector a -> UV.Vector a
(.+) = UV.zipWith (+)

(.-) :: (UV.Unbox a, Num a) => UV.Vector a -> UV.Vector a -> UV.Vector a
(.-) = UV.zipWith (-)

(.*) :: (UV.Unbox a, Num a) => UV.Vector a -> UV.Vector a -> UV.Vector a
(.*) = UV.zipWith (*)

(./) :: (UV.Unbox a, Num a, Fractional a) => UV.Vector a -> UV.Vector a -> UV.Vector a
(./) = UV.zipWith (/)

delta :: (UV.Unbox a, Num a) => UV.Vector a -> UV.Vector a -> UV.Vector a
delta = (.-)

signm :: (UV.Unbox a, Num a) => UV.Vector a -> UV.Vector a
signm = UV.map (signum)

etaFunct :: (UV.Unbox a , Sample a) => Double -> UV.Vector a -> UV.Vector a
etaFunct eta = UV.map (toSample . f . fromSample)
  where f val | val > 0  = val*eta
        f val | val == 0 = 0   
        f val | val < 0  = val/eta  

hasOnlyOneSign :: (UV.Unbox a, Ord a, Num a) => UV.Vector a -> Bool
hasOnlyOneSign xs = length (filter (== True) [allEQ xs, allGT xs, allLT xs]) == 1
  where allEQ = UV.all (== 0)
        allGT = UV.all (> 0)
        allLT = UV.all (< 0)

