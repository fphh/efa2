{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}


module Main where

import qualified EFA.Signal.Signal as S
import EFA.Signal.Signal (Class, SignalIdx, classifyEven)


-- | Without Typed Container First


s1, s2, s3 :: S.UTFSignal [] Double
s1 = S.fromList [2,17,5,5,20]
s2 = S.fromList [0,10,18,18,12]
s3 = S.fromList [3,12,7,7,19]

e1 :: S.FFSignal [] Double
e1 = S.fromList [10,20,100,50,50]

ks1 :: S.UTFSignal [] (Class Double)
ks1 = S.map (classifyEven 10 0) s1

ks2 :: S.UTFSignal [] (Class Double)
ks2 = S.map (classifyEven 10 0) s2

ks3 :: S.UTFSignal [] (Class Double)
ks3 = S.map (classifyEven 10 0) s3

d1, d2, d3, d4 :: S.UTDistr [] ([Class Double], [SignalIdx])
d1 = S.etaDistribution1D (classifyEven 10 0) s1
d2 = S.etaDistribution1D (classifyEven 10 0) s2
d3 = S.etaDistribution1D (classifyEven 10 0) s3


-- d4 = S.combineDistributions [d1, d2, d3]
d4 = S.combineSupportPoints [d1, d2, d3]

df4 :: S.FDistr [] Double
df4 = S.calcDistributionValues d4 e1

main :: IO ()
main = do

  print $ classifyEven 2 1 (3.3::Double)
  print  ks1
  print  ks2
  print  ks3
  print  d1
  print  d2
  print  d3
  print  d4
  print df4


