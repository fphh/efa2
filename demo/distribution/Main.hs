{-# LANGUAGE TypeOperators #-}

module Main where

import EFA.Signal.Signal hiding (map,foldl,zip,filter)
import qualified EFA.Signal.Signal as S (map,foldl,zip,filter)

import qualified EFA.Signal.Vector as V
import EFA.Signal.Data (Data(..),(:>),Nil)
import EFA.Signal.Base (BSum)


import qualified EFA.Signal.Plot as Plot


import qualified Data.List as L
import qualified Data.Map as M

-- | Without Typed Container First

newtype Class a = Class a deriving (Show,Ord,Eq)


classifyEven' :: Double -> Double -> Double -> Class Double
classifyEven' interval offset x = Class (fromIntegral(round((x+offset)/interval))*interval-offset)


classifySignal ::  (V.Walker v,
                      V.Storage v a,
                      V.Storage v (Class a)) =>
                   (a -> Class a) -> UTSignal v a  -> UTSignal v (Class a)
classifySignal f sig = S.map f sig


s1 = fromList [2,17,5,5,20] :: UTSigL
s2 = fromList [0,10,18,18,12] :: UTSigL
s3 = fromList [3,12,7,7,19] :: UTSigL

e1 = fromList [10,20,100,50,50] :: FFSigL

ks1 :: UTSignal [] (Class Double)
ks1 = classifySignal (classifyEven' 10 0) s1


ks2 :: UTSignal [] (Class Double)
ks2 = classifySignal (classifyEven' 10 0) s2

ks3 :: UTSignal [] (Class Double)
ks3 = classifySignal (classifyEven' 10 0) s3

generateDistribution :: (V.Storage v ([Class a],[Int]),
                V.Storage v (Class a),
                V.FromList v,
                Ord (Class a)) =>
               UTSignal v (Class a) -> UTDistr v ([Class a], [Int])
generateDistribution classSig = fromList (map f classes)
  where list = toList classSig
        classes = map fst $ M.toList $ M.fromList $ zip list list
        f clss = ([clss], L.findIndices (\x -> clss == x) list)


d1 = generateDistribution ks1
d2 = generateDistribution ks2
d3 = generateDistribution ks3


-- | to combine Distributions
combineDistributions :: (V.Storage v ([Class a], [Int]),
                         V.FromList v,V.Filter v) =>
                        [UTDistr v ([Class a], [Int])] -> UTDistr v ([Class a],[Int])
combineDistributions [] =  error("Error - empty list in combineDistributions")
combineDistributions (d:[]) = d
combineDistributions (d:ds) = foldl f d ds
  where f acc d = S.filter h $ combineWith g acc d
        g (classes1,indices1) (classes2,indices2) = (classes1++classes2,L.intersect indices1 indices2)
        h (_,[]) = False
        h (_,_) = True
        -- vorher Kombination aller Klassen


combineWith :: (V.Storage v d3,
                V.FromList v,
                V.Storage v d1,
                V.Storage v d2) =>
               (d1 -> d2 -> d3) -> TC s t (Data (v :> Nil) d1) -> TC s t (Data (v :> Nil) d2) ->  TC s t (Data (v :> Nil) d3)
combineWith f xs ys = fromList [ f x y | x <- (toList xs), y <- (toList ys)]


d4 = combineDistributions [d1, d2, d3]


calcDistributionValues :: (Num a,
                           V.Walker v,
                           V.Storage v ([Class a], [Int]),
                           Eq a,
                           V.Storage v a,
                           V.Lookup v,
                           BSum a) =>
                          UTDistr v ([Class a],[Int]) -> FFSignal v a -> FDistr v a
calcDistributionValues d s = setType $ S.map f d
  where f (classes, indices) = fromScalar $ sigSum $ subSignal1D s indices


df4 = calcDistributionValues d4 e1

main :: IO ()
main = do

  print $ classifyEven' 2 1 3.3
  print  ks1
  print  ks2
  print  d1
  print  d2
  print  d3
  print  d4
  print df4


