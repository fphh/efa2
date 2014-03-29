{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}


module EFA.Test.Sweep where

import qualified EFA.Application.Optimisation.Sweep as Sweep
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~-), (~*), (~/))

import EFA.TestUtility (Func(Func))

import Test.QuickCheck.All (quickCheckAll)

import qualified Data.Vector.Unboxed as UV



--------------------------------------------------------------


type Sweep = Sweep.Sweep UV.Vector Double

--------------------------------------------------------------

eps :: Double
eps = 10^^(-9 :: Integer)

(===) :: Sweep -> Sweep -> Bool
(Sweep.Sweep xs) === (Sweep.Sweep ys) =
  UV.all (< eps) (UV.zipWith (\x y -> abs (x-y)) xs ys)

(====) :: Double -> Double -> Bool
x ==== y = abs (x - y) < eps


--------------------------------------------------------------

prop_length :: [Double] -> Bool
prop_length xs =
  length xs == Sweep.length (Sweep.fromList xs :: Sweep)


prop_map_fusion ::
  (Func Double) ->
  (Func Double) ->
  Sweep ->
  Bool
prop_map_fusion (Func f) (Func g) sweep =
  Sweep.map f (Sweep.map g sweep) == Sweep.map (f . g) sweep

prop_conversion :: Sweep -> Bool
prop_conversion sweep = Sweep.fromList (Sweep.toList sweep) == sweep

prop_conversion2 :: [Double] -> Bool
prop_conversion2 xs = Sweep.toList (Sweep.fromList xs :: Sweep) == xs

prop_recip :: Sweep -> Bool
prop_recip sweep = Arith.recip (Arith.recip sweep) === sweep

prop_negate :: Sweep -> Bool
prop_negate sweep = Arith.negate (Arith.negate sweep) == sweep

prop_integrate :: Sweep -> Bool
prop_integrate sweep = Arith.integrate sweep == sweep


prop_allZeros :: Sweep -> Bool
prop_allZeros sweep =
  if Arith.allZeros sweep then sum (Sweep.toList sweep) == 0 else True


{-
prop_allZeros2 :: Sweep -> QC.Property
prop_allZeros2 sweep =
  (Arith.allZeros sweep)
  ==>
  (sum (Sweep.toList sweep) == 0)
-}

prop_constOne :: Sweep -> Bool
prop_constOne sweep =
  sum (Sweep.toList (Arith.constOne sweep)) == fromIntegral (Sweep.length sweep)


prop_replicate :: Sweep -> Double -> Bool
prop_replicate sweep x =
  sum (Sweep.toList (Sweep.replicate sweep x))
  ====
  (fromIntegral (Sweep.length sweep) * x)


prop_fromDouble_replicate :: Sweep -> Double -> Bool
prop_fromDouble_replicate sweep x =
  Sweep.replicate sweep x == Sweep.fromRational (Sweep.length sweep) x


prop_add :: Sweep -> Bool
prop_add sweep = sweep ~+ Arith.negate sweep == sweep ~- sweep

prop_mul :: Sweep -> Bool
prop_mul sweep = (sweep ~* Arith.recip sweep) === (sweep ~/ sweep)

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO ()
main = runTests >>= print
