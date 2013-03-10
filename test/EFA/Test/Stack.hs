{-# LANGUAGE TemplateHaskell #-}
module EFA.Test.Stack where

import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.MultiValue as MV
import EFA.Equation.Stack (Stack)

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~-), (~*), (~/))

import qualified Test.QuickCheck.Property.Generic as Law
import Test.QuickCheck.Modifiers (Positive, getPositive)
import Test.QuickCheck.All (quickCheckAll)


type IntStack = Stack Char Integer
type IntMultiValue = MV.MultiValue Char Integer

type RatioStack = Stack Char Rational
type PosRatioMultiValue = MV.MultiValue Char (Positive Rational)


prop_multiValueConvert :: IntMultiValue -> Bool
prop_multiValueConvert x =
   x == Stack.toMultiValue (Stack.fromMultiValue x)

prop_multiValuePlus :: IntMultiValue -> IntMultiValue -> Bool
prop_multiValuePlus x y =
   Stack.fromMultiValue (x+y)
   ==
   Stack.fromMultiValue x + Stack.fromMultiValue y

prop_multiValueTimes :: IntMultiValue -> IntMultiValue -> Bool
prop_multiValueTimes x y =
   Stack.fromMultiValue (x*y)
   ==
   Stack.fromMultiValue x * Stack.fromMultiValue y

prop_multiValueNegate :: IntMultiValue -> Bool
prop_multiValueNegate x =
   Stack.fromMultiValue (negate x) == negate (Stack.fromMultiValue x)

prop_multiValueRecip :: PosRatioMultiValue -> Bool
prop_multiValueRecip px =
   case fmap getPositive px of
      x -> Stack.fromMultiValue (recip x) == recip (Stack.fromMultiValue x)


prop_arithmeticPlus :: IntStack -> IntStack -> Bool
prop_arithmeticPlus x y  =  x+y == x~+y

prop_arithmeticMinus :: IntStack -> IntStack -> Bool
prop_arithmeticMinus x y  =  x-y == x~-y

prop_arithmeticNegate :: IntStack -> Bool
prop_arithmeticNegate x  =  negate x == Arith.negate x

prop_arithmeticTimes :: RatioStack -> RatioStack -> Bool
prop_arithmeticTimes x y  =  x*y == x~*y

prop_arithmeticDivide :: RatioStack -> PosRatioMultiValue -> Bool
prop_arithmeticDivide x py =
   case Stack.fromMultiValue $ fmap getPositive py of
      y -> x/y == x~/y

prop_arithmeticRecip :: PosRatioMultiValue -> Bool
prop_arithmeticRecip px =
   case Stack.fromMultiValue $ fmap getPositive px of
      x -> recip x == Arith.recip x


prop_commutativePlus :: IntStack -> IntStack -> Bool
prop_commutativePlus = Law.eq $ Law.prop_Commutative (+) Law.T

prop_commutativeTimes :: IntStack -> IntStack -> Bool
prop_commutativeTimes = Law.eq $ Law.prop_Commutative (*) Law.T

prop_associativePlus :: IntStack -> IntStack -> IntStack -> Bool
prop_associativePlus = Law.eq $ Law.prop_Associative (+) Law.T

prop_associativeTimes :: IntStack -> IntStack -> IntStack -> Bool
prop_associativeTimes = Law.eq $ Law.prop_Associative (*) Law.T

prop_identityPlus :: IntStack -> Bool
prop_identityPlus = Law.eq $ Law.prop_Identity 0 (+) Law.T

prop_identityTimes :: IntStack -> Bool
prop_identityTimes = Law.eq $ Law.prop_Identity 1 (*) Law.T

prop_associativeMinus :: IntStack -> IntStack -> IntStack -> Bool
prop_associativeMinus x y z  =  (x+y)-z == x+(y-z)

prop_swapMinus :: IntStack -> IntStack -> Bool
prop_swapMinus x y  =  (x-y) == negate (y-x)

prop_inversePlus :: IntStack -> Bool
prop_inversePlus =
   Law.eqWith Stack.eqRelaxed $ Law.prop_GroupInverse 0 (+) negate Law.T

prop_distributivePlus :: IntStack -> IntStack -> IntStack -> Bool
prop_distributivePlus x y z  =  (x+y)*z == x*z + y*z

prop_distributiveMinus :: IntStack -> IntStack -> IntStack -> Bool
prop_distributiveMinus x y z  =  (x-y)*z == x*z - y*z


runTests :: IO Bool
runTests = $quickCheckAll
