{-# LANGUAGE TemplateHaskell #-}
module EFA.Test.Stack where

import qualified EFA.Equation.Stack as Stack
import EFA.Equation.Stack (Stack)

import qualified Test.QuickCheck.Property.Generic as Law

import Test.QuickCheck.All (quickCheckAll)


type RatioStack = Stack Char Rational

prop_commutativePlus :: RatioStack -> RatioStack -> Bool
prop_commutativePlus = Law.eq $ Law.prop_Commutative (+) Law.T

prop_commutativeTimes :: RatioStack -> RatioStack -> Bool
prop_commutativeTimes = Law.eq $ Law.prop_Commutative (*) Law.T

prop_associativePlus :: RatioStack -> RatioStack -> RatioStack -> Bool
prop_associativePlus = Law.eq $ Law.prop_Associative (+) Law.T

prop_associativeTimes :: RatioStack -> RatioStack -> RatioStack -> Bool
prop_associativeTimes = Law.eq $ Law.prop_Associative (*) Law.T

prop_identityPlus :: RatioStack -> Bool
prop_identityPlus = Law.eq $ Law.prop_Identity 0 (+) Law.T

prop_identityTimes :: RatioStack -> Bool
prop_identityTimes = Law.eq $ Law.prop_Identity 1 (*) Law.T

prop_associativeMinus :: RatioStack -> RatioStack -> RatioStack -> Bool
prop_associativeMinus x y z  =  (x+y)-z == x+(y-z)

prop_swapMinus :: RatioStack -> RatioStack -> Bool
prop_swapMinus x y  =  (x-y) == negate (y-x)

prop_inversePlus :: RatioStack -> Bool
prop_inversePlus =
   Law.eqWith Stack.eqRelaxed $ Law.prop_GroupInverse 0 (+) negate Law.T

prop_distributivePlus :: RatioStack -> RatioStack -> RatioStack -> Bool
prop_distributivePlus x y z  =  (x+y)*z == x*z + y*z

prop_distributiveMinus :: RatioStack -> RatioStack -> RatioStack -> Bool
prop_distributiveMinus x y z  =  (x-y)*z == x*z - y*z


runTests :: IO Bool
runTests = $quickCheckAll
