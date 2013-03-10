{-# LANGUAGE TemplateHaskell #-}
module EFA.Test.Stack where

import qualified EFA.Equation.Stack as Stack
import EFA.Equation.Stack (Stack)

import qualified Test.QuickCheck.Property.Generic as Law

import Test.QuickCheck.All (quickCheckAll)


type IntStack = Stack Char Integer

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
