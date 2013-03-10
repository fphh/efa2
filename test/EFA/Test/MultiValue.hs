{-# LANGUAGE TemplateHaskell #-}
module EFA.Test.MultiValue where

import qualified EFA.Equation.MultiValue as MultiValue
import EFA.Equation.MultiValue (MultiValue)

import qualified Test.QuickCheck.Property.Generic as Law

import Test.QuickCheck.All (quickCheckAll)


type IntMultiValue = MultiValue Char Integer

prop_commutativePlus :: IntMultiValue -> IntMultiValue -> Bool
prop_commutativePlus = Law.eq $ Law.prop_Commutative (+) Law.T

prop_commutativeTimes :: IntMultiValue -> IntMultiValue -> Bool
prop_commutativeTimes = Law.eq $ Law.prop_Commutative (*) Law.T

prop_associativePlus :: IntMultiValue -> IntMultiValue -> IntMultiValue -> Bool
prop_associativePlus = Law.eq $ Law.prop_Associative (+) Law.T

prop_associativeTimes :: IntMultiValue -> IntMultiValue -> IntMultiValue -> Bool
prop_associativeTimes = Law.eq $ Law.prop_Associative (*) Law.T

prop_identityPlus :: IntMultiValue -> Bool
prop_identityPlus = Law.eq $ Law.prop_Identity 0 (+) Law.T

prop_identityTimes :: IntMultiValue -> Bool
prop_identityTimes = Law.eq $ Law.prop_Identity 1 (*) Law.T

prop_associativeMinus :: IntMultiValue -> IntMultiValue -> IntMultiValue -> Bool
prop_associativeMinus x y z  =  (x+y)-z == x+(y-z)

prop_swapMinus :: IntMultiValue -> IntMultiValue -> Bool
prop_swapMinus x y  =  (x-y) == negate (y-x)

prop_inversePlus :: IntMultiValue -> Bool
prop_inversePlus =
   Law.eqWith MultiValue.eqRelaxed $ Law.prop_GroupInverse 0 (+) negate Law.T

prop_distributivePlus :: IntMultiValue -> IntMultiValue -> IntMultiValue -> Bool
prop_distributivePlus x y z  =  (x+y)*z == x*z + y*z

prop_distributiveMinus :: IntMultiValue -> IntMultiValue -> IntMultiValue -> Bool
prop_distributiveMinus x y z  =  (x-y)*z == x*z - y*z


runTests :: IO Bool
runTests = $quickCheckAll
