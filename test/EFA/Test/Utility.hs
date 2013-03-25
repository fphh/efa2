{-# LANGUAGE TemplateHaskell #-}

module EFA.Test.Utility where

import EFA.Utility (pairs, yazf)

import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)


prop_dmap :: Eq a => [a] -> Property
prop_dmap xs = length xs > 0 ==> yazf (,) xs xs == pairs xs

runTests :: IO Bool
runTests = $quickCheckAll
