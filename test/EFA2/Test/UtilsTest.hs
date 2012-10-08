{-# LANGUAGE TemplateHaskell #-}

module EFA2.Test.UtilsTest where

import EFA2.Utils.Utils (pairs, yazf)

import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)


prop_dmap :: Eq a => [a] -> Property
prop_dmap xs = length xs > 0 ==> yazf (,) xs xs == pairs xs

runTests :: IO Bool
runTests = $quickCheckAll
