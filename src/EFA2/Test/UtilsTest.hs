{-# LANGUAGE TemplateHaskell #-}

module EFA2.Test.UtilsTest where

import qualified Data.Set as S
import qualified Data.List as L

import EFA2.Utils.Utils (pairs, yazf, unique)

import Test.QuickCheck (Property, (==>))
import Test.QuickCheck.All (quickCheckAll)


-- The difference between 'unique' and (toList . fromList) is
-- that 'unique' does not sort. Therefore, 'unique' has linear
-- running time, whereas (toList . fromList) has n*log n running time.
prop_unique :: Ord a => [a] -> Bool
prop_unique xs = L.sort (unique xs) == S.toList (S.fromList xs)

prop_dmap :: Eq a => [a] -> Property
prop_dmap xs = length xs > 0 ==> yazf (,) xs xs == pairs xs

runTests :: IO Bool
runTests = $quickCheckAll
