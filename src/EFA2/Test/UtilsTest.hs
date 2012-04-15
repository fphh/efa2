{-# LANGUAGE TemplateHaskell #-}

module EFA2.Test.UtilsTest where

import qualified Data.Set as S
import qualified Data.List as L

import Test.QuickCheck
import Test.QuickCheck.All


import EFA2.Utils.Utils

-- The difference between 'unique' and (toList . fromList) is
-- that 'unique' does not sort. Therefore, 'unique' has linear
-- running time, whereas (toList . fromList) has n*log n running time.
prop_unique xs = L.sort (unique xs) == S.toList (S.fromList xs)

prop_dmap xs = length xs > 0 ==> yazf (,) xs xs == dmap (,) xs

runTests = $quickCheckAll
