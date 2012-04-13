{-# LANGUAGE TemplateHaskell #-}

module EFA2.Test.TestUtils where

import qualified Data.Set as S
import Test.QuickCheck
import Test.QuickCheck.All


import EFA2.Utils.Utils

prop_unique xs = unique xs == S.toList (S.fromList xs)

runTests = $quickCheckAll
