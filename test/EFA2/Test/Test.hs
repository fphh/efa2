module Main where

-- module EFA2.Test.Test where


import qualified EFA2.Test.UtilsTest as UT
import qualified EFA2.Test.SequenceTest as SeqT
-- import qualified EFA2.Test.SolverTest as ST
-- import qualified EFA2.Test.IsVarTest as IVT

import Control.Functor.HT (void)


main :: IO ()
main = do
   void $ UT.runTests
   void $ SeqT.runTests
   -- void $ ST.runTests
   -- void $ IVT.runTests
