module Main where

-- module EFA.Test.Test where


import qualified EFA.Test.UtilsTest as UT
import qualified EFA.Test.SequenceTest as SeqT
-- import qualified EFA.Test.SolverTest as ST
-- import qualified EFA.Test.IsVarTest as IVT

import Control.Functor.HT (void)


main :: IO ()
main = do
   void $ UT.runTests
   void $ SeqT.runTests
   -- void $ ST.runTests
   -- void $ IVT.runTests
