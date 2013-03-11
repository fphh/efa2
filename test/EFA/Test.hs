module Main where

import qualified EFA.Test.MultiValue as MultiValue
import qualified EFA.Test.Stack as Stack
import qualified EFA.Test.Utility as Util
import qualified EFA.Test.Sequence as Seq
-- import qualified EFA.Test.SolverTest as ST
-- import qualified EFA.Test.IsVarTest as IVT

import Control.Functor.HT (void)


main :: IO ()
main = do
   void $ MultiValue.runTests
   void $ Stack.runTests
   void $ Util.runTests
   void $ Seq.runTests
   -- void $ ST.runTests
   -- void $ IVT.runTests
