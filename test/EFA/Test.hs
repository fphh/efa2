module Main where

import qualified EFA.Test.MultiValue as MultiValue
import qualified EFA.Test.Stack as Stack
import qualified EFA.Test.Utility as Util
import qualified EFA.Test.Sequence as Seq

import Control.Functor.HT (void)


main :: IO ()
main = do
   void $ MultiValue.runTests
   void $ Stack.runTests
   void $ Util.runTests
   void $ Seq.runTests
