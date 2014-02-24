module Main where

import qualified EFA.Test.StateAnalysis as StateAnalysis
import qualified EFA.Test.MultiValue as MultiValue
import qualified EFA.Test.Stack as Stack
import qualified EFA.Test.Utility as Util
import qualified EFA.Test.Sequence as Seq
import qualified EFA.Test.Signal as Sig

import qualified EFA.Test.Mix as Mix
import qualified EFA.Test.Cumulated as Cumulated
import qualified EFA.Test.EquationSystem as EqSys
import qualified EFA.Test.Tree as Tree
import qualified EFA.Test.FindBestIndex as FindBestIndex


import Control.Functor.HT (void)


main :: IO ()
main = do
   void $ StateAnalysis.runTests
   void $ MultiValue.runTests
   void $ Stack.runTests
   void $ Util.runTests
   void $ Seq.runTests
   void $ Sig.runTests
   void $ FindBestIndex.runTests
   EqSys.runTests
   Cumulated.runTests
   Mix.runTests
   Tree.runTests
