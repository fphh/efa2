
module EFA2.Test.Test where


import qualified EFA2.Test.UtilsTest as UT
import qualified EFA2.Test.SolverTest as ST
import qualified EFA2.Test.IsVarTest as IVT



main :: IO ()
main = do
  UT.runTests
  ST.runTests
  IVT.runTests
  return ()