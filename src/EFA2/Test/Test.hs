
module EFA2.Test.Test where


import qualified EFA2.Test.TestUtils as TU
import qualified EFA2.Test.SolverTest as ST



main :: IO ()
main = do
  -- TU.runTests
  ST.runTests
  return ()