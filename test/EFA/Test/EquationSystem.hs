

module EFA.Test.EquationSystem where


import qualified EFA.Test.EquationSystem.Given as Given

import qualified EFA.Example.Absolute as EqGen
import qualified EFA.Graph.Draw as Draw

import EFA.Utility.Async (concurrentlyMany_)

import System.Exit (exitFailure)
import Control.Monad (when)


test :: IO ()
test = do
  putStrLn "Check correctness of the equation system for sequence flow graphs."
  when
     (Given.testEnv /= EqGen.solve Given.seqTopo Given.originalGiven)
     exitFailure


main :: IO ()
main = do

  let env = EqGen.solve Given.seqTopo Given.originalGiven

  print (Given.testEnv == env)

  concurrentlyMany_ [
    Draw.xterm $
      Draw.title "Falsch" $
      Draw.sequFlowGraphAbsWithEnv Given.seqTopo env,
    Draw.xterm $
      Draw.title "Richtig" $
      Draw.sequFlowGraphAbsWithEnv Given.seqTopo Given.testEnv ]
