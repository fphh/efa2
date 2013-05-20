

module EFA.CustomTest.EquationSystem.EquationSystem where


import qualified EFA.Example.Absolute as EqGen
import qualified EFA.Graph.Draw as Draw

import EFA.Utility.Async (concurrentlyMany_)


import qualified EFA.CustomTest.EquationSystem.Given as Given


test :: IO ()
test = do
  let env = EqGen.solve Given.seqTopo Given.originalGiven
  putStrLn "Is the equation system for sequence flow graphs correct?"
  print (Given.testEnv == env)


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
