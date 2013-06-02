

module EFA.Test.EquationSystem where


import qualified EFA.Test.EquationSystem.Given as Given

import qualified EFA.Equation.Environment as Env
import qualified EFA.Example.Absolute as EqGen
import qualified EFA.Graph.Draw as Draw

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (formatValue)

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

  putStrLn "Assignments in expected Env but not in computed one:"
  putStrLn $ Format.unUnicode $ formatValue $
     Env.difference Given.testEnv env

  putStrLn "Assignments in computed Env but not in expected one:"
  putStrLn $ Format.unUnicode $ formatValue $
     Env.difference env Given.testEnv

  putStrLn "Conflicts between computed and expected Env:"
  putStrLn $ Format.unUnicode $ formatValue $
     Env.filter (uncurry (/=)) (uncurry (/=)) $
     Env.intersectionWith (,) (,) Given.testEnv env

  putStrLn "These lists should all be empty."
  -- print (Given.testEnv == env)

  concurrentlyMany_ [
    Draw.xterm $
      Draw.title "Falsch" $
      Draw.sequFlowGraphAbsWithEnv Given.seqTopo env,
    Draw.xterm $
      Draw.title "Richtig" $
      Draw.sequFlowGraphAbsWithEnv Given.seqTopo Given.testEnv ]
