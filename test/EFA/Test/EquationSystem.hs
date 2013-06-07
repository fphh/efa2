

module EFA.Test.EquationSystem where


import qualified EFA.Test.EquationSystem.Given as Given

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Equation.System as EqGen
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Verify as Verify
import qualified EFA.Graph.Draw as Draw

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import EFA.Utility.Async (concurrentlyMany_)

import qualified Control.Monad.Exception.Synchronous as ME

import System.Exit (exitFailure)
import Control.Monad (when)
import Data.Foldable (forM_)


checkException ::
  (ME.Exceptional
     (Verify.Exception Format.Unicode)
     env,
   Verify.Assigns Format.Unicode) ->
  IO env
checkException solution =
  case solution of
    (ME.Success env, _) -> return env
    (ME.Exception (Verify.Exception name lhs rhs), assigns) -> do
      putStrLn $ "conflicting assignments during solution:"
      maybe (return ()) (putStrLn . Format.unUnicode) name
      putStrLn $ Format.unUnicode lhs
      putStrLn $ Format.unUnicode rhs
      putStrLn $ "assignments so far:"
      forM_ assigns $ \(Verify.Assign var val) ->
         putStrLn $ Format.unUnicode $ Format.assign var val
      exitFailure

correctness :: IO ()
correctness = do
  putStrLn "Check correctness of the equation system for sequence flow graphs."
  testEnv <- checkException Given.testEnv
  env <- checkException Given.solvedEnv
  when (testEnv /= env) exitFailure


showDifferences ::
  (Node.C node, FormatValue a, FormatValue v, Eq a, Eq v) =>
  Env.Complete node a v ->
  Env.Complete node a v ->
  IO ()
showDifferences testEnv env = do
  putStrLn "Assignments in expected Env but not in computed one:"
  putStrLn $ Format.unUnicode $ formatValue $
     Env.difference testEnv env

  putStrLn "Assignments in computed Env but not in expected one:"
  putStrLn $ Format.unUnicode $ formatValue $
     Env.difference env testEnv

  putStrLn "Conflicts between computed and expected Env:"
  putStrLn $ Format.unUnicode $ formatValue $
     Env.filter (uncurry (/=)) (uncurry (/=)) $
     Env.intersectionWith (,) (,) testEnv env


consistency :: IO ()
consistency = do
  putStrLn "Check consistency of the equation system for sequence flow graphs."
  env <- fmap Given.numericEnv $ checkException $
    EqGen.solveTracked Given.seqTopo Given.testGiven
  testEnv <- checkException Given.testEnv
  -- showDifferences testEnv env
  when (testEnv /= env) exitFailure

runTests :: IO ()
runTests = do
  correctness
  consistency


main :: IO ()
main = do

  testEnv <- checkException Given.testEnv
  env <- checkException Given.solvedEnv

  showDifferences testEnv env
  putStrLn "These lists should all be empty."
  -- print (testEnv == env)

  concurrentlyMany_ [
    Draw.xterm $
      Draw.title "Aktuell berechnet" $
      Draw.sequFlowGraphAbsWithEnv Given.seqTopo env,
    Draw.xterm $
      Draw.title "Zielvorgabe" $
      Draw.sequFlowGraphAbsWithEnv Given.seqTopo testEnv ]
