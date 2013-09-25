

module EFA.Test.EquationSystem where

import qualified EFA.TestUtility as Test

import qualified EFA.Test.EquationSystem.Given as Given

import qualified EFA.Flow.Sequence.Absolute as EqSys
import qualified EFA.Flow.Sequence.AssignMap as AssignMap
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Verify as Verify

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue)

import EFA.Utility.Async (concurrentlyMany_)

import qualified Control.Monad.Exception.Synchronous as ME

import System.Exit (exitFailure)
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
      printAssignments assigns
      exitFailure


printAssignments ::
  Verify.Assigns Format.Unicode -> IO ()
printAssignments assigns =
  forM_ assigns $ \(Verify.Assign var val) ->
    putStrLn $ Format.unUnicode $ Format.assign var val

correctness :: IO ()
correctness =
  Test.singleIO "Check correctness of the equation system for sequence flow graphs." $ do
  testEnv <- checkException Given.testEnv
  env <- checkException Given.solvedEnv
  return $ testEnv == env


showDifferences ::
  (Node.C node, FormatValue a, FormatValue v, Eq a, Eq v) =>
  SeqFlow.Graph node a v ->
  SeqFlow.Graph node a v ->
  IO ()
showDifferences testEnv env = do
  let testAM = SeqFlow.toAssignMap testEnv
  let am     = SeqFlow.toAssignMap env
  putStrLn "Expected assignments that are not computed:"
  putStrLn $ Format.unUnicode $ Format.lines $ AssignMap.format $
     AssignMap.difference testAM am

  putStrLn "Computed assignments that are not expected:"
  putStrLn $ Format.unUnicode $ Format.lines $ AssignMap.format $
     AssignMap.difference am testAM

  putStrLn "Conflicts between expected and computed assignments:"
  putStrLn $ Format.unUnicode $ Format.lines $ AssignMap.format $
     AssignMap.filter (uncurry (/=)) (uncurry (/=)) $
     AssignMap.intersectionWith (,) (,) testAM am


consistency :: IO ()
consistency =
  Test.singleIO "Check consistency of the equation system for sequence flow graphs." $ do
  env <- fmap Given.numericEnv $ checkException $
    EqSys.solveTracked Given.flowGraph Given.testGiven
  testEnv <- checkException Given.testEnv
  -- showDifferences testEnv env
  return $ testEnv == env

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
      Draw.seqFlowGraph Draw.optionsDefault env,
    Draw.xterm $
      Draw.title "Zielvorgabe" $
      Draw.seqFlowGraph Draw.optionsDefault testEnv ]
