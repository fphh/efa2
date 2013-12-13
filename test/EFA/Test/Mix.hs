
module EFA.Test.Mix where

import qualified EFA.TestUtility as Test

import qualified EFA.Test.Mix.Given as Given

import qualified EFA.Flow.Topology.EquationSystem as EqSys
import qualified EFA.Flow.Topology.AssignMap as AssignMap
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.RecordIndex as RecIdx
import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Result as Result

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Format (Format)

import EFA.Utility.Async (concurrentlyMany_)

import qualified Control.Monad.Exception.Synchronous as ME

import System.Exit (exitFailure)

import qualified Data.Foldable as Fold
import qualified Data.List.HT as ListHT
import Control.Monad (when)
import Control.Applicative (liftA2)
import Data.Monoid (All(All), getAll)
import Data.Foldable (forM_, foldMap)


completelyDetermined ::
  Given.ResultGraph a -> Bool
completelyDetermined =
  getAll .
  FlowTopo.foldMap (All . Fold.all Result.isDetermined)

undeterminedFromGraph ::
  (Format output) =>
  Given.ResultGraph a -> [output]
undeterminedFromGraph =
  FlowTopo.fold .
  FlowTopo.mapSectionWithVar
    (\var ->
      Fold.fold .
      liftA2
        (\mixIdx ->
          Result.switch [formatValue (RecIdx.Record mixIdx var)] (const []))
        Record.indices)

solveIncomplete ::
  [Given.Equation Verify.Ignore Rational] ->
  Given.ResultGraph Rational
solveIncomplete eqs =
  EqSys.solveOpts Given.multiMixOptions Given.flowGraph
    (foldMap Given.getEquation eqs)


determinateness :: IO ()
determinateness =
  Test.singleIO "Check whether all quantities can be determined." $ do
    let undetermined =
          undeterminedFromGraph $ solveIncomplete Given.partialEquations
        allDetermined = null $ undetermined
    when (not allDetermined) $ do
      putStrLn $ "undetermined variables:\n" ++
        unlines (map Format.unUnicode undetermined)
    return allDetermined

minimalDeterminateness :: IO ()
minimalDeterminateness =
  Test.singleIO "Check whether the set of given equations is minimal." $ do
    let redundantEqs =
          map fst $
          filter (completelyDetermined . solveIncomplete . snd) $
          zip [(0::Int) .. ] $ map snd $
          ListHT.removeEach Given.partialEquations
        minimal = null redundantEqs
    when (not minimal) $ putStrLn $
      "at least one of the following equations is redundant: " ++
      show redundantEqs
    return minimal


checkException ::
  (ME.Exceptional
     (Verify.Exception Format.Unicode)
     graph,
   Verify.Assigns Format.Unicode) ->
  IO graph
checkException solution =
  case solution of
    (ME.Success graph, _) -> return graph
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
  fullGraph <- checkException Given.fullGraph
  solvedGraph <- checkException Given.solvedGraph
  return $ fullGraph == solvedGraph


showDifferences ::
  (Node.C node, FormatValue a, Eq a) =>
  FlowTopo.Section node a ->
  FlowTopo.Section node a ->
  IO ()
showDifferences fullGraph solvedGraph = do
  let fullAM   = FlowTopo.toAssignMap fullGraph
  let solvedAM = FlowTopo.toAssignMap solvedGraph
  putStrLn "Expected assignments that are not computed:"
  putStrLn $ Format.unUnicode $ Format.lines $ AssignMap.format $
     AssignMap.difference fullAM solvedAM

  putStrLn "Computed assignments that are not expected:"
  putStrLn $ Format.unUnicode $ Format.lines $ AssignMap.format $
     AssignMap.difference solvedAM fullAM

  putStrLn "Conflicts between expected and computed assignments:"
  putStrLn $ Format.unUnicode $ Format.lines $ AssignMap.format $
     AssignMap.filter (uncurry (/=)) $
     AssignMap.intersectionWith (,) fullAM solvedAM


consistency :: IO ()
consistency =
  Test.singleIO "Check consistency of the equation system for sequence flow graphs." $ do
  graph <- fmap Given.numericGraph $ checkException $
    EqSys.solveTrackedOpts Given.multiMixOptions Given.flowGraph Given.fullGiven
  fullGraph <- checkException Given.fullGraph
  return $ fullGraph == graph


runTests :: IO ()
runTests = do
  putStrLn "Mix equation system tests"
  determinateness
  minimalDeterminateness
  correctness
  consistency
  putStrLn ""


main :: IO ()
main = do

  fullGraph <- checkException Given.fullGraph
  solvedGraph <- checkException Given.solvedGraph

  showDifferences fullGraph solvedGraph
  putStrLn "These lists should all be empty."

  concurrentlyMany_ [
    Draw.xterm $
      Draw.title "Aktuell berechnet" $
      Draw.flowSection Draw.optionsDefault solvedGraph,
    Draw.xterm $
      Draw.title "Zielvorgabe" $
      Draw.flowSection Draw.optionsDefault fullGraph ]
