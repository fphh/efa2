
module EFA.Test.Cumulated where

import qualified EFA.TestUtility as Test

import qualified EFA.Test.Cumulated.Given as Given

import qualified EFA.Flow.Cumulated.Absolute as EqSys
import qualified EFA.Flow.Cumulated.AssignMap as AssignMap
import qualified EFA.Flow.Cumulated.Quantity as CumFlow
import qualified EFA.Flow.Draw as Draw

import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Graph as Graph

import qualified EFA.Equation.Verify as Verify
import qualified EFA.Equation.Result as Result
import EFA.Equation.Result (Result(Undetermined))

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue, formatAssign)
import EFA.Report.Format (Format)

import EFA.Utility.Async (concurrentlyMany_)
import EFA.Utility (mapDiagonal)

import qualified Control.Monad.Exception.Synchronous as ME

import System.Exit (exitFailure)

import qualified Data.Foldable as Fold
import Control.Monad (when)
import Control.Applicative (pure)
import Data.Traversable (Traversable, traverse, fmapDefault, foldMapDefault)
import Data.Foldable (Foldable, forM_, foldMap)
import Data.Monoid (All(All), getAll, mempty)


completelyDetermined ::
  Node.C node =>
  CumFlow.Graph node (Result a) -> Bool
completelyDetermined =
  getAll .
  CumFlow.foldMap (All . Result.isDetermined)

undeterminedFromGraph ::
  (Node.C node, Format output) =>
  CumFlow.Graph node (Result a) ->
  [output]
undeterminedFromGraph =
  CumFlow.fold .
  CumFlow.mapGraphWithVar
    (\var -> Result.switch [formatValue var] (const []))

determinateness :: IO ()
determinateness =
  Test.singleIO "Check whether all quantities can be determined." $ do
    let undetermined =
          undeterminedFromGraph $ EqSys.solve Given.flowGraphRatio mempty
        allDetermined = null $ undetermined
    when (not allDetermined) $ do
      putStrLn $ "undetermined variables:\n" ++
        unlines (map Format.unUnicode undetermined)
    return allDetermined


newtype
   WrapCumGraph node a =
      WrapCumGraph {
         unwrapCumGraph :: Graph.Graph node Graph.DirEdge () (CumFlow.Cum a)
      }

instance (Node.C node) => Functor (WrapCumGraph node) where
  fmap = fmapDefault

instance (Node.C node) => Foldable (WrapCumGraph node) where
  foldMap = foldMapDefault

instance (Node.C node) => Traversable (WrapCumGraph node) where
  traverse f (WrapCumGraph gr) =
    fmap WrapCumGraph $ Graph.traverse pure (traverse f) gr

minimalDeterminateness :: IO ()
minimalDeterminateness =
  Test.singleIO
    ("The set of given equations is not minimal " ++
     "and we cannot make it minimal in an easy way.") $ do
    let edgeGraph = Graph.mapNode (const ()) Given.cumGraph
        redundantEqs =
          map fst $
          filter (completelyDetermined . (\gr -> EqSys.solve gr mempty) . snd) $
          zip (Fold.toList $ WrapCumGraph $
               Graph.mapEdgeWithKey (CumFlow.mapCumWithVar formatAssign) edgeGraph) $
          map (Graph.mapNode (\() -> pure Undetermined) .
               Graph.mapEdge CumFlow.flowResultFromCumResult .
               unwrapCumGraph) $
          mapDiagonal id (const Undetermined) $
          WrapCumGraph edgeGraph
        minimal = null redundantEqs
    when (not minimal) $ putStrLn $
      "at least one of the following equations is redundant:\n" ++
      (Format.unUnicode $ Format.lines redundantEqs)
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
  CumFlow.Graph node a ->
  CumFlow.Graph node a ->
  IO ()
showDifferences fullGraph solvedGraph = do
  let fullAM   = CumFlow.toAssignMap fullGraph
  let solvedAM = CumFlow.toAssignMap solvedGraph
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
    EqSys.solveTracked Given.flowGraph Given.fullGiven
  fullGraph <- checkException Given.fullGraph
  return $ fullGraph == graph


runTests :: IO ()
runTests = do
  putStrLn "Cumulated graph equation system tests"
  determinateness
  when False minimalDeterminateness
  correctness
  consistency
  putStrLn ""


main :: IO ()
main = do
  Draw.xterm $
    Draw.title "Unchecked computation" $
    Draw.cumulatedFlow $
    EqSys.solve Given.flowGraphRatio mempty

  mapM_ (putStrLn . Format.unASCII) $ AssignMap.format .
    CumFlow.toAssignMap $
    EqSys.solve Given.flowGraphRatio mempty

  fullGraph <- checkException Given.fullGraph
  solvedGraph <- checkException Given.solvedGraph

  showDifferences fullGraph solvedGraph
  putStrLn "These lists should all be empty."

  concurrentlyMany_ [
    Draw.xterm $
      Draw.title "Aktuell berechnet" $
      Draw.cumulatedFlow solvedGraph,
    Draw.xterm $
      Draw.title "Zielvorgabe" $
      Draw.cumulatedFlow fullGraph ]
