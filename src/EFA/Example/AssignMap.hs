{-# LANGUAGE TypeFamilies #-}

module EFA.Example.AssignMap where

import qualified EFA.Equation.Result as Result
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Stack as Stack
import qualified EFA.Report.Format as Format
import qualified EFA.Graph.Topology.Index as Idx
--import qualified EFA.Equation.Stack as Stack
import qualified EFA.Equation.Environment as Env
import qualified EFA.Equation.Record as EqRecord
--import qualified EFA.Equation.Result as Result
--import qualified EFA.Equation.Variable as Var
-- import qualified EFA.Graph.Topology.Index as Idx
-- import qualified EFA.Graph.Topology.Node as TDNode

import EFA.Equation.Result (Result)
import EFA.Equation.Arithmetic ((~+))
import EFA.Report.FormatValue (FormatValue, formatValue)
import EFA.Report.Format (Format)

import qualified EFA.Utility.Map as MapU
import qualified EFA.Utility.TotalMap as TMap

import qualified Data.Foldable as Fold
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Traversable (traverse)
import Data.Map (Map)


print ::
   (FormatValue i, FormatValue a) =>
   Map i a -> IO ()
print =
   putStrLn . Format.unUnicode . format

format ::
   (FormatValue i, FormatValue a, Format output) =>
   Map i a -> output
format assigns =
   Format.lines $ Map.elems $ flip Map.mapWithKey assigns $ \term val ->
      Format.assign (formatValue term) (formatValue val)

ignoreUndetermined :: (Ord i) => Map i (Result a) -> Map i a
ignoreUndetermined =
   Map.mapMaybe Result.toMaybe


{- |
smart constructor
-}
indexSet :: Map i Stack.Branch -> IndexSet i
indexSet = IndexSet

deltaIndexSet :: (Ord i) => Map i Stack.Branch -> IndexSet i
deltaIndexSet =
   IndexSet . Map.filter (Stack.Delta==)

newtype IndexSet i = IndexSet (Map i Stack.Branch)
   deriving (Eq, Ord, Show)

instance FormatValue i => FormatValue (IndexSet i) where
   formatValue (IndexSet x) =
      Format.list $
      map
         (\(i,b) ->
            Format.recordDelta
               (case b of
                  Stack.Before -> Idx.Before
                  Stack.Delta -> Idx.Delta) $
            formatValue i) $
      Map.toList x

{- |
Convert a list of AssignMaps to an AssignMap of lists.
-}
transpose :: (Ord i, Num a) => [Map i a] -> Map i [a]
transpose = TMap.core . traverse (TMap.cons 0)


{- |
Keep only values above a certain threshold.
-}
threshold :: (Ord i, Ord a, Num a) => a -> Map i a -> Map i a
threshold x = Map.filter ((>=x) . abs)

{- |
Keep all those lists where at least one value is above a threshold.
This allows to filter consistently across stacks.
-}
simultaneousThreshold :: (Ord i, Ord a, Num a) => a -> Map i [a] -> Map i [a]
simultaneousThreshold x = Map.filter (any ((>=x) . abs))


{-
This is compatible with a @Stack i a@,
but it may not be a perfect hyper-cube
since some corners may be missing.
-}
type AssignMap i a = Map (Map i Stack.Branch) a

{- |
@filterDeltaVars vars@ keeps only the terms
where every @var@ from @vars@ is a delta var.
-}
filterDeltaVars ::
   (Ord i) =>
   [i] ->
   AssignMap i a ->
   AssignMap i a
filterDeltaVars is =
   let set = Set.fromList is
   in  Map.filterWithKey
          (\k _ -> Fold.all (Stack.Delta ==) $ MapU.intersectionSet k set)


cumulate ::
   (Ord (idx node), Arith.Sum a) =>
   [AssignMap (Idx.InSection idx node) a] ->
   AssignMap (idx node) a
cumulate =
   Map.unionsWith (~+) .
   map
      (Map.mapKeysWith (~+)
         (Map.mapKeys (\(Idx.InSection _sec node) -> node)))


stripSection ::
   (Ord (idx node)) =>
   AssignMap (Idx.InSection idx node) a ->
   AssignMap (idx node) a
stripSection =
   Map.mapKeysWith
      (error "AssignMap.stripSection: multiple sections in one assignmap")
      (Map.mapKeys (\(Idx.InSection _sec node) -> node))


lookupStack:: (Ord i, Ord node, Show node) =>
                              Idx.InSection Idx.Energy node
                              -> Env.Complete
                                   node t (EqRecord.Absolute (Result.Result (Stack.Stack i a)))
                              -> Map.Map (IndexSet i) a

lookupStack energyIndex env =  case Map.lookup energyIndex (Env.energyMap signalEnv) of
    Nothing -> error (show energyIndex ++ "undefined")
    Just d ->
      case EqRecord.unAbsolute d of
        Result.Undetermined -> error (show energyIndex ++ "undetermined")
        Result.Determined xs -> Map.mapKeys deltaIndexSet $
                             Stack.assignDeltaMap xs

   where
        Env.Complete _scalarEnv signalEnv = env

lookupAllStacks :: (Ord i, Ord node, Eq node) => Idx.Energy node
                   -> Env.Complete node t 
                         (EqRecord.Absolute (Result.Result (Stack.Stack i Double)))
                   -> [(Idx.InSection Idx.Energy node,Stack.Stack i Double)]

lookupAllStacks e0 =
   Map.toList .
   fmap Arith.integrate .
   Map.mapMaybe Result.toMaybe .
   fmap EqRecord.unAbsolute .
   Map.filterWithKey (\(Idx.InSection _sec e) _ -> e == e0) .
   Env.energyMap . Env.signal


lookupAggregatedStack ::
   (Ord i, Ord node, Show node,
    Arith.Constant a, a ~ Arith.Scalar v, Arith.Integrate v) =>
   Idx.Energy node ->
   Env.Complete node t
      (EqRecord.Absolute (Result.Result (Stack.Stack i v))) ->
   Map.Map (Map.Map i Stack.Branch) a

lookupAggregatedStack e0 =
   Fold.foldMap (Stack.assignDeltaMap . Arith.integrate) .
   Map.mapMaybe Result.toMaybe .
   fmap EqRecord.unAbsolute .
   Map.filterWithKey (\(Idx.InSection _sec e) _ -> e == e0) .
   Env.energyMap . Env.signal
