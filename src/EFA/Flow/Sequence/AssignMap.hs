module EFA.Flow.Sequence.AssignMap where

import qualified EFA.Flow.Sequence.Variable as Var

import qualified EFA.Graph.Topology.Node as Node

import EFA.Report.FormatValue (FormatValue, formatAssign)
import EFA.Report.Format (Format)

import qualified Data.Map as Map ; import Data.Map (Map)

import Data.Monoid (Monoid, mappend, mempty)


type Scalar node a = Map (Var.Scalar node) a
type Signal node v = Map (Var.Signal node) v

data AssignMap node a v =
   AssignMap
      (Scalar node a)
      (Signal node v)
   deriving (Eq, Ord, Show)

instance (Ord node) => Monoid (AssignMap node a v) where
   mempty = AssignMap Map.empty Map.empty
   mappend (AssignMap scalar0 signal0) (AssignMap scalar1 signal1) =
      AssignMap
         (Map.unionWith (error "duplicate variable") scalar0 scalar1)
         (Map.unionWith (error "duplicate variable") signal0 signal1)

scalarSingleton :: Var.Scalar node -> a -> AssignMap node a v
scalarSingleton idx a = AssignMap (Map.singleton idx a) Map.empty

signalSingleton :: Var.Signal node -> v -> AssignMap node a v
signalSingleton idx v = AssignMap Map.empty (Map.singleton idx v)



lift0 ::
   Scalar node a ->
   Signal node v ->
   AssignMap node a v
lift0 = AssignMap

lift1 ::
   (Scalar node a0 -> Scalar node a) ->
   (Signal node v0 -> Signal node v) ->
   AssignMap node a0 v0 ->
   AssignMap node a v
lift1 f g (AssignMap scalar0 signal0) =
   AssignMap (f scalar0) (g signal0)

lift2 ::
   (Scalar node a0 -> Scalar node a1 -> Scalar node a) ->
   (Signal node v0 -> Signal node v1 -> Signal node v) ->
   AssignMap node a0 v0 ->
   AssignMap node a1 v1 ->
   AssignMap node a v
lift2 f g (AssignMap scalar0 signal0) (AssignMap scalar1 signal1) =
   AssignMap (f scalar0 scalar1) (g signal0 signal1)


intersectionWith ::
   (Ord node) =>
   (a -> b -> c) ->
   (u -> v -> w) ->
   AssignMap node a u ->
   AssignMap node b v ->
   AssignMap node c w
intersectionWith f g =
   lift2
      (Map.intersectionWith f)
      (Map.intersectionWith g)

difference ::
   (Ord node) =>
   AssignMap node a v ->
   AssignMap node a v ->
   AssignMap node a v
difference = lift2 Map.difference Map.difference

filter ::
   Ord node =>
   (a -> Bool) ->
   (v -> Bool) ->
   AssignMap node a v ->
   AssignMap node a v
filter f g = lift1 (Map.filter f) (Map.filter g)


format ::
   (Node.C node, FormatValue a, FormatValue v, Format output) =>
   AssignMap node a v -> [output]
format (AssignMap scalar signal) =
   Map.elems (Map.mapWithKey formatAssign scalar)
   ++
   Map.elems (Map.mapWithKey formatAssign signal)
