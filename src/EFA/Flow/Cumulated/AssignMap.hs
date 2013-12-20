module EFA.Flow.Cumulated.AssignMap where

import qualified EFA.Flow.Cumulated.Variable as Var

import qualified EFA.Graph.Topology.Node as Node

import EFA.Report.FormatValue (FormatValue, formatAssign)
import EFA.Report.Format (Format)

import qualified Data.Map as Map ; import Data.Map (Map)

import Data.Monoid (Monoid, mappend, mempty)


type Any node a = Map (Var.Any node) a

newtype AssignMap node a = AssignMap (Any node a)

instance (Ord node) => Monoid (AssignMap node a) where
   mempty = AssignMap Map.empty
   mappend (AssignMap map0) (AssignMap map1) =
      AssignMap
         (Map.unionWith (error "duplicate variable") map0 map1)

singleton :: Var.Any node -> a -> AssignMap node a
singleton idx a = AssignMap (Map.singleton idx a)



lift0 ::
   Any node v ->
   AssignMap node v
lift0 = AssignMap

lift1 ::
   (Any node v0 -> Any node v) ->
   AssignMap node v0 ->
   AssignMap node v
lift1 f (AssignMap signal0) =
   AssignMap (f signal0)

lift2 ::
   (Any node v0 -> Any node v1 -> Any node v) ->
   AssignMap node v0 ->
   AssignMap node v1 ->
   AssignMap node v
lift2 f (AssignMap signal0) (AssignMap signal1) =
   AssignMap (f signal0 signal1)


intersectionWith ::
   (Ord node) =>
   (a -> b -> c) ->
   AssignMap node a ->
   AssignMap node b ->
   AssignMap node c
intersectionWith f =
   lift2
      (Map.intersectionWith f)

difference ::
   (Ord node) =>
   AssignMap node v ->
   AssignMap node v ->
   AssignMap node v
difference = lift2 Map.difference

filter ::
   Ord node =>
   (v -> Bool) ->
   AssignMap node v ->
   AssignMap node v
filter f = lift1 (Map.filter f)


format ::
   (Node.C node, FormatValue v, Format output) =>
   AssignMap node v -> [output]
format (AssignMap assigns) =
   Map.elems (Map.mapWithKey formatAssign assigns)
