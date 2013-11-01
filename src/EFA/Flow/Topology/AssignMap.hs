module EFA.Flow.Topology.AssignMap where

import qualified EFA.Flow.Topology.Variable as Var

import qualified EFA.Graph.Topology.Node as Node

import EFA.Report.FormatValue (FormatValue, formatAssign)
import EFA.Report.Format (Format)

import qualified Data.Map as Map ; import Data.Map (Map)

import Data.Monoid (Monoid, mappend, mempty)


type Signal node v = Map (Var.Signal node) v

newtype AssignMap node v = AssignMap (Signal node v)
   deriving (Eq, Ord, Show)

instance (Ord node) => Monoid (AssignMap node v) where
   mempty = AssignMap Map.empty
   mappend (AssignMap signal0) (AssignMap signal1) =
      AssignMap
         (Map.unionWith (error "duplicate variable") signal0 signal1)

singleton :: Var.Signal node -> v -> AssignMap node v
singleton idx v = AssignMap (Map.singleton idx v)



lift0 ::
   Signal node v ->
   AssignMap node v
lift0 = AssignMap

lift1 ::
   (Signal node v0 -> Signal node v) ->
   AssignMap node v0 ->
   AssignMap node v
lift1 f (AssignMap signal0) =
   AssignMap (f signal0)

lift2 ::
   (Signal node v0 -> Signal node v1 -> Signal node v) ->
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
format (AssignMap signal) =
   Map.elems (Map.mapWithKey formatAssign signal)
