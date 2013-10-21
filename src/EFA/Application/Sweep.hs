{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module EFA.Application.Sweep where

import qualified EFA.Signal.Signal as Sig

import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.SystemEta as SeqEta

import qualified EFA.Flow.State.Quantity as StateFlow
import qualified EFA.Flow.State.SystemEta as StateEta

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)

import qualified EFA.Graph.Topology.Node as Node

import Control.Applicative (liftA2)

import qualified Data.Traversable as Trav
import qualified Data.Map as Map; import Data.Map (Map)
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Vector as V
import Data.Traversable (Traversable, traverse)
import Data.Foldable (Foldable, foldMap)
import Data.Maybe (mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)

-- | Map a two dimensional load room (varX, varY) and find per load situation
-- | the optimal solution in the 2d-solution room (two degrees of freevarOptX varOptY)


data Pair f g a =
  Pair {
    fstRecord :: f a,
    sndRecord :: g a
  }

instance (Functor f, Functor g) => Functor (Pair f g) where
  fmap f (Pair xs ys) = Pair (fmap f xs) (fmap f ys)

instance (Foldable f, Foldable g) => Foldable (Pair f g) where
  foldMap f (Pair xs ys) = foldMap f xs <> foldMap f ys

instance (Traversable f, Traversable g) => Traversable (Pair f g) where
  traverse f (Pair xs ys) = liftA2 Pair (traverse f xs) (traverse f ys)

type Points f g v = Pair f g [v]

instance (Show (f a), Show (g a)) => Show (Pair f g a) where
  show (Pair f g) = "Pair (" ++ show f ++ ") (" ++ show g ++ ")"


doubleSweep ::
  (Traversable f, Traversable g, Ord (f a)) =>
  (Pair f g a -> b) -> Points f g a -> Map (f a) [b]
doubleSweep f =
  Map.fromListWith (++) . map (\xy -> (fstRecord xy, [f xy])) . Trav.sequence

-- verallgemeinern für n states
combineOptimalMaps ::
  Sig.UTSignal2 V.Vector V.Vector Sig.ArgMax ->
  Sig.PSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double ->
  Sig.PSignal2 V.Vector V.Vector Double
combineOptimalMaps state charge discharge =
  Sig.zipWith
     (\s (c, d) -> case s of Sig.ArgMax0 -> c; Sig.ArgMax1 -> d) state $
  Sig.zip charge discharge

optimalSolution2D ::
  (Node.C node, Eq a, Ord v, Arith.Constant v) =>
  SeqEta.Condition node a v ->
  SeqEta.Forcing node a v ->
  Sig.UTSignal2 V.Vector V.Vector (SeqFlow.Graph node a (Result v)) ->
  Maybe (v, SeqFlow.Graph node a (Result v))
optimalSolution2D cond forcing =
  optimalSolutionGeneric (SeqEta.objectiveFunction cond forcing) .
  concat . Sig.toList

optimalSolutionState ::
  (Node.C node, Ord v, Arith.Constant v) =>
  StateEta.Condition node a v ->
  StateEta.Forcing node a v ->
  [StateFlow.Graph node a (Result v)] ->
  Maybe (v, StateFlow.Graph node a (Result v))
optimalSolutionState cond forcing =
  optimalSolutionGeneric (StateEta.objectiveFunction cond forcing)

optimalSolutionGeneric ::
  Ord a =>
  (b -> Maybe a) -> [b] -> Maybe (a, b)
optimalSolutionGeneric f =
  fmap (NonEmpty.maximumBy (comparing fst)) . NonEmpty.fetch .
  mapMaybe (\x -> fmap (flip (,) x) $ f x)
