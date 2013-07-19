{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Graph.StateFlow.Environment where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Equation.Variable as Var

import qualified Data.Map as Map
import Data.Map (Map)

import qualified Data.Accessor.Basic as Accessor
import Control.Category ((.))
import Control.Applicative (Applicative, pure, (<*>))
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid, mempty, mappend)

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue (FormatValue, formatValue)

import Prelude hiding (lookup, (.))

import qualified EFA.Utility.Bifunctor as BF


-- Environments
type EnergyMap node a = Map (Idx.InState Idx.Energy node) a
type PowerMap node a = Map (Idx.InState Idx.Power node) a
type EtaMap node a = Map (Idx.InState Idx.Eta node) a
type DTimeMap node a = Map (Idx.InState Idx.DTime node) a
type XMap node a = Map (Idx.InState Idx.X node) a
type SumMap node a = Map (Idx.InState Idx.Sum node) a

type StEnergyMap node a = Map (Idx.ForNode Idx.StateStEnergy node) a
type StXMap node a = Map (Idx.ForNode Idx.StateStX node) a
type StInSumMap node a = Map (Idx.ForNode Idx.StateStInSum node) a
type StOutSumMap node a = Map (Idx.ForNode Idx.StateStOutSum node) a


data Signal node a =
   Signal {
      energyMap :: EnergyMap node a,
      powerMap :: PowerMap node a,
      etaMap :: EtaMap node a,
      dtimeMap :: DTimeMap node a,
      xMap :: XMap node a,
      sumMap :: SumMap node a
   } deriving (Show, Eq)

data Scalar node a =
   Scalar {
      stEnergyMap :: StEnergyMap node a,
      stXMap :: StXMap node a,
      stInSumMap :: StInSumMap node a,
      stOutSumMap :: StOutSumMap node a
   } deriving (Show, Eq)

data Complete node a v =
   Complete {
      scalar :: Scalar node a,
      signal :: Signal node v
   } deriving (Show, Eq)


instance (Ord node) => BF.Bifunctor (Complete node) where
   bimap f g (Complete scal sig) = Complete (fmap f scal) (fmap g sig)
   first f (Complete scal sig) = Complete (fmap f scal) sig
   second g (Complete scal sig) = Complete scal (fmap g sig)


class AccessPart env where
   type PartElement env a v :: *
   switchPart :: f Scalar -> f Signal -> f env

instance AccessPart Scalar where
   type PartElement Scalar a v = a
   switchPart x _ = x

instance AccessPart Signal where
   type PartElement Signal a v = v
   switchPart _ x = x

newtype
   PartAcessor node a v env =
      PartAccessor {
         getPartAccessor ::
            Accessor.T (Complete node a v) (env node (PartElement env a v))
      }

accessPart ::
   (AccessPart env) =>
   Accessor.T (Complete node a v) (env node (PartElement env a v))
accessPart =
   getPartAccessor $
   switchPart
      (PartAccessor $ Accessor.fromSetGet (\x c -> c{scalar = x}) scalar)
      (PartAccessor $ Accessor.fromSetGet (\x c -> c{signal = x}) signal)


formatAssign ::
   (Var.FormatIndex idx, Node.C node, FormatValue a, Format output) =>
   idx node -> a -> output
formatAssign lhs rhs =
   Format.assign (Var.formatIndex lhs) (formatValue rhs)

formatMap ::
   (Var.FormatIndex idx, Node.C node, FormatValue a, Format output) =>
   Map (idx node) a -> [output]
formatMap =
   map (uncurry formatAssign) . Map.toList


signalLift0 ::
   (Ord node) =>
   (forall k. Ord k => Map k a) ->
   Signal node a
signalLift0 f =
   Signal f f f f f f

scalarLift0 ::
   (Ord node) =>
   (forall k. Ord k => Map k a) ->
   Scalar node a
scalarLift0 f =
   Scalar f f f f


signalLift1 ::
   (Ord node) =>
   (forall k. Ord k => Map k a -> Map k b) ->
   Signal node a -> Signal node b
signalLift1 f (Signal e p n dt x s) =
   Signal (f e) (f p) (f n) (f dt) (f x) (f s)

scalarLift1 ::
   (Ord node) =>
   (forall k. Ord k => Map k a -> Map k b) ->
   Scalar node a -> Scalar node b
scalarLift1 f (Scalar se sx sis sos) =
   Scalar (f se) (f sx) (f sis) (f sos)

lift1 ::
   (Scalar node a0 -> Scalar node a) ->
   (Signal node v0 -> Signal node v) ->
   Complete node a0 v0 ->
   Complete node a v
lift1 f g (Complete scalar0 signal0) =
   Complete (f scalar0) (g signal0)


signalLift2 ::
   (Ord node) =>
   (forall k. Ord k => Map k a -> Map k b -> Map k c) ->
   Signal node a -> Signal node b -> Signal node c
signalLift2 f (Signal e p n dt x s) (Signal e' p' n' dt' x' s') =
   Signal (f e e') (f p p') (f n n') (f dt dt') (f x x') (f s s')

scalarLift2 ::
   (Ord node) =>
   (forall k. Ord k => Map k a -> Map k b -> Map k c) ->
   Scalar node a -> Scalar node b -> Scalar node c
scalarLift2 f (Scalar se sx sis sos) (Scalar se' sx' sis' sos') =
   Scalar (f se se') (f sx sx') (f sis sis') (f sos sos')

lift2 ::
   (Scalar node a0 -> Scalar node a1 -> Scalar node a) ->
   (Signal node v0 -> Signal node v1 -> Signal node v) ->
   Complete node a0 v0 ->
   Complete node a1 v1 ->
   Complete node a v
lift2 f g (Complete scalar0 signal0) (Complete scalar1 signal1) =
   Complete (f scalar0 scalar1) (g signal0 signal1)


instance Ord node => Functor (Signal node) where
   fmap f = signalLift1 (fmap f)

instance Ord node => Functor (Scalar node) where
   fmap f = scalarLift1 (fmap f)

completeFMap ::
   Ord node =>
   (a0 -> a1) -> (v0 -> v1) ->
   Complete node a0 v0 -> Complete node a1 v1
completeFMap f g = lift1 (fmap f) (fmap g)


instance Ord node => Foldable (Signal node) where
   foldMap = foldMapDefault

instance Ord node => Foldable (Scalar node) where
   foldMap = foldMapDefault


instance Ord node => Traversable (Signal node) where
   sequenceA (Signal e p n dt x s) =
      pure Signal <?> e <?> p <?> n <?> dt <?> x <?> s

instance Ord node => Traversable (Scalar node) where
   sequenceA (Scalar se sx sis sos) =
      pure Scalar <?> se <?> sx <?> sis <?> sos

infixl 4 <?>
(<?>) ::
   (Applicative f, Traversable map) =>
   f (map a -> b) -> map (f a) -> f b
f <?> x = f <*> sequenceA x


instance (Ord node) => Monoid (Signal node a) where
   mempty = signalLift0 Map.empty
   mappend = signalLift2 Map.union

instance (Ord node) => Monoid (Scalar node a) where
   mempty = scalarLift0 Map.empty
   mappend = scalarLift2 Map.union

instance (Ord node) => Monoid (Complete node b a) where
   mempty = Complete mempty mempty
   mappend = lift2 mappend mappend


signalIntersectionWith ::
   (Ord node) =>
   (a -> b -> c) ->
   Signal node a ->
   Signal node b ->
   Signal node c
signalIntersectionWith f =
   signalLift2 (Map.intersectionWith f)

scalarIntersectionWith ::
   (Ord node) =>
   (a -> b -> c) ->
   Scalar node a ->
   Scalar node b ->
   Scalar node c
scalarIntersectionWith f =
   scalarLift2 (Map.intersectionWith f)

intersectionWith ::
   (Ord node) =>
   (a -> b -> c) ->
   (u -> v -> w) ->
   Complete node a u ->
   Complete node b v ->
   Complete node c w
intersectionWith f g =
   lift2
      (scalarIntersectionWith f)
      (signalIntersectionWith g)


signalDifference ::
   (Ord node) =>
   Signal node a ->
   Signal node a ->
   Signal node a
signalDifference =
   signalLift2 Map.difference

scalarDifference ::
   (Ord node) =>
   Scalar node a ->
   Scalar node a ->
   Scalar node a
scalarDifference =
   scalarLift2 Map.difference

difference ::
   (Ord node) =>
   Complete node a v ->
   Complete node a v ->
   Complete node a v
difference =
   lift2 scalarDifference signalDifference


signalFilter ::
   Ord node =>
   (a -> Bool) -> Signal node a -> Signal node a
signalFilter f = signalLift1 (Map.filter f)

scalarFilter ::
   Ord node =>
   (a -> Bool) -> Scalar node a -> Scalar node a
scalarFilter f = scalarLift1 (Map.filter f)

filter ::
   Ord node =>
   (a -> Bool) ->
   (v -> Bool) ->
   Complete node a v ->
   Complete node a v
filter f g (Complete scalar0 signal0) =
   Complete (scalarFilter f scalar0) (signalFilter g signal0)
