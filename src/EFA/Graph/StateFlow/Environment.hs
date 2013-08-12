{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Graph.StateFlow.Environment where

import qualified EFA.Graph.StateFlow.Index as XIdx
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Variable as Var

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified EFA.Utility.Bifunctor as BF
import qualified EFA.Utility.Map as MapU

import qualified Data.Map as Map; import Data.Map (Map)

import qualified Data.Accessor.Basic as Accessor
import Control.Category ((.))
import Control.Applicative (Applicative, pure, (<*>))
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid, mempty, mappend)

import Prelude hiding (lookup, (.))


-- Environments
type EnergyMap node a = Map (XIdx.Energy node) a
type PowerMap node a = Map (XIdx.Power node) a
type EtaMap node a = Map (XIdx.Eta node) a
type DTimeMap node a = Map (XIdx.DTime node) a
type XMap node a = Map (XIdx.X node) a
type SumMap node a = Map (XIdx.Sum node) a

type StEnergyMap node a = Map (XIdx.StEnergy node) a
type StXMap node a = Map (XIdx.StX node) a
type StInSumMap node a = Map (XIdx.StInSum node) a
type StOutSumMap node a = Map (XIdx.StOutSum node) a


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


instance
   (Node.C node, FormatValue b, FormatValue a) =>
      FormatValue (Complete node b a) where
   formatValue (Complete (Scalar se sx sis sos) (Signal e p n dt x s)) =
      Format.lines $
         formatMap e ++
         formatMap se ++
         formatMap p ++
         formatMap n ++
         formatMap dt ++
         formatMap x ++
         formatMap sx ++
         formatMap s ++
         formatMap sis ++
         formatMap sos ++
         []




type Element idx a v = PartElement (Environment idx) a v

accessMap ::
   (AccessMap idx) =>
   Accessor.T (Complete node a v) (Map (idx node) (Element idx a v))
accessMap =
   accessPartMap . accessPart

insert ::
   (AccessMap idx, Ord (idx node)) =>
   idx node ->
   Element idx a v ->
   Complete node a v ->
   Complete node a v
insert idx val =
   Accessor.modify accessMap $ Map.insert idx val


class (AccessPart (Environment idx), Var.Index idx) => AccessMap idx where
   type Environment idx :: * -> * -> *
   accessPartMap ::
      Accessor.T
         (Environment idx node a)
         (Map (idx node) a)

instance (AccessSignalMap idx) => AccessMap (Idx.InState idx) where
   type Environment (Idx.InState idx) = Signal
   accessPartMap = accessSignalMap

instance (AccessScalarMap idx) => AccessMap (Idx.ForNode idx) where
   type Environment (Idx.ForNode idx) = Scalar
   accessPartMap = accessScalarMap


class (Var.SignalIndex idx) => AccessSignalMap idx where
   accessSignalMap ::
      Accessor.T (Signal node a) (Map (Idx.InState idx node) a)

instance AccessSignalMap Idx.Energy where
   accessSignalMap =
      Accessor.fromSetGet (\x c -> c{energyMap = x}) energyMap

instance AccessSignalMap Idx.Power where
   accessSignalMap =
      Accessor.fromSetGet (\x c -> c{powerMap = x}) powerMap

instance AccessSignalMap Idx.Eta where
   accessSignalMap =
      Accessor.fromSetGet (\x c -> c{etaMap = x}) etaMap

instance AccessSignalMap Idx.DTime where
   accessSignalMap =
      Accessor.fromSetGet (\x c -> c{dtimeMap = x}) dtimeMap

instance AccessSignalMap Idx.X where
   accessSignalMap =
      Accessor.fromSetGet (\x c -> c{xMap = x}) xMap

instance AccessSignalMap Idx.Sum where
   accessSignalMap =
      Accessor.fromSetGet (\x c -> c{sumMap = x}) sumMap


class (Var.ScalarIndex idx) => AccessScalarMap idx where
   accessScalarMap ::
      Accessor.T (Scalar node a) (Map (Idx.ForNode idx node) a)

instance AccessScalarMap (Idx.StEnergy Idx.State) where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{stEnergyMap = x}) stEnergyMap

instance AccessScalarMap (Idx.StX Idx.State) where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{stXMap = x}) stXMap

instance AccessScalarMap (Idx.StInSum Idx.State) where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{stInSumMap = x}) stInSumMap

instance AccessScalarMap (Idx.StOutSum Idx.State) where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{stOutSumMap = x}) stOutSumMap


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


currySignal ::
   (Ord (idx node)) =>
   Map (Idx.InState idx node) v -> Map Idx.State (Map (idx node) v)
currySignal =
   MapU.curry "StateFlow.Environment.currySignal"
      (\(Idx.InPart state idx) -> (state, idx))

uncurrySignal ::
   (Ord (idx node)) =>
   Map Idx.State (Map (idx node) v) -> Map (Idx.InState idx node) v
uncurrySignal =
   MapU.uncurry "StateFlow.Environment.uncurrySignal" Idx.InPart
