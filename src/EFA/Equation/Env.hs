{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Equation.Env where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Equation.Variable as Var

import EFA.Equation.Arithmetic
          (Sum, (~-), Constant, zero)

import qualified Data.Map as M

import qualified Data.Accessor.Basic as Accessor
import Control.Applicative (Applicative, pure, (<*>), liftA3)
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid, mempty, mappend)

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue (FormatValue, formatValue)

import Prelude hiding (lookup)


-- Environments
type EnergyMap node a = M.Map (Idx.Energy node) a
type MaxEnergyMap node a = M.Map (Idx.MaxEnergy node) a
type PowerMap node a = M.Map (Idx.Power node) a
type EtaMap node a = M.Map (Idx.Eta node) a
type DTimeMap node a = M.Map (Idx.DTime node) a
type XMap node a = M.Map (Idx.X node) a
type SumMap node a = M.Map (Idx.Sum node) a
type StorageMap node a = M.Map (Idx.Storage node) a


data Signal node a =
   Signal {
      energyMap :: EnergyMap node a,
      powerMap :: PowerMap node a,
      etaMap :: EtaMap node a,
      dtimeMap :: DTimeMap node a,
      xMap :: XMap node a,
      sumMap :: SumMap node a
   } deriving (Show)

data Scalar node a =
   Scalar {
      maxEnergyMap :: MaxEnergyMap node a,
      storageMap :: StorageMap node a
   } deriving (Show)

data Complete node b a =
   Complete {
      scalar :: Scalar node b,
      signal :: Signal node a
   }

accessScalar :: Accessor.T (Complete node b a) (Scalar node b)
accessScalar =
   Accessor.fromSetGet (\x c -> c{scalar = x}) scalar

accessSignal :: Accessor.T (Complete node b a) (Signal node a)
accessSignal =
   Accessor.fromSetGet (\x c -> c{signal = x}) signal


formatAssign ::
   (Var.FormatIndex (idx node), Node.C node, FormatValue a, Format output) =>
   idx node -> a -> output
formatAssign lhs rhs =
   Format.assign (Var.formatIndex lhs) (formatValue rhs)

formatMap ::
   (Var.FormatIndex (idx node), Node.C node, FormatValue a, Format output) =>
   M.Map (idx node) a -> [output]
formatMap =
   map (uncurry formatAssign) . M.toList


instance
   (Node.C node, FormatValue b, FormatValue a) =>
      FormatValue (Complete node b a) where
   formatValue (Complete (Scalar me st) (Signal e p n dt x s)) =
      Format.lines $
         formatMap e ++
         formatMap me ++
         formatMap p ++
         formatMap n ++
         formatMap dt ++
         formatMap x ++
         formatMap s ++
         formatMap st


lookupSignal :: Ord node => Var.Signal node -> Signal node a -> Maybe a
lookupSignal v =
   case v of
      Var.Energy    idx -> M.lookup idx . energyMap
      Var.Power     idx -> M.lookup idx . powerMap
      Var.Eta       idx -> M.lookup idx . etaMap
      Var.DTime     idx -> M.lookup idx . dtimeMap
      Var.X         idx -> M.lookup idx . xMap
      Var.Sum       idx -> M.lookup idx . sumMap

lookupScalar :: Ord node => Var.Scalar node -> Scalar node a -> Maybe a
lookupScalar v =
   case v of
      Var.MaxEnergy idx -> M.lookup idx . maxEnergyMap
      Var.Storage   idx -> M.lookup idx . storageMap


type RecordIndexed rec = Idx.Record (RecordIndex rec)

lookupSignalRecord ::
   (Record rec, Ord node) =>
   RecordIndexed rec (Var.Signal node) ->
   Signal node (rec a) -> Maybe a
lookupSignalRecord (Idx.Record r v) =
   fmap (Accessor.get (accessRecord r)) . lookupSignal v



class AccessSignalMap idx where
   accessSignalMap :: Accessor.T (Signal node a) (M.Map (idx node) a)

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


class AccessScalarMap idx where
   accessScalarMap :: Accessor.T (Scalar node a) (M.Map (idx node) a)

instance AccessScalarMap Idx.MaxEnergy where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{maxEnergyMap = x}) maxEnergyMap

instance AccessScalarMap Idx.Storage where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{storageMap = x}) storageMap


instance Functor (Signal node) where
   fmap f (Signal e p n dt x s) =
      Signal (fmap f e) (fmap f p) (fmap f n) (fmap f dt) (fmap f x) (fmap f s)

instance Functor (Scalar node) where
   fmap f (Scalar me st) =
      Scalar (fmap f me) (fmap f st)


instance Foldable (Signal node) where
   foldMap = foldMapDefault

instance Foldable (Scalar node) where
   foldMap = foldMapDefault


instance Traversable (Signal node) where
   sequenceA (Signal e p n dt x s) =
      pure Signal <?> e <?> p <?> n <?> dt <?> x <?> s

instance Traversable (Scalar node) where
   sequenceA (Scalar me st) =
      pure Scalar <?> me <?> st

infixl 4 <?>
(<?>) ::
   (Applicative f, Traversable map) =>
   f (map a -> b) -> map (f a) -> f b
f <?> x = f <*> sequenceA x



instance (Ord node) => Monoid (Signal node a) where
   mempty = Signal M.empty M.empty M.empty M.empty M.empty M.empty
   mappend
         (Signal e p n dt x s)
         (Signal e' p' n' dt' x' s') =
      Signal
         (M.union e e') (M.union p p') (M.union n n')
         (M.union dt dt') (M.union x x') (M.union s s')

instance (Ord node) => Monoid (Scalar node a) where
   mempty = Scalar M.empty M.empty
   mappend (Scalar me st) (Scalar me' st') =
      Scalar (M.union me me') (M.union st st')

instance (Ord node) => Monoid (Complete node b a) where
   mempty = Complete mempty mempty
   mappend (Complete scalar0 signal0) (Complete scalar1 signal1) =
      Complete (mappend scalar0 scalar1) (mappend signal0 signal1)


newtype Absolute a = Absolute {unAbsolute :: a} deriving (Show)

instance Functor Absolute where
   fmap f (Absolute a) = Absolute $ f a

instance Applicative Absolute where
   pure a = Absolute a
   Absolute f <*> Absolute a = Absolute $ f a

instance Foldable Absolute where
   foldMap = foldMapDefault

instance Traversable Absolute where
   sequenceA (Absolute a) = fmap Absolute a


data Delta a = Delta {delta, before, after :: a} deriving (Show)

deltaConst :: Constant a => a -> Delta a
deltaConst x = Delta {before = x, after = x, delta = zero}

deltaCons :: Sum a => a -> a -> Delta a
deltaCons b a = Delta {before = b, after = a, delta = a~-b}

instance FormatValue a => FormatValue (Delta a) where
   formatValue rec =
      Format.list $
         Format.assign (Format.literal "delta")  (formatValue $ delta rec) :
         Format.assign (Format.literal "before") (formatValue $ before rec) :
         Format.assign (Format.literal "after")  (formatValue $ after rec) :
         []

instance Functor Delta where
   fmap f (Delta d b a) = Delta (f d) (f b) (f a)

instance Applicative Delta where
   pure a = Delta a a a
   Delta fd fb fa <*> Delta d b a = Delta (fd d) (fb b) (fa a)

instance Foldable Delta where
   foldMap = foldMapDefault

instance Traversable Delta where
   sequenceA (Delta d b a) = liftA3 Delta d b a


class
   (Ord (RecordIndex rec), Format.Record (RecordIndex rec),
    rec ~ IndexRecord (RecordIndex rec)) =>
      Record rec where
   type RecordIndex rec :: *
   type IndexRecord idx :: * -> *
   recordIndices :: (idx ~ RecordIndex rec) => rec idx
   accessRecord :: (idx ~ RecordIndex rec) => idx -> Accessor.T (rec a) a

instance Record Absolute where
   type RecordIndex Absolute = Idx.Absolute
   type IndexRecord Idx.Absolute = Absolute
   recordIndices = Absolute Idx.Absolute
   accessRecord Idx.Absolute = Accessor.fromWrapper Absolute unAbsolute

instance Record Delta where
   type RecordIndex Delta = Idx.Delta
   type IndexRecord Idx.Delta = Delta
   recordIndices =
      Delta {delta = Idx.Delta, before = Idx.Before, after = Idx.After}
   accessRecord idx =
      case idx of
         Idx.Delta  -> Accessor.fromSetGet (\a d -> d{delta  = a}) delta
         Idx.Before -> Accessor.fromSetGet (\a d -> d{before = a}) before
         Idx.After  -> Accessor.fromSetGet (\a d -> d{after  = a}) after
