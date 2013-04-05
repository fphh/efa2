{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Equation.Environment where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Equation.Variable as Var

import qualified Data.Map as M

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


-- Environments
type EnergyMap node a = M.Map (Idx.InSection Idx.Energy node) a
type PowerMap node a = M.Map (Idx.InSection Idx.Power node) a
type EtaMap node a = M.Map (Idx.InSection Idx.Eta node) a
type DTimeMap node a = M.Map (Idx.InSection Idx.DTime node) a
type XMap node a = M.Map (Idx.InSection Idx.X node) a
type SumMap node a = M.Map (Idx.InSection Idx.Sum node) a

type MaxEnergyMap node a = M.Map (Idx.ForNode Idx.MaxEnergy node) a
type StorageMap node a = M.Map (Idx.ForNode Idx.Storage node) a
type StEnergyMap node a = M.Map (Idx.ForNode Idx.StEnergy node) a
type StXMap node a = M.Map (Idx.ForNode Idx.StX node) a
type StSumMap node a = M.Map (Idx.ForNode Idx.StSum node) a


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
      storageMap :: StorageMap node a,
      stEnergyMap :: StEnergyMap node a,
      stXMap :: StXMap node a,
      stSumMap :: StSumMap node a
   } deriving (Show)

data Complete node b a =
   Complete {
      scalar :: Scalar node b,
      signal :: Signal node a
   } deriving (Show)


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
   M.Map (idx node) a -> [output]
formatMap =
   map (uncurry formatAssign) . M.toList


instance
   (Node.C node, FormatValue b, FormatValue a) =>
      FormatValue (Complete node b a) where
   formatValue (Complete (Scalar me st se sx ss) (Signal e p n dt x s)) =
      Format.lines $
         formatMap e ++
         formatMap se ++
         formatMap me ++
         formatMap p ++
         formatMap n ++
         formatMap dt ++
         formatMap x ++
         formatMap sx ++
         formatMap s ++
         formatMap ss ++
         formatMap st


lookupSignal ::
   Ord node =>
   Idx.InSection Var.Signal node -> Signal node a -> Maybe a
lookupSignal (Idx.InSection s var) =
   case var of
      Var.Energy    idx -> M.lookup (Idx.InSection s idx) . energyMap
      Var.Power     idx -> M.lookup (Idx.InSection s idx) . powerMap
      Var.Eta       idx -> M.lookup (Idx.InSection s idx) . etaMap
      Var.DTime     idx -> M.lookup (Idx.InSection s idx) . dtimeMap
      Var.X         idx -> M.lookup (Idx.InSection s idx) . xMap
      Var.Sum       idx -> M.lookup (Idx.InSection s idx) . sumMap

lookupScalar ::
   Ord node =>
   Idx.ForNode Var.Scalar node -> Scalar node a -> Maybe a
lookupScalar (Idx.ForNode var n) =
   case var of
      Var.MaxEnergy idx -> M.lookup (Idx.ForNode idx n) . maxEnergyMap
      Var.Storage   idx -> M.lookup (Idx.ForNode idx n) . storageMap
      Var.StEnergy  idx -> M.lookup (Idx.ForNode idx n) . stEnergyMap
      Var.StX       idx -> M.lookup (Idx.ForNode idx n) . stXMap
      Var.StSum     idx -> M.lookup (Idx.ForNode idx n) . stSumMap


type Element idx a v = PartElement (Environment idx) a v

accessMap ::
   (AccessMap idx) =>
   Accessor.T (Complete node a v) (M.Map (idx node) (Element idx a v))
accessMap =
   accessPartMap . accessPart


class (AccessPart (Environment idx), Var.Index idx) => AccessMap idx where
   type Environment idx :: * -> * -> *
   accessPartMap ::
      Accessor.T
         (Environment idx node a)
         (M.Map (idx node) a)

instance (AccessSignalMap idx) => AccessMap (Idx.InSection idx) where
   type Environment (Idx.InSection idx) = Signal
   accessPartMap = accessSignalMap

instance (AccessScalarMap idx) => AccessMap (Idx.ForNode idx) where
   type Environment (Idx.ForNode idx) = Scalar
   accessPartMap = accessScalarMap


class (Var.SignalIndex idx) => AccessSignalMap idx where
   accessSignalMap ::
      Accessor.T (Signal node a) (M.Map (Idx.InSection idx node) a)

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
      Accessor.T (Scalar node a) (M.Map (Idx.ForNode idx node) a)

instance AccessScalarMap Idx.MaxEnergy where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{maxEnergyMap = x}) maxEnergyMap

instance AccessScalarMap Idx.Storage where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{storageMap = x}) storageMap

instance AccessScalarMap Idx.StEnergy where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{stEnergyMap = x}) stEnergyMap

instance AccessScalarMap Idx.StX where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{stXMap = x}) stXMap

instance AccessScalarMap Idx.StSum where
   accessScalarMap =
      Accessor.fromSetGet (\x c -> c{stSumMap = x}) stSumMap



instance Functor (Signal node) where
   fmap f (Signal e p n dt x s) =
      Signal (fmap f e) (fmap f p) (fmap f n) (fmap f dt) (fmap f x) (fmap f s)

instance Functor (Scalar node) where
   fmap f (Scalar me st se sx ss) =
      Scalar (fmap f me) (fmap f st) (fmap f se) (fmap f sx) (fmap f ss)


instance Foldable (Signal node) where
   foldMap = foldMapDefault

instance Foldable (Scalar node) where
   foldMap = foldMapDefault


instance Traversable (Signal node) where
   sequenceA (Signal e p n dt x s) =
      pure Signal <?> e <?> p <?> n <?> dt <?> x <?> s

instance Traversable (Scalar node) where
   sequenceA (Scalar me st se sx ss) =
      pure Scalar <?> me <?> st <?> se <?> sx <?> ss

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
   mempty = Scalar M.empty M.empty M.empty M.empty M.empty
   mappend (Scalar me st se sx ss) (Scalar me' st' se' sx' ss') =
      Scalar
         (M.union me me') (M.union st st')
         (M.union se se') (M.union sx sx')
         (M.union ss ss')

instance (Ord node) => Monoid (Complete node b a) where
   mempty = Complete mempty mempty
   mappend (Complete scalar0 signal0) (Complete scalar1 signal1) =
      Complete (mappend scalar0 scalar1) (mappend signal0 signal1)
