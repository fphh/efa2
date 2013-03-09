{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Equation.Env where

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
type EnergyMap node a = M.Map (Idx.Energy node) a
type MaxEnergyMap node a = M.Map (Idx.MaxEnergy node) a
type PowerMap node a = M.Map (Idx.Power node) a
type EtaMap node a = M.Map (Idx.Eta node) a
type DTimeMap node a = M.Map (Idx.DTime node) a
type XMap node a = M.Map (Idx.X node) a
type SumMap node a = M.Map (Idx.Sum node) a
type StorageMap node a = M.Map (Idx.Storage node) a
type StEnergyMap node a = M.Map (Idx.StEnergy node) a
type StXMap node a = M.Map (Idx.StX node) a


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
      stXMap :: StXMap node a
   } deriving (Show)

data Complete node b a =
   Complete {
      scalar :: Scalar node b,
      signal :: Signal node a
   }


class AccessPart env where
   type PartElement env a v :: *
   accessPart ::
      Accessor.T (Complete node a v) (env node (PartElement env a v))

instance AccessPart Scalar where
   type PartElement Scalar a v = a
   accessPart =
      Accessor.fromSetGet (\x c -> c{scalar = x}) scalar

instance AccessPart Signal where
   type PartElement Signal a v = v
   accessPart =
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
   formatValue (Complete (Scalar me st se sx) (Signal e p n dt x s)) =
      Format.lines $
         formatMap e ++
         formatMap se ++
         formatMap me ++
         formatMap p ++
         formatMap n ++
         formatMap dt ++
         formatMap x ++
         formatMap s ++
         formatMap sx ++
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
      Var.StEnergy  idx -> M.lookup idx . stEnergyMap
      Var.StX       idx -> M.lookup idx . stXMap


type Element idx a v = PartElement (Environment (Var.Type idx)) a v

accessMap ::
   (AccessMap idx) =>
   Accessor.T (Complete node a v) (M.Map (idx node) (Element idx a v))
accessMap =
   accessPartMap . accessPart

class
   (AccessPart (Environment var), var ~ Variable (Environment var)) =>
      VarEnv var where
   type Environment var :: * -> * -> *
   type Variable env :: * -> *

instance VarEnv Var.Signal where
   type Environment Var.Signal = Signal
   type Variable Signal = Var.Signal

instance VarEnv Var.Scalar where
   type Environment Var.Scalar = Scalar
   type Variable Scalar = Var.Scalar


class (VarEnv (Var.Type idx), Var.Index idx) => AccessMap idx where
   accessPartMap ::
      Accessor.T (Environment (Var.Type idx) node a) (M.Map (idx node) a)

instance AccessMap Idx.Energy where
   accessPartMap =
      Accessor.fromSetGet (\x c -> c{energyMap = x}) energyMap

instance AccessMap Idx.Power where
   accessPartMap =
      Accessor.fromSetGet (\x c -> c{powerMap = x}) powerMap

instance AccessMap Idx.Eta where
   accessPartMap =
      Accessor.fromSetGet (\x c -> c{etaMap = x}) etaMap

instance AccessMap Idx.DTime where
   accessPartMap =
      Accessor.fromSetGet (\x c -> c{dtimeMap = x}) dtimeMap

instance AccessMap Idx.X where
   accessPartMap =
      Accessor.fromSetGet (\x c -> c{xMap = x}) xMap

instance AccessMap Idx.Sum where
   accessPartMap =
      Accessor.fromSetGet (\x c -> c{sumMap = x}) sumMap


instance AccessMap Idx.MaxEnergy where
   accessPartMap =
      Accessor.fromSetGet (\x c -> c{maxEnergyMap = x}) maxEnergyMap

instance AccessMap Idx.Storage where
   accessPartMap =
      Accessor.fromSetGet (\x c -> c{storageMap = x}) storageMap

instance AccessMap Idx.StEnergy where
   accessPartMap =
      Accessor.fromSetGet (\x c -> c{stEnergyMap = x}) stEnergyMap

instance AccessMap Idx.StX where
   accessPartMap =
      Accessor.fromSetGet (\x c -> c{stXMap = x}) stXMap



instance Functor (Signal node) where
   fmap f (Signal e p n dt x s) =
      Signal (fmap f e) (fmap f p) (fmap f n) (fmap f dt) (fmap f x) (fmap f s)

instance Functor (Scalar node) where
   fmap f (Scalar me st se sx) =
      Scalar (fmap f me) (fmap f st) (fmap f se) (fmap f sx)


instance Foldable (Signal node) where
   foldMap = foldMapDefault

instance Foldable (Scalar node) where
   foldMap = foldMapDefault


instance Traversable (Signal node) where
   sequenceA (Signal e p n dt x s) =
      pure Signal <?> e <?> p <?> n <?> dt <?> x <?> s

instance Traversable (Scalar node) where
   sequenceA (Scalar me st se sx) =
      pure Scalar <?> me <?> st <?> se <?> sx

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
   mempty = Scalar M.empty M.empty M.empty M.empty
   mappend (Scalar me st se sx) (Scalar me' st' se' sx') =
      Scalar
         (M.union me me') (M.union st st')
         (M.union se se') (M.union sx sx')

instance (Ord node) => Monoid (Complete node b a) where
   mempty = Complete mempty mempty
   mappend (Complete scalar0 signal0) (Complete scalar1 signal1) =
      Complete (mappend scalar0 scalar1) (mappend signal0 signal1)
