module EFA.Equation.Env where

import qualified EFA.Graph.Topology.Index as Idx

import qualified Data.Map as M

import qualified Data.Accessor.Basic as Accessor
import Control.Applicative (Applicative, pure, (<*>))
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid, mempty, mappend)


-- Environments
type EnergyMap rec node a = M.Map (Idx.Record rec (Idx.Energy node)) a
type MaxEnergyMap rec node a = M.Map (Idx.Record rec (Idx.MaxEnergy node)) a
type PowerMap rec node a = M.Map (Idx.Record rec (Idx.Power node)) a
type EtaMap rec node a = M.Map (Idx.Record rec (Idx.Eta node)) a
type DTimeMap rec node a = M.Map (Idx.Record rec (Idx.DTime node)) a
type XMap rec node a = M.Map (Idx.Record rec (Idx.X node)) a
type YMap rec node a = M.Map (Idx.Record rec (Idx.Y node)) a
type SumMap rec node a = M.Map (Idx.Record rec (Idx.Sum node)) a
type StorageMap rec node a = M.Map (Idx.Record rec (Idx.Storage node)) a


data Env rec node a =
   Env {
      energyMap :: EnergyMap rec node a,
      maxenergyMap :: MaxEnergyMap rec node a,

      powerMap :: PowerMap rec node a,
      etaMap :: EtaMap rec node a,
      dtimeMap :: DTimeMap rec node a,
      xMap :: XMap rec node a,
      yMap :: YMap rec node a,
      sumMap :: SumMap rec node a,
      {-
      If 'a' is a signal type,
      then storages must still be scalar values.
      Maybe we should move the storageMap to another data type
      in order to maintain the Functor instances.
      -}
      storageMap :: StorageMap rec node a
   } deriving (Show)


class AccessMap idx where
   accessMap :: Accessor.T (Env rec node a) (M.Map (Idx.Record rec (idx node)) a)

instance AccessMap Idx.Energy where
   accessMap =
      Accessor.fromSetGet (\x c -> c{energyMap = x}) energyMap

instance AccessMap Idx.MaxEnergy where
   accessMap =
      Accessor.fromSetGet (\x c -> c{maxenergyMap = x}) maxenergyMap

instance AccessMap Idx.Power where
   accessMap =
      Accessor.fromSetGet (\x c -> c{powerMap = x}) powerMap

instance AccessMap Idx.Eta where
   accessMap =
      Accessor.fromSetGet (\x c -> c{etaMap = x}) etaMap

instance AccessMap Idx.DTime where
   accessMap =
      Accessor.fromSetGet (\x c -> c{dtimeMap = x}) dtimeMap

instance AccessMap Idx.X where
   accessMap =
      Accessor.fromSetGet (\x c -> c{xMap = x}) xMap

instance AccessMap Idx.Y where
   accessMap =
      Accessor.fromSetGet (\x c -> c{yMap = x}) yMap

instance AccessMap Idx.Sum where
   accessMap =
      Accessor.fromSetGet (\x c -> c{sumMap = x}) sumMap

instance AccessMap Idx.Storage where
   accessMap =
      Accessor.fromSetGet (\x c -> c{storageMap = x}) storageMap


instance Functor (Env rec node) where
   fmap f (Env e me p n dt x y s st) =
      Env (fmap f e) (fmap f me) (fmap f p) (fmap f n) (fmap f dt) (fmap f x) (fmap f y) (fmap f s) (fmap f st)

instance Foldable (Env rec node) where
   foldMap = foldMapDefault

instance Traversable (Env rec node) where
   sequenceA (Env e me p n dt x y s st) =
      pure Env <?> e <?> me <?> p <?> n <?> dt <?> x <?> y <?> s <?> st

infixl 4 <?>
(<?>) ::
   (Applicative f, Traversable map) =>
   f (map a -> b) -> map (f a) -> f b
f <?> x = f <*> sequenceA x



instance (Ord node, Ord rec) => Monoid (Env rec node a) where
   mempty = Env M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty
   mappend
         (Env e me p n dt x y s st)
         (Env e' me' p' n' dt' x' y' s' st') =
      Env
         (M.union e e') (M.union me me')
         (M.union p p') (M.union n n')
         (M.union dt dt') (M.union x x') (M.union y y')
         (M.union s s') (M.union st st')
