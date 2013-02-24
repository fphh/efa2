{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module EFA.Equation.Env where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology.Node as Node
import qualified EFA.Equation.Variable as Var

import qualified Data.Map as M

import qualified Data.Accessor.Basic as Accessor
import Control.Applicative (Applicative, pure, (<*>), liftA3)
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid, mempty, mappend)

import qualified EFA.Report.Format as Format
import EFA.Report.Format (Format)
import EFA.Report.FormatValue (FormatValue, formatValue)


-- Environments
type EnergyMap node a = M.Map (Idx.Energy node) a
type MaxEnergyMap node a = M.Map (Idx.MaxEnergy node) a
type PowerMap node a = M.Map (Idx.Power node) a
type EtaMap node a = M.Map (Idx.Eta node) a
type DTimeMap node a = M.Map (Idx.DTime node) a
type XMap node a = M.Map (Idx.X node) a
type YMap node a = M.Map (Idx.Y node) a
type SumMap node a = M.Map (Idx.Sum node) a
type StorageMap node a = M.Map (Idx.Storage node) a


data Env node a =
   Env {
      energyMap :: EnergyMap node a,
      maxenergyMap :: MaxEnergyMap node a,

      powerMap :: PowerMap node a,
      etaMap :: EtaMap node a,
      dtimeMap :: DTimeMap node a,
      xMap :: XMap node a,
      yMap :: YMap node a,
      sumMap :: SumMap node a,
      {-
      If 'a' is a signal type,
      then storages must still be scalar values.
      Maybe we should move the storageMap to another data type
      in order to maintain the Functor instances.
      -}
      storageMap :: StorageMap node a
   } deriving (Show)


formatAssign ::
   (Var.MkIdxC idx, Node.C node, FormatValue a, Format output) =>
   idx node -> a -> output
formatAssign lhs rhs =
   Format.assign (formatValue $ Var.mkIdx lhs) (formatValue rhs)

formatMap ::
   (Var.MkIdxC idx, Node.C node, FormatValue a, Format output) =>
   M.Map (idx node) a -> [output]
formatMap =
   map (uncurry formatAssign) . M.toList

instance (Node.C node, FormatValue a) => FormatValue (Env node a) where
   formatValue (Env e me p n dt x y s st) =
      Format.lines $
         formatMap e ++
         formatMap me ++
         formatMap p ++
         formatMap n ++
         formatMap dt ++
         formatMap x ++
         formatMap y ++
         formatMap s ++
         formatMap st


class AccessMap idx where
   accessMap :: Accessor.T (Env node a) (M.Map (idx node) a)

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


instance Functor (Env node) where
   fmap f (Env e me p n dt x y s st) =
      Env (fmap f e) (fmap f me) (fmap f p) (fmap f n) (fmap f dt) (fmap f x) (fmap f y) (fmap f s) (fmap f st)

instance Foldable (Env node) where
   foldMap = foldMapDefault

instance Traversable (Env node) where
   sequenceA (Env e me p n dt x y s st) =
      pure Env <?> e <?> me <?> p <?> n <?> dt <?> x <?> y <?> s <?> st

infixl 4 <?>
(<?>) ::
   (Applicative f, Traversable map) =>
   f (map a -> b) -> map (f a) -> f b
f <?> x = f <*> sequenceA x



instance (Ord node) => Monoid (Env node a) where
   mempty = Env M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty
   mappend
         (Env e me p n dt x y s st)
         (Env e' me' p' n' dt' x' y' s' st') =
      Env
         (M.union e e') (M.union me me')
         (M.union p p') (M.union n n')
         (M.union dt dt') (M.union x x') (M.union y y')
         (M.union s s') (M.union st st')


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

deltaConst :: Num a => a -> Delta a
deltaConst x = Delta {before = x, after = x, delta = 0}

deltaCons :: Num a => a -> a -> Delta a
deltaCons b a = Delta {before = b, after = a, delta = a-b}

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


class Ord idx => Record idx rec | idx -> rec, rec -> idx where
   recordIndices :: rec idx
   accessRecord :: idx -> Accessor.T (rec a) a

instance Record Idx.Absolute Absolute where
   recordIndices = Absolute Idx.Absolute
   accessRecord Idx.Absolute = Accessor.fromWrapper Absolute unAbsolute

instance Record Idx.Delta Delta where
   recordIndices =
      Delta {delta = Idx.Delta, before = Idx.Before, after = Idx.After}
   accessRecord idx =
      case idx of
         Idx.Delta  -> Accessor.fromSetGet (\a d -> d{delta  = a}) delta
         Idx.Before -> Accessor.fromSetGet (\a d -> d{before = a}) before
         Idx.After  -> Accessor.fromSetGet (\a d -> d{after  = a}) after
