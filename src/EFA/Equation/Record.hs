{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Equation.Record where

import qualified EFA.Equation.Env as Env
import qualified EFA.Equation.Variable as Var

import qualified EFA.Graph.Topology.Index as Idx

import EFA.Equation.Arithmetic (Sum, (~-), Constant, zero)

import qualified Data.Accessor.Basic as Accessor
import Control.Category ((.))
import Control.Applicative (Applicative, pure, (<*>), liftA3)
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import Prelude hiding (lookup, (.))


type Indexed rec = Idx.Record (ToIndex rec)

lookupSignal ::
   (C rec, Ord node) =>
   Indexed rec (Var.Signal node) ->
   Env.Signal node (rec a) -> Maybe a
lookupSignal (Idx.Record r v) =
   fmap (Accessor.get (access r)) . Env.lookupSignal v


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
   (Ord (ToIndex rec), Format.Record (ToIndex rec),
    rec ~ FromIndex (ToIndex rec)) =>
      C rec where
   type ToIndex rec :: *
   type FromIndex idx :: * -> *
   indices :: (idx ~ ToIndex rec) => rec idx
   access :: (idx ~ ToIndex rec) => idx -> Accessor.T (rec a) a

instance C Absolute where
   type ToIndex Absolute = Idx.Absolute
   type FromIndex Idx.Absolute = Absolute
   indices = Absolute Idx.Absolute
   access Idx.Absolute = Accessor.fromWrapper Absolute unAbsolute

instance C Delta where
   type ToIndex Delta = Idx.Delta
   type FromIndex Idx.Delta = Delta
   indices =
      Delta {delta = Idx.Delta, before = Idx.Before, after = Idx.After}
   access idx =
      case idx of
         Idx.Delta  -> Accessor.fromSetGet (\a d -> d{delta  = a}) delta
         Idx.Before -> Accessor.fromSetGet (\a d -> d{before = a}) before
         Idx.After  -> Accessor.fromSetGet (\a d -> d{after  = a}) after
