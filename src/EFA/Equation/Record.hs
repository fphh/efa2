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
import Data.Monoid ((<>))

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


data ExtDelta f a = ExtDelta {extDelta, extBefore, extAfter :: f a} deriving (Show)

extDeltaCons :: Sum (f a) => f a -> f a -> ExtDelta f a
extDeltaCons b a = ExtDelta {extBefore = b, extAfter = a, extDelta = a~-b}

instance FormatValue (f a) => FormatValue (ExtDelta f a) where
   formatValue rec =
      Format.list $
         Format.assign (Format.literal "delta")  (formatValue $ extDelta rec) :
         Format.assign (Format.literal "before") (formatValue $ extBefore rec) :
         Format.assign (Format.literal "after")  (formatValue $ extAfter rec) :
         []

instance Functor f => Functor (ExtDelta f) where
   fmap f (ExtDelta d b a) = ExtDelta (fmap f d) (fmap f b) (fmap f a)

instance (Applicative f) => Applicative (ExtDelta f) where
   pure a = ExtDelta (pure a) (pure a) (pure a)
   ExtDelta fd fb fa <*> ExtDelta d b a =
      ExtDelta (fd <*> d) (fb <*> b) (fa <*> a)

instance (Foldable f) => Foldable (ExtDelta f) where
   foldMap f (ExtDelta d b a) =
      foldMap f d <> foldMap f b <> foldMap f a

instance (Traversable f) => Traversable (ExtDelta f) where
   sequenceA (ExtDelta d b a) =
      liftA3 ExtDelta (sequenceA d) (sequenceA b) (sequenceA a)



class
   (Ord (ToIndex rec), Functor rec, Format.Record (ToIndex rec),
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

instance C rec => C (ExtDelta rec) where
   type ToIndex (ExtDelta rec) = Idx.ExtDelta (ToIndex rec)
   type FromIndex (Idx.ExtDelta idx) = ExtDelta (FromIndex idx)
   indices =
      ExtDelta {
         extDelta  = fmap (Idx.ExtDelta Idx.Delta)  indices,
         extBefore = fmap (Idx.ExtDelta Idx.Before) indices,
         extAfter  = fmap (Idx.ExtDelta Idx.After)  indices
      }
   access (Idx.ExtDelta idx sub) =
      case idx of
         Idx.Delta  -> access sub . Accessor.fromSetGet (\a d -> d{extDelta  = a}) extDelta
         Idx.Before -> access sub . Accessor.fromSetGet (\a d -> d{extBefore = a}) extBefore
         Idx.After  -> access sub . Accessor.fromSetGet (\a d -> d{extAfter  = a}) extAfter


class C rec => Summands rec where
   {- |
   This method fetches only the before and delta components,
   in contrast to Fold.toList.
   -}
   summands :: rec a -> [a]

instance Summands Absolute where
   summands (Absolute x) = [x]

instance Summands Delta where
   summands r = [before r, delta r]

instance Summands rec => Summands (ExtDelta rec) where
   summands r = summands (extBefore r) ++ summands (extDelta r)
