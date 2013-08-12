{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Equation.SystemRecord where

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, Scalar, integrate)

import EFA.Utility ((>>!))

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.System as Sys
import UniqueLogic.ST.TF.Expression ((=:=))

import Control.Applicative (Applicative, pure, liftA3)
import Control.Category ((.))

import qualified Data.Foldable as Fold
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid, (<>), mempty, mappend)

import qualified Prelude as P
import Prelude hiding (lookup, init, sqrt, (.))


newtype Wrap rec a = Wrap {unwrap :: rec a}
   deriving (Functor, Applicative, Foldable, Traversable)

type RecordVariable mode rec s x = rec (Sys.Variable mode s x)


newtype System mode s = System (Sys.T mode s ())

instance Monoid (System mode s) where
   mempty = System $ return ()
   mappend (System x) (System y) = System $ x >>! y


type Expr mode = Expr.T mode


class (Traversable rec, Applicative rec, Record.IndexSet rec) => Record rec where
   recordRules ::
      (Sys.Value mode a, Sum a) =>
      RecordVariable mode rec s a -> System mode s
   equalRecord ::
      (Sys.Value mode a) =>
      Wrap rec (Expr mode s a) ->
      Wrap rec (Expr mode s a) ->
      System mode s
   liftE0 :: (Sum x) => x -> Wrap rec x
   liftE1 ::
      (Sum y) =>
      (x -> y) ->
      Wrap rec x -> Wrap rec y
   liftE2 ::
      (Sum z) =>
      (x -> y -> z) ->
      Wrap rec x -> Wrap rec y -> Wrap rec z


instance Record Record.Absolute where

   recordRules _ = mempty

   equalRecord (Wrap (Record.Absolute x)) (Wrap (Record.Absolute y)) =
      System (x =:= y)

   liftE0 = Wrap . Record.Absolute

   liftE1 f (Wrap (Record.Absolute x)) = Wrap $ Record.Absolute $ f x

   liftE2 f (Wrap (Record.Absolute x)) (Wrap (Record.Absolute y)) =
      Wrap $ Record.Absolute $ f x y


instance Record Record.Delta where

   recordRules vars = System $
      Arith.ruleAdd (Record.before vars) (Record.delta vars) (Record.after vars)

   {-
   I omit equality on the delta part since it would be redundant.
   -}
   equalRecord (Wrap recX) (Wrap recY) =
      System (Record.before recX =:= Record.before recY) <>
      System (Record.after  recX =:= Record.after  recY)

   liftE0 x = Wrap $ Record.deltaCons x x

   liftE1 f (Wrap rec) =
      Wrap $
      Record.deltaCons (f $ Record.before rec) (f $ Record.after rec)

   liftE2 f (Wrap recX) (Wrap recY) =
      Wrap $
      Record.deltaCons
         (f (Record.before recX) (Record.before recY))
         (f (Record.after  recX) (Record.after  recY))


extDeltaCons ::
   (Record f, Sum a) => Wrap f a -> Wrap f a -> Record.ExtDelta f a
extDeltaCons b a =
   Record.ExtDelta {
      Record.extBefore = unwrap b,
      Record.extAfter = unwrap a,
      Record.extDelta = unwrap (a ~- b)
   }


instance (Record rec) => Record (Record.ExtDelta rec) where

   recordRules vars =
      recordRules (Record.extBefore vars) <>
      recordRules (Record.extDelta vars) <>
      recordRules (Record.extAfter vars) <>
      (System $ Fold.sequence_ $
         liftA3 Arith.ruleAdd
            (Record.extBefore vars)
            (Record.extDelta vars)
            (Record.extAfter vars))

   {-
   I omit equality on the delta part since it would be redundant.
   -}
   equalRecord (Wrap recX) (Wrap recY) =
      equalRecord (Wrap $ Record.extBefore recX) (Wrap $ Record.extBefore recY) <>
      equalRecord (Wrap $ Record.extAfter  recX) (Wrap $ Record.extAfter  recY)

   liftE0 x = Wrap $ extDeltaCons (liftE0 x) (liftE0 x)

   liftE1 f (Wrap rec) =
      Wrap $
      extDeltaCons
         (liftE1 f $ Wrap $ Record.extBefore rec)
         (liftE1 f $ Wrap $ Record.extAfter rec)

   liftE2 f (Wrap recX) (Wrap recY) =
      Wrap $
      extDeltaCons
         (liftE2 f (Wrap $ Record.extBefore recX) (Wrap $ Record.extBefore recY))
         (liftE2 f (Wrap $ Record.extAfter  recX) (Wrap $ Record.extAfter  recY))


instance (Record rec, Sum a) => Sum (Wrap rec a) where
   (~+) = liftE2 (~+)
   (~-) = liftE2 (~-)
   negate = liftE1 Arith.negate

instance (Record rec, Product a) => Product (Wrap rec a) where
   (~*) = liftE2 (~*)
   (~/) = liftE2 (~/)
   recip = liftE1 Arith.recip
   constOne = liftE1 Arith.constOne

instance (Record rec, Constant a) => Constant (Wrap rec a) where
   zero = pure zero
   fromInteger  = liftE0 . Arith.fromInteger
   fromRational = liftE0 . Arith.fromRational

instance
   (Record rec, Integrate v, Sum (Scalar v)) =>
      Integrate (Wrap rec v) where
   type Scalar (Wrap rec v) = Wrap rec (Scalar v)
   integrate = liftE1 integrate
