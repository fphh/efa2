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

type Variable mode rec s x = rec (Sys.Variable mode s x)


newtype System mode s = System (Sys.T mode s ())

instance Monoid (System mode s) where
   mempty = System $ return ()
   mappend (System x) (System y) = System $ x >>! y


type Expr mode = Expr.T mode


class (Traversable rec, Applicative rec, Record.IndexSet rec) => Record rec where
   rules ::
      (Sys.Value mode a, Sum a) =>
      Variable mode rec s a -> System mode s
   equal ::
      (Sys.Value mode a) =>
      Wrap rec (Expr mode s a) ->
      Wrap rec (Expr mode s a) ->
      System mode s
   lift0 :: (Sum x) => x -> Wrap rec x
   lift1 ::
      (Sum y) =>
      (x -> y) ->
      Wrap rec x -> Wrap rec y
   lift2 ::
      (Sum z) =>
      (x -> y -> z) ->
      Wrap rec x -> Wrap rec y -> Wrap rec z


instance Record Record.Absolute where

   rules _ = mempty

   equal (Wrap (Record.Absolute x)) (Wrap (Record.Absolute y)) =
      System (x =:= y)

   lift0 = Wrap . Record.Absolute

   lift1 f (Wrap (Record.Absolute x)) = Wrap $ Record.Absolute $ f x

   lift2 f (Wrap (Record.Absolute x)) (Wrap (Record.Absolute y)) =
      Wrap $ Record.Absolute $ f x y


instance Record Record.Delta where

   rules vars = System $
      Arith.ruleAdd (Record.before vars) (Record.delta vars) (Record.after vars)

   {-
   I omit equality on the delta part since it would be redundant.
   -}
   equal (Wrap recX) (Wrap recY) =
      System (Record.before recX =:= Record.before recY) <>
      System (Record.after  recX =:= Record.after  recY)

   lift0 x = Wrap $ Record.deltaCons x x

   lift1 f (Wrap rec) =
      Wrap $
      Record.deltaCons (f $ Record.before rec) (f $ Record.after rec)

   lift2 f (Wrap recX) (Wrap recY) =
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

   rules vars =
      rules (Record.extBefore vars) <>
      rules (Record.extDelta vars) <>
      rules (Record.extAfter vars) <>
      (System $ Fold.sequence_ $
         liftA3 Arith.ruleAdd
            (Record.extBefore vars)
            (Record.extDelta vars)
            (Record.extAfter vars))

   {-
   I omit equality on the delta part since it would be redundant.
   -}
   equal (Wrap recX) (Wrap recY) =
      equal (Wrap $ Record.extBefore recX) (Wrap $ Record.extBefore recY) <>
      equal (Wrap $ Record.extAfter  recX) (Wrap $ Record.extAfter  recY)

   lift0 x = Wrap $ extDeltaCons (lift0 x) (lift0 x)

   lift1 f (Wrap rec) =
      Wrap $
      extDeltaCons
         (lift1 f $ Wrap $ Record.extBefore rec)
         (lift1 f $ Wrap $ Record.extAfter rec)

   lift2 f (Wrap recX) (Wrap recY) =
      Wrap $
      extDeltaCons
         (lift2 f (Wrap $ Record.extBefore recX) (Wrap $ Record.extBefore recY))
         (lift2 f (Wrap $ Record.extAfter  recX) (Wrap $ Record.extAfter  recY))


instance (Record rec, Sum a) => Sum (Wrap rec a) where
   (~+) = lift2 (~+)
   (~-) = lift2 (~-)
   negate = lift1 Arith.negate

instance (Record rec, Product a) => Product (Wrap rec a) where
   (~*) = lift2 (~*)
   (~/) = lift2 (~/)
   recip = lift1 Arith.recip
   constOne = lift1 Arith.constOne

instance (Record rec, Constant a) => Constant (Wrap rec a) where
   zero = pure zero
   fromInteger  = lift0 . Arith.fromInteger
   fromRational = lift0 . Arith.fromRational

instance
   (Record rec, Integrate v, Sum (Scalar v)) =>
      Integrate (Wrap rec v) where
   type Scalar (Wrap rec v) = Wrap rec (Scalar v)
   integrate = lift1 integrate
