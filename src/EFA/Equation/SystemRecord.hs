{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Equation.SystemRecord where

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Result (Result)
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, Scalar, integrate,
           Scale, scale)

import EFA.Utility ((>>!))

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.Rule as Rule
import qualified UniqueLogic.ST.TF.System as Sys
import UniqueLogic.ST.TF.Expression ((=:=))

import Control.Applicative (Applicative, pure, liftA2, liftA3)

import qualified Data.Foldable as Fold
import Data.Traversable (Traversable)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid, (<>), mempty, mappend)


newtype Wrap rec a = Wrap {unwrap :: rec a}
   deriving (Functor, Applicative, Foldable, Traversable)

type Variable mode rec s x = rec (Sys.Variable mode s x)

type Expr mode rec s x = Wrap rec (Expr.T mode s x)


exprFromVariable ::
   (Record rec) =>
   Variable mode rec s x -> Expr mode rec s x
exprFromVariable = Wrap . fmap Expr.fromVariable


newtype System mode s = System (Sys.T mode s ())

instance Monoid (System mode s) where
   mempty = System $ return ()
   mappend (System x) (System y) = System $ x >>! y


lift0 :: (Record rec, Sum x) => x -> Wrap rec x
lift0 = Wrap . liftR0

lift1 ::
   (Record rec, Sum y) =>
   (x -> y) ->
   Wrap rec x -> Wrap rec y
lift1 f (Wrap x) = Wrap $ liftR1 f x

lift2 ::
   (Record rec, Sum z) =>
   (x -> y -> z) ->
   Wrap rec x -> Wrap rec y -> Wrap rec z
lift2 f (Wrap x) (Wrap y) = Wrap $ liftR2 f x y


class (Traversable rec, Applicative rec, Record.IndexSet rec) => Record rec where
   rules ::
      (Sys.Value mode a, Sum a) =>
      Variable mode rec s a -> System mode s
   equal ::
      (Sys.Value mode a) =>
      Expr mode rec s a ->
      Expr mode rec s a ->
      System mode s
   liftR0 :: (Sum x) => x -> rec x
   liftR1 ::
      (Sum y) =>
      (x -> y) ->
      rec x -> rec y
   liftR2 ::
      (Sum z) =>
      (x -> y -> z) ->
      rec x -> rec y -> rec z


instance Record Record.Absolute where

   rules _ = mempty

   equal (Wrap (Record.Absolute x)) (Wrap (Record.Absolute y)) =
      System (x =:= y)

   liftR0 = Record.Absolute

   liftR1 f (Record.Absolute x) = Record.Absolute $ f x

   liftR2 f (Record.Absolute x) (Record.Absolute y) =
      Record.Absolute $ f x y


instance Record Record.Delta where

   rules vars = System $
      Arith.ruleAdd (Record.before vars) (Record.delta vars) (Record.after vars)

   {-
   I omit equality on the delta part since it would be redundant.
   -}
   equal (Wrap recX) (Wrap recY) =
      System (Record.before recX =:= Record.before recY) <>
      System (Record.after  recX =:= Record.after  recY)

   liftR0 x = Record.deltaCons x x

   liftR1 f rec =
      Record.deltaCons (f $ Record.before rec) (f $ Record.after rec)

   liftR2 f recX recY =
      Record.deltaCons
         (f (Record.before recX) (Record.before recY))
         (f (Record.after  recX) (Record.after  recY))


extDeltaCons ::
   (Record rec, Sum a) => rec a -> rec a -> Record.ExtDelta rec a
extDeltaCons b a =
   Record.ExtDelta {
      Record.extBefore = b,
      Record.extAfter = a,
      Record.extDelta = liftR2 (~-) a b
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

   liftR0 x = extDeltaCons (liftR0 x) (liftR0 x)

   liftR1 f rec =
      extDeltaCons
         (liftR1 f $ Record.extBefore rec)
         (liftR1 f $ Record.extAfter rec)

   liftR2 f recX recY =
      extDeltaCons
         (liftR2 f (Record.extBefore recX) (Record.extBefore recY))
         (liftR2 f (Record.extAfter  recX) (Record.extAfter  recY))


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

instance
   (Record rec, Scale v, Sum (Scalar v), Sum v) =>
      Scale (Wrap rec v) where
   scale = lift2 scale


equalResult ::
   (Record rec, Sys.Value mode a) =>
   Variable mode rec s a -> rec (Result a) ->
   System mode s
equalResult evar val =
   Fold.fold $
   liftA2
      (\var -> Fold.foldMap (\x -> System $ Rule.equ var =<< Sys.constant x))
      evar val
