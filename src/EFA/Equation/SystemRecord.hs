{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module EFA.Equation.SystemRecord where

import qualified EFA.Flow.Topology.Index as Idx

import qualified EFA.Equation.Record as Record
import qualified EFA.Equation.Arithmetic as Arith
import qualified EFA.Equation.Mix as Mix
import EFA.Equation.Result (Result)
import EFA.Equation.Arithmetic
          (Sum, (~+), (~-),
           Product, (~*), (~/),
           Constant, zero,
           Integrate, Scalar, integrate,
           Scale, scale)

import qualified EFA.Utility.FixedLength as FixedLength
import EFA.Utility ((>>!))

import qualified UniqueLogic.ST.TF.Expression as Expr
import qualified UniqueLogic.ST.TF.Rule as Rule
import qualified UniqueLogic.ST.TF.System as Sys
import UniqueLogic.ST.TF.Expression ((=:=))

import Control.Applicative (Applicative, pure, liftA2)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Foldable as Fold
import qualified Data.Monoid.HT as MonoidHT
import Data.Traversable (Traversable, sequenceA)
import Data.Foldable (Foldable, foldMap)
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


class
   (Traversable rec, Applicative rec, Record.IndexSet rec) =>
      Record rec where
   rules ::
      (Sys.Value mode a, Sum a) =>
      Variable mode rec s a -> System mode s
   mixSumRules ::
      (Sys.Value mode a, Sum a) =>
      rec (Expr.T mode s a) -> System mode s
   mixLevelRules ::
      (Sys.Value mode a, Sum a) =>
      Maybe Idx.Direction ->
      rec (Expr.T mode s a) -> System mode s

   equalR ::
      (Sys.Value mode a) =>
      rec (Expr.T mode s a) ->
      rec (Expr.T mode s a) ->
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
   mixSumRules _ = mempty
   mixLevelRules _ _ = mempty

   equalR (Record.Absolute x) (Record.Absolute y) =
      System (x =:= y)

   liftR0 = Record.Absolute

   liftR1 f (Record.Absolute x) = Record.Absolute $ f x

   liftR2 f (Record.Absolute x) (Record.Absolute y) =
      Record.Absolute $ f x y


instance Record Record.Delta where

   rules vars = System $
      Arith.ruleAdd (Record.before vars) (Record.delta vars) (Record.after vars)
   mixSumRules _ = mempty
   mixLevelRules _ _ = mempty

   {-
   I omit equality on the delta part since it would be redundant.
   -}
   equalR recX recY =
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
   Record.ExtDelta $ fmap unwrap $ Record.deltaCons (Wrap b) (Wrap a)


instance (Record rec) => Record (Record.ExtDelta rec) where

   rules (Record.ExtDelta vars) =
      foldMap rules vars <>
      foldMap rules (sequenceA vars)

   mixSumRules (Record.ExtDelta vars) =
      foldMap mixSumRules vars

   mixLevelRules dir (Record.ExtDelta vars) =
      foldMap (mixLevelRules dir) vars

   {-
   I omit equality on the delta part since it would be redundant.
   -}
   equalR recX recY =
      equalR (Record.extBefore recX) (Record.extBefore recY) <>
      equalR (Record.extAfter  recX) (Record.extAfter  recY)

   liftR0 x = extDeltaCons (liftR0 x) (liftR0 x)

   liftR1 f rec =
      extDeltaCons
         (liftR1 f $ Record.extBefore rec)
         (liftR1 f $ Record.extAfter rec)

   liftR2 f recX recY =
      extDeltaCons
         (liftR2 f (Record.extBefore recX) (Record.extBefore recY))
         (liftR2 f (Record.extAfter  recX) (Record.extAfter  recY))


newtype
   MatchMixDir f a dir =
      MatchMixDir {getMatchMixDir :: Record.Mix dir f a -> Bool}

matchMixDir ::
   Mix.Direction dir =>
   Maybe Idx.Direction -> Record.Mix dir f a -> Bool
matchMixDir dir =
   getMatchMixDir $
   Mix.switch
      (MatchMixDir $ \_ -> dir /= Just Idx.In)
      (MatchMixDir $ \_ -> dir /= Just Idx.Out)

instance (Mix.Direction dir, FixedLength.C f) => Record (Record.Mix dir f) where

   rules _ = mempty

   mixSumRules (Record.Mix s (NonEmpty.Cons p ps)) =
      System (s =:= Fold.foldl (~+) p (FixedLength.Wrap ps))

   mixLevelRules dir r@(Record.Mix s ps) =
      MonoidHT.when (matchMixDir dir r) $
      foldMap (\p -> System (s =:= p)) (FixedLength.Wrap ps)

   equalR recX recY =
      foldMap System $ liftA2 (=:=) recX recY

   liftR0 x = pure x

   liftR1 f rec = fmap f rec

   liftR2 f recX recY = liftA2 f recX recY


instance
   (Mix.Direction dir, Record rec, FixedLength.C f) =>
      Record (Record.ExtMix dir f rec) where

   rules (Record.ExtMix m) = foldMap rules m

   mixSumRules (Record.ExtMix m) =
      foldMap mixSumRules m <>
      foldMap mixSumRules (sequenceA m)

   mixLevelRules dir (Record.ExtMix m) =
      foldMap (mixLevelRules dir) m <>
      foldMap (mixLevelRules dir) (sequenceA m)

   equalR (Record.ExtMix x) (Record.ExtMix y) =
      Fold.fold (liftA2 equalR x y)

   liftR0 x = pure x

   liftR1 f rec = fmap f rec

   liftR2 f recX recY = liftA2 f recX recY


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


equal ::
   (Record rec, Sys.Value mode a) =>
   Expr mode rec s a ->
   Expr mode rec s a ->
   System mode s
equal (Wrap recX) (Wrap recY) = equalR recX recY

equalResult ::
   (Record rec, Sys.Value mode a) =>
   Variable mode rec s a -> rec (Result a) ->
   System mode s
equalResult evar val =
   Fold.fold $
   liftA2
      (\var -> foldMap (\x -> System $ Rule.equ var =<< Sys.constant x))
      evar val
