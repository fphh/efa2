{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.Stack where

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.MultiValue as MV
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~-))

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Data.NonEmpty as NonEmpty

import Control.Applicative (liftA2)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))
import Data.Tuple.HT (mapFst)


{- |
The length of the list must match the depth of the tree.
The indices must be in strictly ascending order.
-}
data Stack i a = Stack [i] (Sum a)
   deriving (Show)

data Sum a =
     Value a
   | Plus (Sum a) (Sum a)
   deriving (Show)

instance Functor Sum where
   fmap f (Value a) = Value (f a)
   fmap f (Plus a0 a1) = Plus (fmap f a0) (fmap f a1)

instance Foldable Sum where
   foldMap f = fold (<>) . fmap f


instance FormatValue a => FormatValue (Stack i a) where
   formatValue (Stack _ s) = formatValue s

instance FormatValue a => FormatValue (Sum a) where
   formatValue = fold Format.plus . fmap formatValue


toList :: Sum a -> NonEmpty.T [] a
toList = fold NonEmpty.append . fmap NonEmpty.singleton

{- |
You may use 'Data.Foldable.sum' for evaluation with respect to 'Num' class.
-}
evaluate :: Arith.Sum a => Sum a -> a
evaluate = fold (~+)

fold :: (a -> a -> a) -> Sum a -> a
fold op =
   let go (Value a) = a
       go (Plus a d) = go a `op` go d
   in  go


{- |
Generate a Sum from a MultiValue
representing the right-most value from the MultiValue
where the first summand is the left-most value from the MultiValue.
-}
fromMultiValue :: Arith.Sum a => MV.MultiValue i a -> Stack i a
fromMultiValue (MV.MultiValue indices tree) =
   let go (MV.Leaf a) = Value a
       go (MV.Branch a0 a1) =
          Plus (go a0) (go $ liftA2 (~-) a1 a0)
   in  Stack indices $ go tree

assigns :: Stack i a -> NonEmpty.T [] ([Idx.Record Idx.Delta i], a)
assigns (Stack [] (Value a)) = NonEmpty.singleton ([], a)
assigns (Stack (i:is) (Plus a0 a1)) =
   NonEmpty.append
      (fmap (mapFst (Idx.before i :)) $ assigns $ Stack is a0)
      (fmap (mapFst (Idx.delta  i :)) $ assigns $ Stack is a1)
assigns _ = error "Stack.assigns: inconsistent data"
