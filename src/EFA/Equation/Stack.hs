{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.Stack where

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.MultiValue as MV
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~-))

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Data.NonEmpty as NonEmpty

import qualified Test.QuickCheck as QC

import Control.Applicative (liftA2)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))
import Data.Tuple.HT (mapFst)


{- |
The length of the list must match the depth of the tree.
The indices must be in strictly ascending order.
-}
data Stack i a = Stack [i] (Sum a)
   deriving (Show, Eq)

data Sum a =
     Value a
   | Plus (Sum a) (Sum a)
   deriving (Show, Eq)

instance Functor Sum where
   fmap f (Value a) = Value (f a)
   fmap f (Plus a0 a1) = Plus (fmap f a0) (fmap f a1)

instance Foldable Sum where
   foldMap f = fold (<>) . fmap f


instance FormatValue a => FormatValue (Stack i a) where
   formatValue (Stack _ s) = formatValue s

instance FormatValue a => FormatValue (Sum a) where
   formatValue = fold Format.plus . fmap formatValue


descent :: Stack i a -> Either a (i, (Stack i a, Stack i a))
descent (Stack [] (Value a)) = Left a
descent (Stack (i:is) (Plus a0 a1)) = Right (i, (Stack is a0, Stack is a1))
descent _ = error "Stack.descent: inconsistent data structure"


eqRelaxed :: (Ord i, Eq a, Num a) => Stack i a -> Stack i a -> Bool
eqRelaxed =
   let go a b =
          case (descent a, descent b) of
             (Left av, Left bv) -> av==bv
             (Left _, Right (_, (b0, b1))) -> go a b0 && go 0 b1
             (Right (_, (a0, a1)), Left _) -> go a0 b && go a1 0
             (Right (i, (a0, a1)), Right (j, (b0, b1))) ->
                case compare i j of
                   EQ -> go a0 b0 && go a1 b1
                   LT -> go a0 b && go a1 0
                   GT -> go a b0 && go 0 b1
   in  go


instance (Ord i, Num a) => Num (Stack i a) where
   fromInteger = singleton . fromInteger
   negate (Stack is s) = Stack is $ fmap negate s

   x0@(Stack is0 _) + y0@(Stack js0 _) =
      let go a b =
             case (descent a, descent b) of
                (Left av, Left bv) -> Value $ av+bv
                (Left _, Right (_, (b0, Stack _ b1))) -> Plus (go a b0) b1
                (Right (_, (a0, Stack _ a1)), Left _) -> Plus (go a0 b) a1
                (Right (i, (a0, a1)), Right (j, (b0, b1))) ->
                   case compare i j of
                      EQ -> Plus (go a0 b0) (go a1 b1)
                      LT -> Plus (go a0 b) (go a1 (zeroStack b))
                      GT -> Plus (go a b0) (go (zeroStack a) b1)
      in  Stack (MV.mergeIndices is0 js0) $ go x0 y0

   x0@(Stack is0 _) * y0@(Stack js0 _) =
      let go a b =
             case (descent a, descent b) of
                (Left av, _) -> case b of Stack _ bs -> fmap (av*) bs
                (_, Left bv) -> case a of Stack _ as -> fmap (*bv) as
                (Right (i, (a0, a1)), Right (j, (b0, b1))) ->
                   case compare i j of
                      EQ -> Plus (go a0 b0) (addMatch (go a0 b1) $ go a1 (b0+b1))
--                      EQ -> Plus (go a0 b0) (addMatch (go a0 b1) $ addMatch (go a1 b0) $ go b0 b1)
                      LT -> Plus (go a0 b) (go a1 b)
                      GT -> Plus (go a b0) (go a b1)
      in  Stack (MV.mergeIndices is0 js0) $ go x0 y0

   abs = fromMultiValueNum . abs . toMultiValueNum
   signum = fromMultiValueNum . signum . toMultiValueNum


zeroStack :: (Num a) => Stack i a -> Stack i a
zeroStack (Stack is a) = Stack is $ zeroMatch a

zeroMatch :: (Num a) => Sum a -> Sum a
zeroMatch (Value _) = Value 0
zeroMatch (Plus x0 x1) = Plus (zeroMatch x0) (zeroMatch x1)

addMatch :: (Num a) => Sum a -> Sum a -> Sum a
addMatch (Value x) (Value y) = Value (x+y)
addMatch (Plus x0 x1) (Plus y0 y1) = Plus (addMatch x0 y0) (addMatch x1 y1)
addMatch _ _ = error "Stack.addMatch: inconsistent data structure"

mulMatch :: (Num a) => Sum a -> Sum a -> Sum a
mulMatch (Value x) (Value y) = Value (x*y)
mulMatch (Plus x0 x1) (Plus y0 y1) =
   Plus (mulMatch x0 y0)
      (addMatch (mulMatch x0 y1) $ mulMatch x1 $ addMatch y0 y1)
mulMatch _ _ = error "Stack.mulMatch: inconsistent data structure"

instance (Ord i, Fractional a) => Fractional (Stack i a) where
   fromRational = singleton . fromRational
   recip (Stack is s) =
      let go (Value a) = Value $ recip a
          go (Plus a d) =
             let ra = go a
                 rd = go (addMatch a d)
             in  Plus ra (fmap negate $ mulMatch d $ mulMatch ra rd)
          -- 1/(a+d) - 1/a = -d/(a*(a+d))
      in  Stack is $ go s


singleton :: a -> Stack i a
singleton = Stack [] . Value

deltaPair :: i -> a -> a -> Stack i a
deltaPair i a d = Stack [i] $ Plus (Value a) (Value d)


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
fromMultiValue = fromMultiValueGen (~-)

toMultiValue :: Arith.Sum a => Stack i a -> MV.MultiValue i a
toMultiValue = toMultiValueGen (~+)


fromMultiValueNum :: Num a => MV.MultiValue i a -> Stack i a
fromMultiValueNum = fromMultiValueGen (-)

toMultiValueNum :: Num a => Stack i a -> MV.MultiValue i a
toMultiValueNum = toMultiValueGen (+)


fromMultiValueGen :: (a -> a -> a) -> MV.MultiValue i a -> Stack i a
fromMultiValueGen minus (MV.MultiValue indices tree) =
   let go (MV.Leaf a) = Value a
       go (MV.Branch a0 a1) =
          Plus (go a0) (go $ liftA2 minus a1 a0)
   in  Stack indices $ go tree

toMultiValueGen :: (a -> a -> a) -> Stack i a -> MV.MultiValue i a
toMultiValueGen plus (Stack indices tree) =
   let go (Value a) = MV.Leaf a
       go (Plus a0 a1) =
          MV.Branch (go a0) (liftA2 plus (go a1) (go a0))
   in  MV.MultiValue indices $ go tree


assigns :: Stack i a -> NonEmpty.T [] ([Idx.Record Idx.Delta i], a)
assigns (Stack [] (Value a)) = NonEmpty.singleton ([], a)
assigns (Stack (i:is) (Plus a0 a1)) =
   NonEmpty.append
      (fmap (mapFst (Idx.before i :)) $ assigns $ Stack is a0)
      (fmap (mapFst (Idx.delta  i :)) $ assigns $ Stack is a1)
assigns _ = error "Stack.assigns: inconsistent data"


instance
   (QC.Arbitrary i, Ord i, QC.Arbitrary a, Arith.Sum a) =>
      QC.Arbitrary (Stack i a) where
   arbitrary = fmap fromMultiValue QC.arbitrary
