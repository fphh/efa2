{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
module EFA.Equation.StackConsistent where

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.MultiValue as MV
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~/))
import EFA.Utility (differenceMapSet)

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Test.QuickCheck as QC

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import qualified Data.Foldable as Fold
import Control.Applicative (Applicative, pure, liftA2, (<*>))
import Data.NonEmpty ((!:))
import Data.Map (Map)
import Data.Traversable (sequenceA)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))
import Data.Tuple.HT (mapFst, mapSnd)
import Data.Maybe.HT (toMaybe)

import qualified Prelude as P
import Prelude hiding (recip, filter)


{- |
This data structure is the same as in "EFA.Equation.StackConsistent"
but we assert statically
that the length of the list matches the depth of the tree.

The indices must be in strictly ascending order.
Unfortunately, we cannot assert this statically.
-}
data Stack i a = forall sum. Sum sum => Stack (List sum i) (sum a)

data ExStack sum i a = ExStack (List sum i) (sum a)

data Stack2 i a = forall sum. Sum sum => Stack2 (List sum i) (sum a) (sum a)

data ExStack2 sum i a = ExStack2 (List sum i) (sum a) (sum a)


wrapStack :: Sum sum => ExStack sum i a -> Stack i a
wrapStack (ExStack is s) = Stack is s


newtype Value a = Value a deriving (Show, Eq)
data Plus sum a = Plus (sum a) (sum a) deriving (Eq)

instance NonEmptyC.Show Value where
   showsPrec = showsPrec

instance NonEmptyC.Show sum => NonEmptyC.Show (Plus sum) where
   showsPrec prec (Plus a d) =
      showParen (prec>10) $
         showString "Plus " . NonEmptyC.showsPrec 11 a .
         showString " " . NonEmptyC.showsPrec 11 d

instance (Show i, Show a) => Show (Stack i a) where
   showsPrec prec (Stack is s) =
      showParen (prec>10) $
         showString "Stack " . NonEmptyC.showsPrec 11 is .
         showString " " . NonEmptyC.showsPrec 11 s


class
   (NonEmptyC.Show (List sum), NonEmptyC.Show sum,
    Functor (List sum), Applicative sum,
    Foldable (List sum), NonEmpty.RemoveEach (List sum)) =>
      Sum sum where
   type List sum :: * -> *
   descentCore :: List sum i -> sum a -> Either a (i, Stack2 i a)
   fold :: (a -> a -> a) -> sum a -> a
   mulMatch :: (a -> a -> a) -> (a -> a -> a) -> sum a -> sum a -> sum a
   filterMask ::
      (Ord i) => Map i Branch -> ExStack sum i a -> FilterMask sum
   fillMask ::
      (Ord i, Sum rsum) => ExStack sum i a -> ExStack rsum i a -> FillMask sum rsum i
   fillValueMask ::
      (Ord i) => ExStack Value i a -> ExStack sum i a -> FillMask Value sum i
   fillPlusMask ::
      (Ord i, Sum lsum) => ExStack (Plus lsum) i a -> ExStack sum i a -> FillMask (Plus lsum) sum i
   shrinkStack :: ExStack sum i a -> [Stack i a]
   shrinkValues ::
      (QC.Arbitrary a) => ExStack sum i a -> [ExStack sum i a]

instance Sum Value where
   type List Value = NonEmpty.Empty
   descentCore NonEmpty.Empty (Value a) = Left a
   fold _ (Value a) = a
   mulMatch times _plus (Value x) (Value y) = Value (times x y)
   filterMask _cond _s = FilterMask TakeStop
   fillMask = fillValueMask
   fillValueMask _l _r = FillMask NonEmpty.Empty FillStop FillStop
   fillPlusMask (ExStack (NonEmpty.Cons i is) (Plus a0 _a1)) r =
      case fillMask (ExStack is a0) r of
         FillMask js lmask rmask ->
            FillMask (i!:js) (FillTake lmask) (FillSkip rmask)
   shrinkStack _ = []
   shrinkValues (ExStack empty (Value x)) =
      map (ExStack empty . Value) $ QC.shrink x

instance (Sum sum) => Sum (Plus sum) where
   type List (Plus sum) = NonEmpty.T (List sum)
   descentCore (NonEmpty.Cons i is) (Plus a0 a1) =
      Right (i, (Stack2 is a0 a1))
   fold op (Plus a d) = fold op a `op` fold op d

   mulMatch times plus (Plus x0 x1) (Plus y0 y1) =
      Plus (mulMatch times plus x0 y0)
         (liftA2 plus (mulMatch times plus x0 y1) $
          mulMatch times plus x1 $ liftA2 plus y0 y1)

   filterMask cond (ExStack (NonEmpty.Cons i is) (Plus a _d)) =
      case filterMask cond (ExStack is a) of
         FilterMask mask ->
            case Map.lookup i cond of
               Nothing -> FilterMask (TakeAll mask)
               Just branch -> FilterMask (TakeOne branch mask)

   fillMask = fillPlusMask

   fillValueMask l (ExStack (NonEmpty.Cons i is) (Plus a _d)) =
      case fillMask l (ExStack is a) of
         FillMask js lmask rmask ->
            FillMask (i!:js) (FillSkip lmask) (FillTake rmask)
   fillPlusMask a b =
      let (i,a0) = leftStack a
          (j,b0) = leftStack b
      in  case compare i j of
             EQ ->
                case fillMask a0 b0 of
                   FillMask ks lmask rmask ->
                      FillMask (i!:ks) (FillTake lmask) (FillTake rmask)
             LT ->
                case fillMask a0 b  of
                   FillMask ks lmask rmask ->
                      FillMask (i!:ks) (FillTake lmask) (FillSkip rmask)
             GT ->
                case fillMask a  b0 of
                   FillMask ks lmask rmask ->
                      FillMask (j!:ks) (FillSkip lmask) (FillTake rmask)

   shrinkStack (ExStack it (Plus a0 a1)) =
      concatMap (\(_,is) -> [Stack is a0, Stack is a1]) $
      Fold.toList $ NonEmpty.removeEach it

   shrinkValues x =
      case splitPlus x of
         (i, (a0,a1)) ->
            map (flip (exPlus i) a1) (shrinkValues a0)
            ++
            map (exPlus i a0) (shrinkValues a1)

instance Functor (Stack i) where
   fmap f (Stack is s) = Stack is (fmap f s)

instance Functor Value where
   fmap f (Value a) = Value (f a)

instance Functor sum => Functor (Plus sum) where
   fmap f (Plus a0 a1) = Plus (fmap f a0) (fmap f a1)


instance Applicative Value where
   pure = Value
   Value f <*> Value a = Value (f a)

instance Applicative sum => Applicative (Plus sum) where
   pure a = Plus (pure a) (pure a)
   Plus f0 f1 <*> Plus a0 a1 = Plus (f0 <*> a0) (f1 <*> a1)


instance FormatValue a => FormatValue (Stack i a) where
   formatValue (Stack _ s) =
      fold Format.plus . fmap formatValue $ s

{- |
The index function must generate an index list with ascending index order.
That is, it must be monotonic
at least on the indices that are present in the stack.
-}
mapIndicesMonotonic :: (Ord j) => (i -> j) -> Stack i a -> Stack j a
mapIndicesMonotonic g (Stack is s) =
   let js = fmap g is
   in  if ListHT.isAscending $ Fold.toList js
         then Stack js s
         else error "Stack.mapIndicesMonotonic: non-monotonic index function"


descent :: Stack i a -> Either a (i, (Stack i a, Stack i a))
descent (Stack is x) = fmap (mapSnd splitStack2) $ descentCore is x

exDescent :: ExStack (Plus sum) i a -> (i, (ExStack sum i a, ExStack sum i a))
exDescent (ExStack (NonEmpty.Cons i is) (Plus a0 a1)) =
   (i, (ExStack is a0, ExStack is a1))

splitStack2 :: Stack2 i a -> (Stack i a, Stack i a)
splitStack2 (Stack2 is x y) = (Stack is x, Stack is y)

leftStack :: ExStack (Plus sum) i a -> (i, ExStack sum i a)
leftStack (ExStack (NonEmpty.Cons i is) (Plus a0 _a1)) =
   (i, ExStack is a0)

eqRelaxed :: (Ord i, Eq a, Num a) => Stack i a -> Stack i a -> Bool
eqRelaxed =
   let go a b =
          case (descent a, descent b) of
             (Left av, Left bv) -> av==bv
             (Left _, Right (_, (b0, b1))) -> go a b0 && go (singleton 0) b1
             (Right (_, (a0, a1)), Left _) -> go a0 b && go a1 (singleton 0)
             (Right (i, (a0, a1)), Right (j, (b0, b1))) ->
                case compare i j of
                   EQ -> go a0 b0 && go a1 b1
                   LT -> go a0 b && go a1 (singleton 0)
                   GT -> go a b0 && go (singleton 0) b1
   in  go


data FillStop      = FillStop
data FillTake rest = FillTake rest
data FillSkip rest = FillSkip rest

data
   FillMask lsum rsum i =
      forall lmask rmask sum.
         (lsum ~ FillFromSum lmask, sum ~ FillToSum lmask, Fill lmask,
          rsum ~ FillFromSum rmask, sum ~ FillToSum rmask, Fill rmask) =>
         FillMask (List sum i) lmask rmask

class
   (Sum (FillToSum mask), Sum (FillFromSum mask)) => Fill mask where
   type FillFromSum mask :: * -> *
   type FillToSum mask :: * -> *
   fill ::
      (a -> a) ->
      mask ->
      FillFromSum mask a ->
      FillToSum mask a

instance Fill FillStop where
   type FillFromSum FillStop = Value
   type FillToSum FillStop = Value
   fill _clear FillStop (Value x) = Value x

instance Fill mask => Fill (FillTake mask) where
   type FillFromSum (FillTake mask) = Plus (FillFromSum mask)
   type FillToSum (FillTake mask) = Plus (FillToSum mask)
   fill clear (FillTake mask) (Plus x y) =
      Plus (fill clear mask x) (fill clear mask y)

instance Fill mask => Fill (FillSkip mask) where
   type FillFromSum (FillSkip mask) = FillFromSum mask
   type FillToSum (FillSkip mask) = Plus (FillToSum mask)
   fill clear (FillSkip mask) x =
      let y = fill clear mask x
      in  Plus y (fmap clear y)

add ::
   (Ord i) =>
   (a -> a -> a) -> (a -> a) ->
   Stack i a -> Stack i a -> Stack i a
add plus clear (Stack is x0) (Stack js y0) =
   let x = ExStack is x0
       y = ExStack js y0
   in  case fillMask x y of
          FillMask ks lmask rmask ->
             Stack ks (liftA2 plus (fill clear lmask x0) (fill clear rmask y0))


instance (Ord i, Arith.Integrate v) => Arith.Integrate (Stack i v) where
   type Scalar (Stack i v) = Stack i (Arith.Scalar v)
   integrate (Stack is a) = Stack is $ fmap Arith.integrate a


singleton :: a -> Stack i a
singleton = Stack NonEmpty.Empty . Value

deltaPair :: i -> a -> a -> Stack i a
deltaPair i a d = Stack (NonEmpty.singleton i) $ Plus (Value a) (Value d)

exPlus ::
   i ->
   ExStack sum i a ->
   ExStack sum i a ->
   ExStack (Plus sum) i a
exPlus i (ExStack is a) (ExStack _ d) = ExStack (i !: is) (Plus a d)

splitPlus :: ExStack (Plus sum) i a -> (i, (ExStack sum i a, ExStack sum i a))
splitPlus (ExStack (NonEmpty.Cons i is) (Plus a0 a1)) =
   (i, (ExStack is a0, ExStack is a1))

-- could be a simple Semigroup.Foldable.head if it would exist
absolute :: Stack i a -> a
absolute (Stack _ s) = fold const s

normalize :: (Arith.Product a) => Stack i a -> Stack i a
normalize s = fmap (~/ absolute s) s


toList :: Sum sum => sum a -> NonEmpty.T [] a
toList = fold NonEmpty.append . fmap NonEmpty.singleton

{- |
You may use 'Data.Foldable.sum' for evaluation with respect to 'Num' class.
-}
evaluate :: (Sum sum, Arith.Sum a) => sum a -> a
evaluate = fold (~+)


data Branch = Before | Delta deriving (Eq, Show)

instance QC.Arbitrary Branch where
   arbitrary = QC.elements [Before, Delta]


{- |
A filtered stack contains a sub-hypercube of values
and the point from where it was taken from a larger hypercube.
The index sets of the map and the stack must be distinct.
-}
data Filtered i a =
   Filtered {
      filterCorner :: Map i Branch,
      filteredStack :: Stack i a
   } deriving (Show)

startFilter :: Stack i a -> Filtered i a
startFilter = Filtered Map.empty

{- |
With the Map you can choose
whether you want to keep only the Before or only the Delta part of a variable.
A missing entry in the Map means that both branches are maintained.

If an index is in the condition map but not in the stack,
then the result depends on the value in the condition:
Is it Before, then the Stack value will be maintained,
is it Delta, then the Stack value is set to zero.
This is consistent with how missing indices in the Stack
are handled by the arithmetic operations.

If you filter for 'Before' on a certain variable
and then for 'Delta' on the same one,
or vice versa, then you get 'Nothing'.
-}
filter ::
   (Ord i, Arith.Sum a) =>
   Map i Branch -> Filtered i a -> Maybe (Filtered i a)
filter c1 (Filtered c0 s@(Stack is _x)) =
   liftA2 Filtered (mergeConditions c0 c1) $
   fmap
      (\c1' ->
         (if Fold.any (Delta==) $ differenceMapSet c1' $ foldMap Set.singleton is
            then fmap Arith.clear
            else id) $
         filterNaive c1' s)
      (adaptConditions c0 c1)

adaptConditions ::
   (Ord i) => Map i Branch -> Map i Branch -> Maybe (Map i Branch)
adaptConditions c0 c1 =
   fmap (Map.difference c1 c0 <> ) $
   sequenceA $ Map.intersectionWith (\a0 a1 -> toMaybe (a0==a1) Before) c0 c1

mergeConditions ::
   (Ord i) => Map i Branch -> Map i Branch -> Maybe (Map i Branch)
mergeConditions c0 c1 =
   fmap ((Map.difference c0 c1 <> Map.difference c1 c0) <> ) $
   sequenceA $ Map.intersectionWith (\a0 a1 -> toMaybe (a0==a1) a0) c0 c1



data TakeStop      = TakeStop
data TakeAll  rest = TakeAll rest
data TakeOne  rest = TakeOne Branch rest

data
   FilterMask sum =
      forall mask. (sum ~ FilterFromSum mask, Filter mask) =>
         FilterMask mask

class (Sum (FilterToSum mask), Sum (FilterFromSum mask)) => Filter mask where
   type FilterFromSum mask :: * -> *
   type FilterToSum mask :: * -> *
   exFilter ::
      (Ord i) =>
      mask ->
      ExStack (FilterFromSum mask) i a ->
      ExStack (FilterToSum mask) i a

instance Filter TakeStop where
   type FilterFromSum TakeStop = Value
   type FilterToSum TakeStop = Value
   exFilter TakeStop (ExStack is s) = (ExStack is s)

instance Filter mask => Filter (TakeAll mask) where
   type FilterFromSum (TakeAll mask) = Plus (FilterFromSum mask)
   type FilterToSum (TakeAll mask) = Plus (FilterToSum mask)
   exFilter (TakeAll mask) (ExStack (NonEmpty.Cons i is) (Plus a d)) =
      exPlus i (exFilter mask (ExStack is a)) (exFilter mask (ExStack is d))

instance Filter mask => Filter (TakeOne mask) where
   type FilterFromSum (TakeOne mask) = Plus (FilterFromSum mask)
   type FilterToSum (TakeOne mask) = FilterToSum mask
   exFilter (TakeOne branch mask) (ExStack (NonEmpty.Cons _i is) (Plus a d)) =
      case branch of
         Before -> exFilter mask (ExStack is a)
         Delta  -> exFilter mask (ExStack is d)


{- |
The naive implementation ignores indices
that are in the condition map but not in the stack.

Unfortunately, 'filterNaive' does not satisfy simple laws.
This is because it selects not only certain branches
but also re-declares @delta@ branches as @before@ branches.
-}
filterNaive :: (Ord i) => Map i Branch -> Stack i a -> Stack i a
filterNaive cond (Stack is s) =
   case ExStack is s of
      x ->
         case filterMask cond x of
            FilterMask mask -> wrapStack $ exFilter mask x


toMultiValue :: Arith.Sum a => Stack i a -> MV.MultiValue i a
toMultiValue = toMultiValueGen (~+)

toMultiValueNum :: Num a => Stack i a -> MV.MultiValue i a
toMultiValueNum = toMultiValueGen (+)

toMultiValueGen :: (a -> a -> a) -> Stack i a -> MV.MultiValue i a
toMultiValueGen plus (Stack indices tree) =
   MV.MultiValue (Fold.toList indices) $
   fold (\a0 a1 -> MV.Branch a0 (liftA2 plus a1 a0)) $
   fmap MV.Leaf tree


assigns :: Stack i a -> NonEmpty.T [] ([Idx.Record Idx.Delta i], a)
assigns s =
   case descent s of
      Left a -> NonEmpty.singleton ([], a)
      Right (i, (a0,a1)) ->
         NonEmpty.append
            (fmap (mapFst (Idx.before i :)) $ assigns a0)
            (fmap (mapFst (Idx.delta  i :)) $ assigns a1)


instance
   (QC.Arbitrary i, Ord i, QC.Arbitrary a, Arith.Sum a) =>
      QC.Arbitrary (Stack i a) where

   shrink (Stack it tree) =
      shrinkStack (ExStack it tree)
      ++
      map wrapStack (shrinkValues (ExStack it tree))
