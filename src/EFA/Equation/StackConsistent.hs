{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module EFA.Equation.StackConsistent where

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.MultiValueConsistent as MV
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~-), (~*), (~/))
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
import Data.NonEmpty (Empty(Empty), (!:))
import Data.Map (Map)
import Data.Traversable (sequenceA)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))
import Data.Tuple.HT (mapFst)
import Data.Maybe.HT (toMaybe)

import qualified Prelude as P
import Prelude hiding (recip, filter, showsPrec)


{- |
This data structure is the same as in "EFA.Equation.StackConsistent"
but we assert statically
that the length of the list matches the depth of the tree.

The indices must be in strictly ascending order.
Unfortunately, we cannot assert this statically.
-}
data Stack i a = forall idx. List idx => Stack (idx i) (Sum idx a)

data ExStack idx i a = ExStack (idx i) (Sum idx a)


wrapStack :: List idx => ExStack idx i a -> Stack i a
wrapStack (ExStack is s) = Stack is s


newtype Value a = Value a deriving (Show, Eq)
data Plus sum a = Plus (sum a) (sum a) deriving (Eq)

instance NonEmptyC.Show Value where
   showsPrec = P.showsPrec

instance NonEmptyC.Show sum => NonEmptyC.Show (Plus sum) where
   showsPrec prec (Plus a d) =
      showParen (prec>10) $
         showString "Plus " . NonEmptyC.showsPrec 11 a .
         showString " " . NonEmptyC.showsPrec 11 d



showsPrecSum :: (Show i, Show a) => Int -> Stack i a -> ShowS
showsPrecSum prec s =
   case descent s of
      Left a -> P.showsPrec prec a
      Right (_, (a,d)) ->
         showParen (prec>10) $
            showString "Plus " . showsPrecSum 11 a .
            showString " " . showsPrecSum 11 d

instance (Show i, Show a) => Show (Stack i a) where
   showsPrec prec s@(Stack is _) =
      showParen (prec>10) $
         showString "Stack " . NonEmptyC.showsPrec 11 is .
         showString " " . showsPrecSum 11 s


class Applicative sum => SumC sum where
   fold :: (a -> a -> a) -> sum a -> a
   mulMatch :: (a -> a -> a) -> (a -> a -> a) -> sum a -> sum a -> sum a

instance SumC Value where
   fold _ (Value a) = a
   mulMatch times _plus (Value x) (Value y) = Value (times x y)

instance SumC sum => SumC (Plus sum) where
   fold op (Plus a d) = fold op a `op` fold op d

   mulMatch times plus (Plus x0 x1) (Plus y0 y1) =
      Plus (mulMatch times plus x0 y0)
         (liftA2 plus (mulMatch times plus x0 y1) $
          mulMatch times plus x1 $ liftA2 plus y0 y1)


class
   (NonEmptyC.Show idx,
    MV.List idx, SumC (Sum idx),
    Foldable idx, NonEmpty.RemoveEach idx) =>
      List idx where
   type Sum idx :: * -> *

   switch ::
      (Empty i -> f Empty) ->
      (forall didx. List didx => NonEmpty.T didx i -> f (NonEmpty.T didx)) ->
      idx i -> f idx

   fillMask ::
      (Ord i, List ridx) =>
      idx i -> ridx i -> FillMask idx ridx i
   fillValueMask ::
      (Ord i) =>
      Empty i -> idx i -> FillMask Empty idx i
   fillPlusMask ::
      (Ord i, List lidx) =>
      (NonEmpty.T lidx) i -> idx i -> FillMask (NonEmpty.T lidx) idx i

   exToMultiValue ::
      (a -> a -> a) -> ExStack idx i a -> MV.ExMultiValue idx i a
   exFromMultiValue ::
      (a -> a -> a) -> MV.ExMultiValue idx i a -> ExStack idx i a

instance List Empty where
   type Sum Empty = Value

   switch f _ x = f x

   fillMask = fillValueMask
   fillValueMask _l _r = FillMask Empty FillStop FillStop
   fillPlusMask (NonEmpty.Cons i is) r =
      case fillMask is r of
         FillMask js lmask rmask ->
            FillMask (i!:js) (FillTake lmask) (FillSkip rmask)

   exToMultiValue _plus (ExStack Empty (Value x)) =
      MV.ExMultiValue Empty (MV.Leaf x)
   exFromMultiValue _minus (MV.ExMultiValue Empty (MV.Leaf x)) =
      ExStack Empty (Value x)

instance (List idx) => List (NonEmpty.T idx) where
   type Sum (NonEmpty.T idx) = Plus (Sum idx)

   switch _ f x = f x

   fillMask = fillPlusMask

   fillValueMask l (NonEmpty.Cons i is) =
      case fillMask l is of
         FillMask js lmask rmask ->
            FillMask (i!:js) (FillSkip lmask) (FillTake rmask)
   fillPlusMask it@(NonEmpty.Cons i is) jt@(NonEmpty.Cons j js) =
      case compare i j of
         EQ ->
            case fillMask is js of
               FillMask ks lmask rmask ->
                  FillMask (i!:ks) (FillTake lmask) (FillTake rmask)
         LT ->
            case fillMask is jt of
               FillMask ks lmask rmask ->
                  FillMask (i!:ks) (FillTake lmask) (FillSkip rmask)
         GT ->
            case fillMask it js of
               FillMask ks lmask rmask ->
                  FillMask (j!:ks) (FillSkip lmask) (FillTake rmask)

   exToMultiValue plus (ExStack (NonEmpty.Cons i is) (Plus a0 b0)) =
      case (exToMultiValue plus (ExStack is a0),
            exToMultiValue plus (ExStack is b0)) of
         (MV.ExMultiValue js a1, MV.ExMultiValue _js b1) ->
            MV.ExMultiValue (i!:js) (MV.Branch a1 (liftA2 plus b1 a1))
   exFromMultiValue minus (MV.ExMultiValue (NonEmpty.Cons i is) (MV.Branch a0 b0)) =
      case (exFromMultiValue minus (MV.ExMultiValue is a0),
            exFromMultiValue minus (MV.ExMultiValue is b0)) of
         (ExStack js a1, ExStack _js b1) ->
            ExStack (i!:js) (Plus a1 (liftA2 minus b1 a1))



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


newtype
   SwitchEx f a idx =
      SwitchEx {runSwitchEx :: Sum idx a -> f idx}

switchExStack ::
   List idx =>
   (ExStack Empty i a -> f Empty) ->
   (forall didx. List didx =>
    ExStack (NonEmpty.T didx) i a -> f (NonEmpty.T didx)) ->
   ExStack idx i a -> f idx
switchExStack f g (ExStack is0 s0) =
   runSwitchEx
      (switch
         (\is -> SwitchEx $ \s -> f $ ExStack is s)
         (\is -> SwitchEx $ \s -> g $ ExStack is s)
         is0) s0


newtype
   Switch x (idx :: * -> *) =
      Switch {runSwitch :: x}

switchStack ::
   (ExStack Empty i a -> x) ->
   (forall didx. List didx =>
    ExStack (NonEmpty.T didx) i a -> x) ->
   Stack i a -> x
switchStack f g (Stack is s) =
   runSwitch
      (switchExStack (Switch . f) (Switch . g) (ExStack is s))


descent :: Stack i a -> Either a (i, (Stack i a, Stack i a))
descent =
   switchStack
      (\(ExStack Empty (Value a)) -> Left a)
      (\(ExStack (NonEmpty.Cons i is) (Plus x y)) ->
          Right (i, (Stack is x, Stack is y)))


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
   FillMask lidx ridx i =
      forall lmask rmask idx.
         (lidx ~ FillFromIndex lmask, idx ~ FillToIndex lmask, Fill lmask,
          ridx ~ FillFromIndex rmask, idx ~ FillToIndex rmask, Fill rmask) =>
         FillMask (idx i) lmask rmask

class
   (List (FillToIndex mask), List (FillFromIndex mask)) => Fill mask where
   type FillFromIndex mask :: * -> *
   type FillToIndex mask :: * -> *
   fill ::
      (a -> a) ->
      mask ->
      Sum (FillFromIndex mask) a ->
      Sum (FillToIndex mask) a

instance Fill FillStop where
   type FillFromIndex FillStop = Empty
   type FillToIndex FillStop = Empty
   fill _clear FillStop (Value x) = Value x

instance Fill mask => Fill (FillTake mask) where
   type FillFromIndex (FillTake mask) = NonEmpty.T (FillFromIndex mask)
   type FillToIndex (FillTake mask) = NonEmpty.T (FillToIndex mask)
   fill clear (FillTake mask) (Plus x y) =
      Plus (fill clear mask x) (fill clear mask y)

instance Fill mask => Fill (FillSkip mask) where
   type FillFromIndex (FillSkip mask) = FillFromIndex mask
   type FillToIndex (FillSkip mask) = NonEmpty.T (FillToIndex mask)
   fill clear (FillSkip mask) x =
      let y = fill clear mask x
      in  Plus y (fmap clear y)

add ::
   (Ord i) =>
   (a -> a -> a) -> (a -> a) ->
   Stack i a -> Stack i a -> Stack i a
add plus clear (Stack is x) (Stack js y) =
   case fillMask is js of
      FillMask ks lmask rmask ->
         Stack ks (liftA2 plus (fill clear lmask x) (fill clear rmask y))

{-
A more efficient solution would not need 'clear'.
However, I am afraid it could not use the comfortable 'Fill' class.
-}
mul ::
   (Ord i) =>
   (a -> a -> a) -> (a -> a -> a) -> (a -> a) ->
   Stack i a -> Stack i a -> Stack i a
mul times plus clear (Stack is x) (Stack js y) =
   case fillMask is js of
      FillMask ks lmask rmask ->
         Stack ks (mulMatch times plus (fill clear lmask x) (fill clear rmask y))

instance (Ord i, Num a) => Num (Stack i a) where
   fromInteger = singleton . fromInteger
   negate (Stack is s) = Stack is $ fmap negate s
   (+) = add (+) (const 0)
   (*) = mul (*) (+) (const 0)

   abs = liftMultiValue (+) (-) abs
   signum = liftMultiValue (+) (-) signum


{-
recip ::
   (Ord i) =>
   (a -> a) -> (a -> a -> a) ->
   (a -> a) -> (a -> a -> a) ->
   Stack i a -> Stack i a
recip rec times neg plus (Stack is s) =
   let go (Value a) = Value $ rec a
       go (Plus a d) =
          let ra = go a
              rd = go (liftA2 plus a d)
          in  Plus ra $
                 fmap neg $ mulMatch times plus d $ mulMatch times plus ra rd
       -- 1/(a+d) - 1/a = -d/(a*(a+d))
   in  Stack is $ go s
-}

instance (Ord i, Fractional a) => Fractional (Stack i a) where
   fromRational = singleton . fromRational
   -- cf. limitations of Arith.recip
   -- recip = recip P.recip (*) P.negate (+)
   recip = liftMultiValue (+) (-) P.recip
   -- x / y = fromMultiValueNum $ toMultiValueNum x / toMultiValueNum y


instance (Ord i, Arith.Sum a) => Arith.Sum (Stack i a) where
   negate (Stack is s) = Stack is $ fmap Arith.negate s
   (~+) = add (~+) Arith.clear

instance (Ord i, Arith.Product a) => Arith.Product (Stack i a) where
   (~*) = mul (~*) (~+) Arith.clear
   {-
   If we are going through the MultiValue,
   then chances are higher that divisions like @x/x@ are detected
   which is essential for symbolic computation.
   However, it is highly fragile.
   I also think that the native 'recip'
   has less problems with numeric cancelations.
   -}
   -- recip = recip Arith.recip (~*) Arith.negate (~+)
   recip = liftMultiValue (~+) (~-) Arith.recip
   -- x ~/ y = fromMultiValue $ toMultiValue x ~/ toMultiValue y

instance (Ord i, Arith.Constant a) => Arith.Constant (Stack i a) where
   zero = singleton Arith.zero
   fromInteger = singleton . Arith.fromInteger
   fromRational = singleton . Arith.fromRational

instance (Ord i, Arith.Integrate v) => Arith.Integrate (Stack i v) where
   type Scalar (Stack i v) = Stack i (Arith.Scalar v)
   integrate (Stack is a) = Stack is $ fmap Arith.integrate a


singleton :: a -> Stack i a
singleton = Stack Empty . Value

deltaPair :: i -> a -> a -> Stack i a
deltaPair i a d =
   Stack (i!:Empty) $ Plus (Value a) (Value d)

exPlus ::
   i ->
   ExStack idx i a ->
   ExStack idx i a ->
   ExStack (NonEmpty.T idx) i a
exPlus i (ExStack is a) (ExStack _ d) = ExStack (i !: is) (Plus a d)

splitPlus ::
   ExStack (NonEmpty.T idx) i a -> (i, (ExStack idx i a, ExStack idx i a))
splitPlus (ExStack (NonEmpty.Cons i is) (Plus a0 a1)) =
   (i, (ExStack is a0, ExStack is a1))

-- could be a simple Semigroup.Foldable.head if it would exist
absolute :: Stack i a -> a
absolute (Stack _ s) = fold const s

normalize :: (Arith.Product a) => Stack i a -> Stack i a
normalize s = fmap (~/ absolute s) s


toList :: SumC sum => sum a -> NonEmpty.T [] a
toList = fold NonEmpty.append . fmap NonEmpty.singleton

{- |
You may use 'Data.Foldable.sum' for evaluation with respect to 'Num' class.
-}
evaluate :: (SumC sum, Arith.Sum a) => sum a -> a
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
   FilterMask idx =
      forall mask. (idx ~ FilterFromIndex mask, Filter mask) =>
         FilterMask mask

class (List (FilterToIndex mask), List (FilterFromIndex mask)) => Filter mask where
   type FilterFromIndex mask :: * -> *
   type FilterToIndex mask :: * -> *
   exFilter ::
      (Ord i) =>
      mask ->
      ExStack (FilterFromIndex mask) i a ->
      ExStack (FilterToIndex mask) i a

instance Filter TakeStop where
   type FilterFromIndex TakeStop = Empty
   type FilterToIndex TakeStop = Empty
   exFilter TakeStop (ExStack is s) = (ExStack is s)

instance Filter mask => Filter (TakeAll mask) where
   type FilterFromIndex (TakeAll mask) = NonEmpty.T (FilterFromIndex mask)
   type FilterToIndex (TakeAll mask) = NonEmpty.T (FilterToIndex mask)
   exFilter (TakeAll mask) (ExStack (NonEmpty.Cons i is) (Plus a d)) =
      exPlus i (exFilter mask (ExStack is a)) (exFilter mask (ExStack is d))

instance Filter mask => Filter (TakeOne mask) where
   type FilterFromIndex (TakeOne mask) = NonEmpty.T (FilterFromIndex mask)
   type FilterToIndex (TakeOne mask) = FilterToIndex mask
   exFilter (TakeOne branch mask) (ExStack (NonEmpty.Cons _i is) (Plus a d)) =
      case branch of
         Before -> exFilter mask (ExStack is a)
         Delta  -> exFilter mask (ExStack is d)


filterMask ::
   (List idx, Ord i) => Map i Branch -> idx i -> FilterMask idx
filterMask cond =
   switch
      (\Empty -> FilterMask TakeStop)
      (\(NonEmpty.Cons i is) ->
         case filterMask cond is of
            FilterMask mask ->
               case Map.lookup i cond of
                  Nothing -> FilterMask (TakeAll mask)
                  Just branch -> FilterMask (TakeOne branch mask))

{- |
The naive implementation ignores indices
that are in the condition map but not in the stack.

Unfortunately, 'filterNaive' does not satisfy simple laws.
This is because it selects not only certain branches
but also re-declares @delta@ branches as @before@ branches.
-}
filterNaive :: (Ord i) => Map i Branch -> Stack i a -> Stack i a
filterNaive cond (Stack is s) =
   case filterMask cond is of
      FilterMask mask -> wrapStack $ exFilter mask $ ExStack is s


toMultiValue :: Arith.Sum a => Stack i a -> MV.MultiValue i a
toMultiValue = toMultiValueGen (~+)

toMultiValueNum :: Num a => Stack i a -> MV.MultiValue i a
toMultiValueNum = toMultiValueGen (+)

toMultiValueGen :: (a -> a -> a) -> Stack i a -> MV.MultiValue i a
toMultiValueGen plus (Stack is s) =
   case exToMultiValue plus (ExStack is s) of
      MV.ExMultiValue js tree -> MV.MultiValue js tree

liftMultiValue ::
   (a -> a -> a) ->
   (b -> b -> b) ->
   (a -> b) -> Stack t a -> Stack t b
liftMultiValue plus minus f (Stack is s) =
   wrapStack $ exFromMultiValue minus $
   fmap f $ exToMultiValue plus (ExStack is s)


assigns :: Stack i a -> NonEmpty.T [] ([Idx.Record Idx.Delta i], a)
assigns s =
   case descent s of
      Left a -> NonEmpty.singleton ([], a)
      Right (i, (a0,a1)) ->
         NonEmpty.append
            (fmap (mapFst (Idx.before i :)) $ assigns a0)
            (fmap (mapFst (Idx.delta  i :)) $ assigns a1)


shrinkStack :: Stack i a -> [Stack i a]
shrinkStack =
   switchStack
      (const [])
      (\(ExStack it (Plus a0 a1)) ->
         concatMap (\(_,is) -> [Stack is a0, Stack is a1]) $
         Fold.toList $ NonEmpty.removeEach it)


newtype
   Shrink i a idx =
      Shrink {runShrink :: [ExStack idx i a]}

shrinkValues ::
   (List idx, QC.Arbitrary a) =>
   ExStack idx i a -> [ExStack idx i a]
shrinkValues s =
   runShrink $
   switchExStack
      (\(ExStack Empty (Value a)) -> Shrink $
           map (ExStack Empty . Value) $ QC.shrink a)
      (\x -> Shrink $
         case splitPlus x of
            (i, (a0,a1)) ->
               map (flip (exPlus i) a1) (shrinkValues a0)
               ++
               map (exPlus i a0) (shrinkValues a1))
      s

instance
   (QC.Arbitrary i, Ord i, QC.Arbitrary a, Arith.Sum a) =>
      QC.Arbitrary (Stack i a) where

   shrink s@(Stack it tree) =
      shrinkStack s
      ++
      map wrapStack (shrinkValues (ExStack it tree))
