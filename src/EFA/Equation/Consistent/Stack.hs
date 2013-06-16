{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
module EFA.Equation.Consistent.Stack where

import qualified EFA.Equation.Consistent.Dimension as Dim
import EFA.Equation.Consistent.Dimension (switch)

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.Consistent.MultiValue as MV
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~-), (~*), (~/))

import qualified EFA.Utility.Map as MapU

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Test.QuickCheck as QC

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.NonEmpty.Class as NonEmptyC
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import qualified Data.List.HT as ListHT
import qualified Data.Foldable as Fold
import Control.Applicative (Applicative, pure, liftA2, (<*>))
import Data.NonEmpty ((!:))
import Data.Map (Map)
import Data.Traversable (sequenceA)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))
import Data.Tuple.HT (mapFst)
import Data.Maybe.HT (toMaybe)

import qualified Prelude as P
import Prelude hiding (recip, filter, showsPrec)


{- |
This data structure is the same as in "EFA.Equation.Stack"
but we assert statically
that the length of the list matches the depth of the tree.

The indices must be in strictly ascending order.
Unfortunately, we cannot assert this statically.
-}
data Stack i a = forall idx. Dim.C idx => Stack (idx i) (Sum idx a)

data
   ExStack idx i a =
      ExStack {exStackIndices :: idx i, exStackSum :: Sum idx a}


wrapStack :: Dim.C idx => ExStack idx i a -> Stack i a
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



type family Sum (idx :: * -> *) :: * -> *  -- must be removed
type instance Sum Empty.T = Value
type instance Sum (NonEmpty.T idx) = Plus (Sum idx)


newtype Cube idx a = Cube {unCube :: Sum idx a}

cubeFromExStack :: ExStack idx i a -> Cube idx a
cubeFromExStack (ExStack _is s) = Cube s

withCubeFromStack ::
   (forall idx. Dim.C idx => Cube idx a -> b) -> Stack i a -> b
withCubeFromStack f (Stack is s) = f (cubeFromExStack $ ExStack is s)

exStackFromCube :: idx i -> Cube idx a -> ExStack idx i a
exStackFromCube is (Cube s) = ExStack is s

stackFromCube :: Dim.C idx => idx i -> Cube idx a -> Stack i a
stackFromCube is (Cube s) = Stack is s


newtype WrapCube a idx = WrapCube {unwrapCube :: Cube idx a}

newtype
   WrapFunctor a b idx =
      WrapFunctor {unwrapFunctor :: Cube idx a -> Cube idx b}

mapCubeValue :: (a -> b) -> Cube Empty.T a -> Cube Empty.T b
mapCubeValue f = valueCube . f . valueFromCube

valueFromCube :: Cube Empty.T a -> a
valueFromCube (Cube (Value a)) = a

splitCube ::
   Cube (NonEmpty.T idx) a -> (Cube idx a, Cube idx a)
splitCube (Cube (Plus a d)) = (Cube a, Cube d)

valueCube :: a -> Cube Empty.T a
valueCube a = Cube $ Value a

plusCube ::
   Cube idx a -> Cube idx a ->
   Cube (NonEmpty.T idx) a
plusCube (Cube a) (Cube d) = Cube (Plus a d)

instance Dim.C idx => Functor (Cube idx) where
   fmap f =
      unwrapFunctor $
      switch
         (WrapFunctor $ mapCubeValue f)
         (WrapFunctor $ \x ->
            case splitCube x of
               (a,d) -> plusCube (fmap f a) (fmap f d))


newtype
   WrapApply a b c idx =
      WrapApply {unwrapApply :: Cube idx a -> Cube idx b -> Cube idx c}


instance Dim.C idx => Applicative (Cube idx) where
   pure a =
      unwrapCube $
      switch
         (WrapCube $ valueCube a)
         (WrapCube $ (\c -> plusCube c c) $ pure a)
   (<*>) =
      unwrapApply $
      switch
         (WrapApply $ \f a -> valueCube $ (valueFromCube f) (valueFromCube a))
         (WrapApply $ \f a ->
            case (splitCube f, splitCube a) of
               ((fa,fd), (aa,ad)) -> plusCube (fa<*>aa) (fd<*>ad))


instance Functor (Stack i) where
   fmap f (Stack is s) =
      case ExStack is s of
         x -> stackFromCube is . fmap f . cubeFromExStack $ x

instance Functor Value where
   fmap f (Value a) = Value (f a)

instance Functor sum => Functor (Plus sum) where
   fmap f (Plus a0 a1) = Plus (fmap f a0) (fmap f a1)


newtype
   WrapExStack i a idx =
      WrapExStack {unwrapExStack :: ExStack idx i a}

instance (Dim.C idx) => Functor (ExStack idx i) where
   fmap f s =
      exStackFromCube (exStackIndices s) $
      fmap f $ cubeFromExStack s


instance Applicative Value where
   pure = Value
   Value f <*> Value a = Value (f a)

instance Applicative sum => Applicative (Plus sum) where
   pure a = Plus (pure a) (pure a)
   Plus f0 f1 <*> Plus a0 a1 = Plus (f0 <*> a0) (f1 <*> a1)


instance FormatValue a => FormatValue (Stack i a) where
   formatValue =
      withCubeFromStack (fold Format.plus . fmap formatValue)

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


newtype Index f i idx = Index {runIndex :: idx i -> f idx}

switchIndex ::
   (Dim.C idx) =>
   (Empty.T i -> f Empty.T) ->
   (forall didx. Dim.C didx => NonEmpty.T didx i -> f (NonEmpty.T didx)) ->
   idx i -> f idx
switchIndex f g =
   runIndex $ switch (Index f) (Index g)


newtype
   Switch i a x idx =
      Switch {runSwitch :: ExStack idx i a -> x}

switchStack ::
   (ExStack Empty.T i a -> x) ->
   (forall didx. Dim.C didx =>
    ExStack (NonEmpty.T didx) i a -> x) ->
   Stack i a -> x
switchStack f g (Stack is s) =
   runSwitch
      (switch (Switch f) (Switch g))
      (ExStack is s)


descent :: Stack i a -> Either a (i, (Stack i a, Stack i a))
descent =
   switchStack
      (\(ExStack Empty.Cons (Value a)) -> Left a)
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


newtype
   FillLeftMask ridx i lidx =
      FillLeftMask {getFillLeftMask :: FillMask lidx ridx i}

fillMask ::
   (Ord i, Dim.C lidx, Dim.C ridx) =>
   lidx i -> ridx i -> FillMask lidx ridx i
fillMask l r =
   getFillLeftMask $
   switchIndex
      (\is -> FillLeftMask $ fillValueMask is r)
      (\is -> FillLeftMask $ fillPlusMask is r)
      l


newtype
   FillRightMask lidx i ridx =
      FillRightMask {getFillRightMask :: FillMask lidx ridx i}

fillValueMask ::
   (Ord i, Dim.C idx) =>
   Empty.T i -> idx i -> FillMask Empty.T idx i
fillValueMask l =
   getFillRightMask .
   switchIndex
      (\empty -> FillRightMask $ FillMask empty FillStop FillStop)
      (\(NonEmpty.Cons i is) ->
         FillRightMask $
         case fillMask l is of
            FillMask js lmask rmask ->
               FillMask (i!:js) (FillSkip lmask) (FillTake rmask))


fillPlusMask ::
   (Ord i, Dim.C lidx, Dim.C ridx) =>
   (NonEmpty.T lidx) i -> ridx i -> FillMask (NonEmpty.T lidx) ridx i
fillPlusMask it@(NonEmpty.Cons i is) =
   getFillRightMask .
   switchIndex
      (\empty ->
         FillRightMask $
         case fillMask is empty of
            FillMask js lmask rmask ->
               FillMask (i!:js) (FillTake lmask) (FillSkip rmask))
      (\jt@(NonEmpty.Cons j js) ->
         FillRightMask $
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
                     FillMask (j!:ks) (FillSkip lmask) (FillTake rmask))


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
   (Dim.C (FillToIndex mask), Dim.C (FillFromIndex mask)) => Fill mask where
   type FillFromIndex mask :: * -> *
   type FillToIndex mask :: * -> *
   fill ::
      (a -> a) ->
      mask ->
      Sum (FillFromIndex mask) a ->
      Cube (FillToIndex mask) a

instance Fill FillStop where
   type FillFromIndex FillStop = Empty.T
   type FillToIndex FillStop = Empty.T
   fill _clear FillStop x = Cube x

instance Fill mask => Fill (FillTake mask) where
   type FillFromIndex (FillTake mask) = NonEmpty.T (FillFromIndex mask)
   type FillToIndex (FillTake mask) = NonEmpty.T (FillToIndex mask)
   fill clear (FillTake mask) (Plus x y) =
      plusCube (fill clear mask x) (fill clear mask y)

instance Fill mask => Fill (FillSkip mask) where
   type FillFromIndex (FillSkip mask) = FillFromIndex mask
   type FillToIndex (FillSkip mask) = NonEmpty.T (FillToIndex mask)
   fill clear (FillSkip mask) x =
      let y = fill clear mask x
      in  plusCube y (fmap clear y)


add ::
   (Ord i) =>
   (a -> a -> a) -> (a -> a) ->
   Stack i a -> Stack i a -> Stack i a
add plus clear (Stack is x) (Stack js y) =
   case fillMask is js of
      FillMask ks lmask rmask ->
         stackFromCube ks $
         liftA2 plus (fill clear lmask x) (fill clear rmask y)


newtype
   CubeFunc2 i a idx =
      CubeFunc2 {runCubeFunc2 :: Cube idx a -> Cube idx a -> Cube idx a}

mulMatch ::
   Dim.C idx =>
   (a -> a -> a) -> (a -> a -> a) ->
   Cube idx a -> Cube idx a -> Cube idx a
mulMatch times plus =
   runCubeFunc2 $
   switch
      (CubeFunc2 $ liftA2 times)
      (CubeFunc2 $ \x y ->
         case (splitCube x, splitCube y) of
            ((x0, x1), (y0, y1)) ->
               plusCube
                  (mulMatch times plus x0 y0)
                  (liftA2 plus (mulMatch times plus x0 y1) $
                   mulMatch times plus x1 $
                   liftA2 plus y0 y1))


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
         stackFromCube ks $
         mulMatch times plus
            (fill clear lmask x) (fill clear rmask y)

instance (Ord i, Num a) => Num (Stack i a) where
   fromInteger = singleton . fromInteger
   negate = fmap negate
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
   negate = fmap Arith.negate
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
   constOne = liftMultiValue (~+) (~-) Arith.constOne

instance (Ord i, Arith.Constant a) => Arith.Constant (Stack i a) where
   zero = singleton Arith.zero
   fromInteger = singleton . Arith.fromInteger
   fromRational = singleton . Arith.fromRational

instance (Ord i, Arith.Integrate v) => Arith.Integrate (Stack i v) where
   type Scalar (Stack i v) = Stack i (Arith.Scalar v)
   integrate = fmap Arith.integrate


singleton :: a -> Stack i a
singleton = Stack Empty.Cons . Value

deltaPair :: i -> a -> a -> Stack i a
deltaPair i a d =
   Stack (i!:Empty.Cons) $ Plus (Value a) (Value d)

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


newtype Fold a idx = Fold {unfold :: Cube idx a -> a}

fold :: (Dim.C idx) => (a -> a -> a) -> Cube idx a -> a
fold op =
   unfold $
   switch
      (Fold valueFromCube)
      (Fold $ \x ->
         case splitCube x of
            (a,d) -> fold op a `op` fold op d)


-- could be a simple Semigroup.Foldable.head if it would exist
absolute :: Stack i a -> a
absolute = withCubeFromStack (fold const)

normalize :: (Arith.Product a) => Stack i a -> Stack i a
normalize s = fmap (~/ absolute s) s


toList :: Dim.C idx => Cube idx a -> NonEmpty.T [] a
toList = fold NonEmptyC.append . fmap NonEmpty.singleton

{- |
You may use 'Data.Foldable.sum' for evaluation with respect to 'Num' class.
-}
evaluate :: (Dim.C idx, Arith.Sum a) => Cube idx a -> a
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
         (if Fold.any (Delta==) $ MapU.differenceSet c1' $ foldMap Set.singleton is
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

class (Dim.C (FilterToIndex mask), Dim.C (FilterFromIndex mask)) => Filter mask where
   type FilterFromIndex mask :: * -> *
   type FilterToIndex mask :: * -> *
   exFilter ::
      (Ord i) =>
      mask ->
      ExStack (FilterFromIndex mask) i a ->
      ExStack (FilterToIndex mask) i a

instance Filter TakeStop where
   type FilterFromIndex TakeStop = Empty.T
   type FilterToIndex TakeStop = Empty.T
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
   (Dim.C idx, Ord i) => Map i Branch -> idx i -> FilterMask idx
filterMask cond =
   switchIndex
      (\Empty.Cons -> FilterMask TakeStop)
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


newtype
   ExToMultiValue i a idx =
      ExToMultiValue {
         runExToMultiValue :: ExStack idx i a -> MV.ExMultiValue idx i a
      }

exToMultiValue ::
   (Dim.C idx) =>
   (a -> a -> a) -> ExStack idx i a -> MV.ExMultiValue idx i a
exToMultiValue plus =
   runExToMultiValue $
   switch
      (ExToMultiValue $ \(ExStack Empty.Cons (Value x)) ->
         MV.ExMultiValue Empty.Cons (MV.Leaf x))
      (ExToMultiValue $ \x ->
         case splitPlus x of
            (i, (a,d)) ->
               case (exToMultiValue plus a,
                     exToMultiValue plus $
                        exStackFromCube (exStackIndices a) $
                        liftA2 plus (cubeFromExStack a) (cubeFromExStack d)) of
                  (MV.ExMultiValue js a1, MV.ExMultiValue _js d1) ->
                     MV.ExMultiValue (i!:js) (MV.Branch a1 d1))

toMultiValue :: Arith.Sum a => Stack i a -> MV.MultiValue i a
toMultiValue = toMultiValueGen (~+)

toMultiValueNum :: Num a => Stack i a -> MV.MultiValue i a
toMultiValueNum = toMultiValueGen (+)

toMultiValueGen :: (a -> a -> a) -> Stack i a -> MV.MultiValue i a
toMultiValueGen plus (Stack is s) =
   case exToMultiValue plus (ExStack is s) of
      MV.ExMultiValue js tree -> MV.MultiValue js tree



newtype
   ExFromMultiValue i a idx =
      ExFromMultiValue {
         runExFromMultiValue :: MV.ExMultiValue idx i a -> ExStack idx i a
      }

exFromMultiValue ::
   (Dim.C idx) =>
   (a -> a -> a) -> MV.ExMultiValue idx i a -> ExStack idx i a
exFromMultiValue minus =
   runExFromMultiValue $
   switch
      (ExFromMultiValue $
       \(MV.ExMultiValue Empty.Cons (MV.Leaf x)) -> ExStack Empty.Cons (Value x))
      (ExFromMultiValue $
       \(MV.ExMultiValue (NonEmpty.Cons i is) (MV.Branch a0 b0)) ->
         case (cubeFromExStack $ exFromMultiValue minus (MV.ExMultiValue is a0),
               cubeFromExStack $ exFromMultiValue minus (MV.ExMultiValue is b0)) of
            (a1, b1) ->
               exStackFromCube (i!:is) (plusCube a1 (liftA2 minus b1 a1)))

{- |
Generate a Sum from a MultiValue
representing the right-most value from the MultiValue
where the first summand is the left-most value from the MultiValue.
-}
fromMultiValue :: Arith.Sum a => MV.MultiValue i a -> Stack i a
fromMultiValue = fromMultiValueGen (~-)

fromMultiValueNum :: Num a => MV.MultiValue i a -> Stack i a
fromMultiValueNum = fromMultiValueGen (-)

fromMultiValueGen :: (a -> a -> a) -> MV.MultiValue i a -> Stack i a
fromMultiValueGen minus (MV.MultiValue indices tree) =
   case exFromMultiValue minus (MV.ExMultiValue indices tree) of
      ExStack is s -> Stack is s


liftMultiValue ::
   (a -> a -> a) ->
   (b -> b -> b) ->
   (a -> b) -> Stack i a -> Stack i b
liftMultiValue plus minus f (Stack is s) =
   wrapStack $ exFromMultiValue minus $
   fmap f $ exToMultiValue plus (ExStack is s)


assigns :: Stack i a -> NonEmpty.T [] ([Idx.Record Idx.Delta i], a)
assigns s =
   case descent s of
      Left a -> NonEmpty.singleton ([], a)
      Right (i, (a0,a1)) ->
         NonEmptyC.append
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
      Shrink {runShrink :: ExStack idx i a -> [ExStack idx i a]}

shrinkValues ::
   (Dim.C idx, QC.Arbitrary a) =>
   ExStack idx i a -> [ExStack idx i a]
shrinkValues =
   runShrink $
   switch
      (Shrink $
       \(ExStack Empty.Cons (Value a)) ->
           map (ExStack Empty.Cons . Value) $ QC.shrink a)
      (Shrink $ \x ->
         case splitPlus x of
            (i, (a0,a1)) ->
               map (flip (exPlus i) a1) (shrinkValues a0)
               ++
               map (exPlus i a0) (shrinkValues a1))

instance
   (QC.Arbitrary i, Ord i, QC.Arbitrary a, Arith.Sum a) =>
      QC.Arbitrary (Stack i a) where

   shrink s@(Stack it tree) =
      shrinkStack s
      ++
      map wrapStack (shrinkValues (ExStack it tree))
