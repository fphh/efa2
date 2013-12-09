{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.Stack (
   Stack,
   fromMultiValue,
   toMultiValue,

   singleton,
   deltaPair,
   absolute,
   normalize,

   Branch(..), Filtered(..),
   filter, filterNaive, startFilter, mergeConditions,
   eqRelaxedNum,

   mapIndicesMonotonic,
   toList, evaluate, assignDeltaMap, assigns, mapWithIndices,
   ) where

import qualified EFA.Equation.MultiValue as MV
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
import qualified Data.Foldable as Fold
import Control.Applicative (liftA2)
import Data.Map (Map)
import Data.Traversable (sequenceA)
import Data.Monoid ((<>))
import Data.Tuple.HT (mapPair)
import Data.Maybe.HT (toMaybe)

import qualified Prelude as P
import Prelude hiding (recip, filter)


{- |
The indices of type @i@ must be in strictly descending order.
-}
data Stack i a =
     Stack a
   | Dim i (Stack i (a, a))
   deriving (Show, Eq)

instance Functor (Stack i) where
   fmap f n =
      case n of
         Stack a -> Stack $ f a
         Dim i c -> Dim i $ fmap (mapPair (f,f)) c



instance FormatValue a => FormatValue (Stack i a) where
   formatValue = fold Format.plus . fmap formatValue


indices :: Stack i a -> [i]
indices (Stack _) = []
indices (Dim i a) = i : indices a

{- |
The index function must generate an index list with descending index order.
That is, it must be monotonic
at least on the indices that are present in the stack.
-}
mapIndicesMonotonic :: (Ord j) => (i -> j) -> Stack i a -> Stack j a
mapIndicesMonotonic _ (Stack a) = Stack a
mapIndicesMonotonic g (Dim i a) =
   Dim (g i) $ mapIndicesMonotonic g a


double0 :: a -> (a, a)
double0 a = (a, a)

double1 :: (a -> b) -> (a, a) -> (b, b)
double1 clear = mapPair (clear, clear)

double2 :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
double2 plus (a0,a1) (b0,b1) = (plus a0 b0, plus a1 b1)

descent2 ::
   (Ord i) =>
   (a -> b -> c) ->
   (i -> Stack i (a,a) -> Stack i b -> c) ->
   (i -> Stack i a -> Stack i (b,b) -> c) ->
   (i -> Stack i (a,a) -> Stack i (b,b) -> c) ->
   Stack i a -> Stack i b -> c
descent2 final gt lt eq x0 y0 =
   case (x0,y0) of
      (Stack x, Stack y) -> final x y
      (Dim i x, Stack _) -> gt i x y0
      (Stack _, Dim j y) -> lt j x0 y
      (Dim i x, Dim j y) ->
         case compare i j of
            GT -> gt i x y0
            LT -> lt j x0 y
            EQ -> eq i x y


eqRelaxedNum ::
   (Ord i, Eq a, Num a) =>
   Stack i a -> Stack i a -> Bool
eqRelaxedNum = eqRelaxed (==) (0==) (==0)

eqRelaxed ::
   (Ord i) =>
   (a -> b -> Bool) ->
   (a -> Bool) -> (b -> Bool) ->
   Stack i a -> Stack i b -> Bool
eqRelaxed eq isZeroA isZeroB =
   descent2 eq
      (\_i ->
         eqRelaxed
            (\(a0,a1) b -> eq a0 b && isZeroA a1)
            (uncurry (&&) . double1 isZeroA)
            isZeroB)
      (\_i ->
         eqRelaxed
            (\a (b0,b1) -> eq a b0 && isZeroB b1)
            isZeroA
            (uncurry (&&) . double1 isZeroB))
      (\_i ->
         eqRelaxed
            (\a b -> uncurry (&&) $ double2 eq a b)
            (uncurry (&&) . double1 isZeroA)
            (uncurry (&&) . double1 isZeroB))


add ::
   (Ord i) =>
   (a -> b -> c) -> (a -> a) -> (b -> b) ->
   Stack i a -> Stack i b -> Stack i c
add plus clearA clearB =
   descent2
      (\x y -> Stack $ plus x y)
      (\i x y ->
         Dim i $
         add
            (\a b -> double2 plus a (b, clearB b))
            (double1 clearA) clearB
            x y)
      (\i x y ->
         Dim i $
         add
            (\a b -> double2 plus (a, clearA a) b)
            clearA (double1 clearB)
            x y)
      (\i x y ->
         Dim i $
         add (double2 plus) (double1 clearA) (double1 clearB) x y)

mul ::
   (Ord i) =>
   (a -> b -> c) ->
   (a -> b -> c) -> (c -> c -> c) ->
   (a -> a) -> (b -> b) ->
   Stack i a -> Stack i b -> Stack i c
mul times plus plusC clearA clearB =
   descent2
      (\x y -> Stack $ times x y)
      (\i x y ->
         Dim i $
         mul
            (\a b -> double2 times a (double0 b))
            (\(a0,a1) b -> (plus a0 b, plus a1 (clearB b))) (double2 plusC)
            (double1 clearA) clearB
            x y)
      (\i x y ->
         Dim i $
         mul
            (\a b -> double2 times (double0 a) b)
            (\a (b0,b1) -> (plus a b0, plus (clearA a) b1)) (double2 plusC)
            clearA (double1 clearB)
            x y)
      (\i x y ->
         Dim i $
         mul
            (\(a0,a1) (b0,b1) ->
               (times a0 b0,
                plusC (times a0 b1) $
                plusC (times a1 b0) $
                times a1 b1))
            (double2 plus) (double2 plusC)
            (double1 clearA) (double1 clearB)
            x y)

instance (Ord i, Num a) => Num (Stack i a) where
   fromInteger = singleton . fromInteger
   negate = fmap negate
   (+) = add (+) (const 0) (const 0)
   (*) = mul (*) (+) (+) (const 0) (const 0)

   abs = fromMultiValueNum . abs . toMultiValueNum
   signum = fromMultiValueNum . signum . toMultiValueNum


instance (Ord i, Fractional a) => Fractional (Stack i a) where
   fromRational = singleton . fromRational
   -- cf. limitations of Arith.recip
   -- recip = recip P.recip (*) P.negate (+)
   recip = fromMultiValueNum . P.recip . toMultiValueNum
   x / y = fromMultiValueNum $ toMultiValueNum x / toMultiValueNum y


instance (Ord i, Arith.Sum a) => Arith.Sum (Stack i a) where
   negate = fmap Arith.negate
   (~+) = add (~+) Arith.clear Arith.clear

instance (Ord i, Arith.Product a) => Arith.Product (Stack i a) where
   (~*) = mul (~*) (~+) (~+) Arith.clear Arith.clear
   {-
   If we are going through the MultiValue,
   then chances are higher that divisions like @x/x@ are detected
   which is essential for symbolic computation.
   However, it is highly fragile.
   I also think that the native 'recip'
   has less problems with numeric cancelations.
   -}
   -- recip = recip Arith.recip (~*) Arith.negate (~+)
   recip = fromMultiValue . Arith.recip . toMultiValue
   x ~/ y = fromMultiValue $ toMultiValue x ~/ toMultiValue y
   constOne = fromMultiValue . Arith.constOne . toMultiValue

instance (Ord i, Arith.Constant a) => Arith.Constant (Stack i a) where
   zero = singleton Arith.zero
   fromInteger = singleton . Arith.fromInteger
   fromRational = singleton . Arith.fromRational

instance (Ord i, Arith.Integrate v) => Arith.Integrate (Stack i v) where
   type Scalar (Stack i v) = Stack i (Arith.Scalar v)
   integrate = fmap Arith.integrate

instance
   (Ord i, Arith.Sum a, Arith.ZeroTestable a) =>
      Arith.ZeroTestable (Stack i a) where
   allZeros = fold (&&) . fmap Arith.allZeros
   coincidingZeros x y =
      Arith.coincidingZeros (toMultiValue x) (toMultiValue y)


singleton :: a -> Stack i a
singleton = Stack

deltaPair :: i -> a -> a -> Stack i a
deltaPair i a d = Dim i $ Stack (a, d)

-- could be a simple Semigroup.Foldable.head if it would exist
absolute :: Stack i a -> a
absolute (Stack x) = x
absolute (Dim _ x) = fst $ absolute x

normalize :: (Arith.Product a) => Stack i a -> Stack i a
normalize s = fmap (~/ absolute s) s


toList :: Stack i a -> NonEmpty.T [] a
toList = fold NonEmptyC.append . fmap NonEmpty.singleton

{- |
You may use 'Data.Foldable.sum' for evaluation with respect to 'Num' class.
-}
evaluate :: Arith.Sum a => Stack i a -> a
evaluate = fold (~+)

fold :: (a -> a -> a) -> Stack i a -> a
fold _op (Stack x) = x
fold op (Dim _ x) = fold op $ fmap (uncurry op) x


data Branch = Before | Delta deriving (Eq, Ord, Show)

instance QC.Arbitrary Branch where
   arbitrary = QC.elements [Before, Delta]


{- |
A filtered stack contains a sub-hypercube of values
and the point from where it was taken from a larger hypercube.
The index sets of the map and the stack must be distinct.
-}
data Filtered i a =
   Filtered {
      corner :: Map i Branch,
      filtered :: Stack i a
   } deriving (Eq, Show)

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
filter c1 (Filtered c0 s) =
   liftA2 Filtered (mergeConditions c0 c1) $
   fmap
      (\c1' ->
         (if Fold.any (Delta==) $ MapU.differenceSet c1' $
             Set.fromList $ indices s
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

{- |
The naive implementation ignores indices
that are in the condition map but not in the stack.

Unfortunately, 'filterNaive' does not satisfy simple laws.
This is because it selects not only certain branches
but also re-declares @delta@ branches as @before@ branches.
-}
filterNaive :: (Ord i) => Map i Branch -> Stack i a -> Stack i a
filterNaive _cond (Stack a) = Stack a
filterNaive cond (Dim i a) =
   (case Map.lookup i cond of
      Nothing -> Dim i
      Just Before -> fmap fst
      Just Delta  -> fmap snd) $
   filterNaive cond a


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
fromMultiValueGen _minus (MV.MultiValue cube) = Stack cube
fromMultiValueGen minus (MV.Dim i cube) =
   Dim i $
   fromMultiValueGen
      (\(x0,x1) (y0,y1) -> (minus x0 y0, minus x1 y1)) $
   fmap (\(x,y) -> (x, minus y x)) cube

toMultiValueGen :: (a -> a -> a) -> Stack i a -> MV.MultiValue i a
toMultiValueGen _plus (Stack cube) = MV.MultiValue cube
toMultiValueGen plus (Dim i cube) =
   MV.Dim i $
   fmap (\(x,y) -> (x, plus y x)) $
   toMultiValueGen
      (\(x0,x1) (y0,y1) -> (plus x0 y0, plus x1 y1)) cube


assignDeltaMap :: (Ord i) => Stack i a -> Map (Map i Branch) a
assignDeltaMap =
   Map.fromListWith (error "assignDeltaMap: duplicate indices") .
   NonEmpty.tail . assigns

assigns :: (Ord i) => Stack i a -> NonEmpty.T [] (Map i Branch, a)
assigns = fold NonEmptyC.append . fmap NonEmpty.singleton . mapWithIndices (,)

mapWithIndices ::
   (Ord i) =>
   (Map i Branch -> a -> b) ->
   Stack i a -> Stack i b
mapWithIndices f (Stack a) = Stack $ f Map.empty a
mapWithIndices f (Dim i a) =
   Dim i $
   mapWithIndices
      (\is (a0,a1) ->
         (f (Map.insert i Before is) a0,
          f (Map.insert i Delta  is) a1))
      a


instance
   (QC.Arbitrary i, Ord i, QC.Arbitrary a, Arith.Sum a) =>
      QC.Arbitrary (Stack i a) where
   arbitrary = fmap fromMultiValue QC.arbitrary

   shrink cube =
      removeEachDimension cube ++ removeValues cube


-- * private functions

removeEachDimension :: Stack i a -> [Stack i a]
removeEachDimension (Stack _) = []
removeEachDimension (Dim i cube) =
   fmap fst cube :
   fmap snd cube :
   map (Dim i) (removeEachDimension cube)

removeValues :: (QC.Arbitrary a) => Stack i a -> [Stack i a]
removeValues (Stack a) = map Stack $ QC.shrink a
removeValues (Dim i cube) = map (Dim i) $ removeValues cube
