{-# LANGUAGE TypeFamilies #-}
module EFA.Equation.Stack where

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Equation.MultiValue as MV
import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~-), (~*), (~/))

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import qualified Test.QuickCheck as QC

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.HT as ListHT
import Control.Applicative (liftA2)
import Data.Map (Map)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid ((<>))
import Data.Tuple.HT (mapFst)

import qualified Prelude as P
import Prelude hiding (recip, filter)


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

instance Functor (Stack i) where
   fmap f (Stack is s) = Stack is (fmap f s)

instance Functor Sum where
   fmap f (Value a) = Value (f a)
   fmap f (Plus a0 a1) = Plus (fmap f a0) (fmap f a1)

instance Foldable Sum where
   foldMap f = fold (<>) . fmap f


instance FormatValue a => FormatValue (Stack i a) where
   formatValue (Stack _ s) = formatValue s

instance FormatValue a => FormatValue (Sum a) where
   formatValue = fold Format.plus . fmap formatValue

{- |
The index function must generate an index list with ascending index order.
That is, it must be monotonic
at least on the indices that are present in the stack.
-}
mapIndicesMonotonic :: (Ord j) => (i -> j) -> Stack i a -> Stack j a
mapIndicesMonotonic g (Stack is s) =
   let js = map g is
   in  if ListHT.isAscending js
         then Stack js s
         else error "Stack.mapIndicesMonotonic: non-monotonic index function"


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


add ::
   (Ord i) =>
   (a -> a -> a) -> (a -> a) ->
   Stack i a -> Stack i a -> Stack i a
add plus zero x0@(Stack is0 _) y0@(Stack js0 _) =
   let go a b =
          case (descent a, descent b) of
             (Left av, Left bv) -> Value $ plus av bv
             (Left _, Right (_, (b0, Stack _ b1))) -> Plus (go a b0) b1
             (Right (_, (a0, Stack _ a1)), Left _) -> Plus (go a0 b) a1
             (Right (i, (a0, a1)), Right (j, (b0, b1))) ->
                case compare i j of
                   EQ -> Plus (go a0 b0) (go a1 b1)
                   LT -> Plus (go a0 b) (go a1 (zeroStack zero b))
                   GT -> Plus (go a b0) (go (zeroStack zero a) b1)
   in  Stack (MV.mergeIndices is0 js0) $ go x0 y0

mul ::
   (Ord i) =>
   (a -> a -> a) -> (a -> a -> a) ->
   Stack i a -> Stack i a -> Stack i a
mul times plus x0@(Stack is0 _) y0@(Stack js0 _) =
   let go a b =
          case (descent a, descent b) of
             (Left av, _) -> case b of Stack _ bs -> fmap (times av) bs
             (_, Left bv) -> case a of Stack _ as -> fmap (flip times bv) as
             (Right (i, (a0, a1)), Right (j, (b0, b1))) ->
                case compare i j of
{-
                   EQ ->
                      Plus (go a0 b0)
                         (addMatch plus (go a0 b1) $
                          go a1 (case (b0,b1) of
                                    (Stack ks bs0, Stack _ bs1) ->
                                       Stack ks $ addMatch plus bs0 bs1))
-}
                   EQ ->
                       Plus (go a0 b0)
                          (addMatch plus (go a0 b1) $
                           addMatch plus (go a1 b0) $ go a1 b1)
                   LT -> Plus (go a0 b) (go a1 b)
                   GT -> Plus (go a b0) (go a b1)
   in  Stack (MV.mergeIndices is0 js0) $ go x0 y0

instance (Ord i, Num a) => Num (Stack i a) where
   fromInteger = singleton . fromInteger
   negate (Stack is s) = Stack is $ fmap negate s
   (+) = add (+) (const 0)
   (*) = mul (*) (+)

   abs = fromMultiValueNum . abs . toMultiValueNum
   signum = fromMultiValueNum . signum . toMultiValueNum


zeroStack :: (a -> a) -> Stack i a -> Stack i a
zeroStack z (Stack is a) = Stack is $ zeroMatch z a

zeroMatch :: (a -> a) -> Sum a -> Sum a
zeroMatch z =
   let go (Value x) = Value $ z x
       go (Plus x0 x1) = Plus (go x0) (go x1)
   in  go

addMatch :: (a -> a -> a) -> Sum a -> Sum a -> Sum a
addMatch plus =
   let go (Value x) (Value y) = Value $ plus x y
       go (Plus x0 x1) (Plus y0 y1) = Plus (go x0 y0) (go x1 y1)
       go _ _ = error "Stack.addMatch: inconsistent data structure"
   in  go

mulMatch :: (a -> a -> a) -> (a -> a -> a) -> Sum a -> Sum a -> Sum a
mulMatch times plus =
   let go (Value x) (Value y) = Value (times x y)
       go (Plus x0 x1) (Plus y0 y1) =
          Plus (go x0 y0)
             (addMatch plus (go x0 y1) $ go x1 $ addMatch plus y0 y1)
       go _ _ = error "Stack.mulMatch: inconsistent data structure"
   in  go

recip ::
   (Ord i) =>
   (a -> a) -> (a -> a -> a) ->
   (a -> a) -> (a -> a -> a) ->
   Stack i a -> Stack i a
recip rec times neg plus (Stack is s) =
   let go (Value a) = Value $ rec a
       go (Plus a d) =
          let ra = go a
              rd = go (addMatch plus a d)
          in  Plus ra $
                 fmap neg $ mulMatch times plus d $ mulMatch times plus ra rd
       -- 1/(a+d) - 1/a = -d/(a*(a+d))
   in  Stack is $ go s

instance (Ord i, Fractional a) => Fractional (Stack i a) where
   fromRational = singleton . fromRational
   -- cf. limitations of Arith.recip
   -- recip = recip P.recip (*) P.negate (+)
   recip = fromMultiValueNum . P.recip . toMultiValueNum
   x / y = fromMultiValueNum $ toMultiValueNum x / toMultiValueNum y


instance (Ord i, Arith.Sum a) => Arith.Sum (Stack i a) where
   negate (Stack is s) = Stack is $ fmap Arith.negate s
   (~+) = add (~+) (\x -> x~-x)

instance (Ord i, Arith.Product a) => Arith.Product (Stack i a) where
   (~*) = mul (~*) (~+)
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

instance (Ord i, Arith.Constant a) => Arith.Constant (Stack i a) where
   zero = singleton Arith.zero
   fromInteger = singleton . Arith.fromInteger
   fromRational = singleton . Arith.fromRational

instance (Ord i, Arith.Integrate v) => Arith.Integrate (Stack i v) where
   type Scalar (Stack i v) = Stack i (Arith.Scalar v)
   integrate (Stack is a) = Stack is $ fmap Arith.integrate a


singleton :: a -> Stack i a
singleton = Stack [] . Value

deltaPair :: i -> a -> a -> Stack i a
deltaPair i a d = Stack [i] $ Plus (Value a) (Value d)

-- could be a simple Semigroup.Foldable.head if it would exist
absolute :: Stack i a -> a
absolute (Stack _ s) = fold const s

normalize :: (Arith.Product a) => Stack i a -> Stack i a
normalize s = fmap (~/ absolute s) s


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


data Branch = Before | Delta deriving (Eq, Show)

instance QC.Arbitrary Branch where
   arbitrary = QC.elements [Before, Delta]


{- |
With the Map you can choose
whether you want to keep only the Before or only the Delta part of a variable.
A missing entry in the Map means that both branches are maintained.
-}
filter :: (Ord i) => Map i Branch -> Stack i a -> Stack i a
filter cond (Stack is0 s) =
   let go [] (Value a) = Value a
       go (i:is) (Plus a d) =
          case Map.lookup i cond of
             Nothing -> Plus (go is a) (go is d)
             Just Before -> go is a
             Just Delta  -> go is d
       go _ _ = error "inconsistent data structure"
   in  Stack
          (Set.toAscList $ Set.difference (Set.fromList is0) (Map.keysSet cond))
          (go is0 s)


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

   shrink (Stack it tree) =
      (case tree of
         Value _ -> []
         Plus a0 a1 ->
            concatMap (\(_,is) -> [Stack is a0, Stack is a1]) $
            ListHT.removeEach it)
      ++
      (let go (Value x) = map Value $ QC.shrink x
           go (Plus a0 a1) =
              map (flip Plus a1) (go a0) ++ map (Plus a0) (go a1)
       in  map (Stack it) $ go tree)
