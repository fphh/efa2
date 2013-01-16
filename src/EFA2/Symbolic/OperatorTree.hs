

module EFA2.Symbolic.OperatorTree where

import qualified EFA2.Symbolic.SumProduct as Term
import qualified EFA2.Report.Format as Format
import EFA2.Report.FormatValue (FormatValue, formatValue)

import Control.Monad (liftM2)

import Data.Ratio ((%))

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Stream as Stream
import Data.Stream (Stream)


data Term a =
            Atom a
          | Const Rational
               {- we initialize it only with 0 or 1,
                  but constant folding may yield any rational number -}

          | Minus (Term a)
          | Recip (Term a)
          | (Term a) :+ (Term a)
          | (Term a) :* (Term a) deriving (Show, Eq, Ord)



instance Num (Term idx) where
   fromInteger x = Const (x % 1)
   negate = Minus
   (+) = (:+)
   (*) = (:*)

instance Fractional (Term idx) where
   fromRational = Const
   recip = Recip
   (/) = (&/)


instance Functor Term where
   fmap f =
      let go t =
             case t of
                Atom a -> Atom $ f a
                Const x -> Const x

                Minus x -> Minus $ go x
                Recip x -> Recip $ go x
                x :+ y -> go x :+ go y
                x :* y -> go x :* go y
      in go

infixl 7  :*, &/
infixl 6  :+, &-


{- |
For consistency with '(:+)' it should be named '(:-)'
but this is reserved for constructors.
-}

(&-) :: Term a -> Term a -> Term a
x &- y  =  x :+ Minus y

(&/) :: Term a -> Term a -> Term a
x &/ y  =  x :* Recip y



instance (Eq a, FormatValue a) => FormatValue (Term a) where
   formatValue = formatTerm


formatTerm ::
   (FormatValue a, Format.Format output) => Term a -> output
formatTerm =
   let go t =
          case t of
             Const x -> Format.real (fromRational x :: Double)
             Atom x -> formatValue x

             x :+ y -> Format.parenthesize $ Format.plus (go x) (go y)
             x :* y -> Format.multiply (go x) (go y)

             Recip x -> Format.recip $ go x
             Minus x -> Format.minus $ Format.parenthesize $ go x
   in  go


--------------------------------------------------------------------


pushMult :: Term a -> Term a
pushMult t = Term.add $ go t
  where go :: Term a -> NonEmpty.T [] (Term a)
        go (Minus u) = fmap Minus (go u)
        go (Recip u) = NonEmpty.singleton $ Recip $ pushMult u
        go (u :+ v) = NonEmpty.append (go u) (go v)
        go (u :* v) = liftM2 (:*) (go u) (go v)
        go s = NonEmpty.singleton s

streamPairs :: Stream a -> Stream (a, a)
streamPairs xs = Stream.zip xs (Stream.tail xs)

iterateUntilFix :: (Eq a) => (a -> a) -> a -> a
iterateUntilFix f =
   fst . Stream.head . Stream.dropWhile (uncurry (/=)) .
   streamPairs . Stream.iterate f

simplifyOld :: Eq a => Term a -> Term a
simplifyOld = iterateUntilFix simplify' . pushMult
  where simplify' :: Eq a => Term a -> Term a
        simplify' (Const x :+ Const y) = Const $ x+y
        simplify' ((Const 0.0) :+ x) = simplify' x
        simplify' (x :+ (Const 0.0)) = simplify' x

        simplify' (Const x :* Const y) = Const $ x*y
        simplify' ((Const 1.0) :* x) = simplify' x
        simplify' (x :* (Const 1.0)) = simplify' x
        simplify' ((Const 0.0) :* _) = Const 0.0
        simplify' (_ :* (Const 0.0)) = Const 0.0

        simplify' (Recip (Const x)) = Const $ recip x
        simplify' (x :* (Recip y)) | x == y = Const 1.0
        simplify' ((Minus x) :* (Recip y)) | x == y = Const (-1.0)
        simplify' ((Recip x) :* y) | x == y = Const 1.0
        simplify' ((Recip x) :* (Minus y)) | x == y = Const (-1.0)

        simplify' (Recip (Recip x)) = simplify' x
        simplify' (Recip x) = Recip (simplify' x)

        simplify' (Minus (Const x)) = Const $ negate x
        simplify' (Minus (Minus x)) = simplify' x
        simplify' (Minus x) = Minus (simplify' x)
        simplify' ((Minus x) :* (Minus y)) = simplify' x :* simplify' y
        simplify' (x :+ y) = simplify' x :+ simplify' y
        simplify' (x :* y) = simplify' x :* simplify' y
        simplify' x = x


simplify :: Ord a => Term a -> Term a
simplify = fromNormalTerm . toNormalTerm

toNormalTerm :: Ord a => Term a -> Term.Term a
toNormalTerm =
   let go t =
          case t of
             Atom a -> Term.Atom a
             Const x -> fromRational x

             Minus x -> negate $ go x
             Recip x -> recip $ go x
             x :+ y -> go x + go y
             x :* y -> go x * go y
   in  go

fromNormalTerm :: Ord a => Term.Term a -> Term a
fromNormalTerm = Term.evaluate Atom
