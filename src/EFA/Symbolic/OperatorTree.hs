
module EFA.Symbolic.OperatorTree where

import qualified EFA.Symbolic.SumProduct as Term
import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

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
             Const x -> Format.ratio x
             Atom x -> formatValue x

             x :+ y -> Format.parenthesize $ Format.plus (go x) (go y)
             x :* y -> Format.multiply (go x) (go y)

             Recip x -> Format.recip $ go x
             Minus x -> Format.minus $ Format.parenthesize $ go x
   in  go


--------------------------------------------------------------------


expand :: Term a -> NonEmpty.T [] (Term a)
expand = go
  where go (Minus u) = fmap Minus (go u)
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
simplifyOld = iterateUntilFix simplify' . NonEmpty.sum . expand
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

evaluate :: Fractional b => (a -> b) -> Term a -> b
evaluate f =
   let go t =
          case t of
             Atom a -> f a
             Const x -> fromRational x

             Minus x -> negate $ go x
             Recip x -> recip $ go x
             x :+ y -> go x + go y
             x :* y -> go x * go y
   in  go

toNormalTerm :: Ord a => Term a -> Term.Term a
toNormalTerm = evaluate Term.Atom

fromNormalTerm :: Ord a => Term.Term a -> Term a
fromNormalTerm = Term.evaluate Atom


delta :: Term (Idx.Record Idx.Absolute a) -> Term (Idx.Record Idx.Delta a)
delta =
   let before = fmap (\(Idx.Record Idx.Absolute a) -> (Idx.Record Idx.Before a))
       go (Const _) = Const 0
       go (Atom (Idx.Record Idx.Absolute a)) = (Atom (Idx.Record Idx.Delta a))
       go (Minus t) = Minus $ go t
       go (s :+ t) = go s + go t
       go (Recip s) =
          let bs = before s ; ds = go s
              --  recip (s+ds) - recip s
              --  (s-(s+ds)) / ((s+ds) * s)
          in  -ds / ((bs+ds) * bs)
       go (s :* t) =
          let bs = before s ; ds = go s
              bt = before t ; dt = go t
          in  ds * bt + bs * dt + ds * dt
   in  go
