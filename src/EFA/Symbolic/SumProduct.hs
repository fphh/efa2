module EFA.Symbolic.SumProduct where

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~-), (~*), (~/))

import qualified EFA.Report.Format as Format
import EFA.Report.FormatValue (FormatValue, formatValue)

import EFA.Utility (Pointed, point)

import qualified Data.Map as Map
import Data.Map (Map, )

import qualified Data.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.NonEmpty as NonEmpty ((!:))
import Data.Monoid (Monoid(mappend, mempty), (<>), )
import Data.Maybe (fromMaybe, )

import Data.Ratio (numerator, denominator)


{-
The Ord instance can be misunderstood.
We need it only for the Map.
We might use a custom Map type.
-}
data Term a =
     Atom a
   | Function Format.Function (Term a)
   | Sum (Map (Product a) Rational)
   deriving (Show, Eq, Ord)

{-
Invariant:
Sub-sums must always have at least two summands.
Otherwise they can be fused with the surrounding product.

It would be cool if we could express this with types.
-}
newtype Product a =
   Product (Map (Term a) Integer)
   deriving (Show, Eq, Ord)

instance Ord a => Monoid (Product a) where
   mempty = one
   mappend (Product x) (Product y) =
      Product $ Map.filter (/=0) $ Map.unionWith (+) x y

recipProduct :: Product a -> Product a
recipProduct (Product p) = Product (fmap negate p)


data CoeffProduct a =
   CoeffProduct Rational (Product a)
   deriving (Show, Eq, Ord)

instance Ord a => Monoid (CoeffProduct a) where
   mempty = CoeffProduct 1 one
   mappend (CoeffProduct xc xp) (CoeffProduct yc yp) =
      CoeffProduct (xc*yc) (xp<>yp)

recipCoeffProduct :: CoeffProduct a -> CoeffProduct a
recipCoeffProduct (CoeffProduct c p) =
   CoeffProduct (recip c) (recipProduct p)


zero :: Term a
zero = Sum Map.empty

one :: Product a
one = Product Map.empty

sumFromTerm :: Term a -> Map (Product a) Rational
sumFromTerm (Sum s) = s
sumFromTerm t = Map.singleton (productFromTerm t) 1

{-
termFromProduct :: Product a -> Term a
termFromProduct p@(Product m) =
   case Map.toList m of
      [(x,1)] -> x
      _ -> Sum $ Map.singleton p 1
-}

productFromTerm :: Term a -> Product a
productFromTerm x = Product $ Map.singleton x 1

termFromCoeffProd :: CoeffProduct a -> Term a
termFromCoeffProd (CoeffProduct c p@(Product m)) =
   case (c, Map.toList m) of
      (0, _) -> zero
      (1, [(x,1)]) -> x
      _ -> Sum $ Map.singleton p c

coeffProdFromTerm :: Term a -> CoeffProduct a
coeffProdFromTerm x = fromMaybe (CoeffProduct 1 $ productFromTerm x) $ do
   Sum xs <- Just x
   case Map.toList xs of
      [] -> Just $ CoeffProduct 0 one
      [(xb,xc)] -> Just $ CoeffProduct xc xb
      _ -> Nothing

instance Ord a => Num (Term a) where
   fromInteger n = Sum $ Map.singleton one $ fromInteger n
   x+y =
      Sum $ Map.filter (/=0) $
      Map.unionWith (+) (sumFromTerm x) (sumFromTerm y)
   -- (-) is not just unionWith (-) because the elements in the second map would remain unchanged
   negate = Sum . fmap negate . sumFromTerm
   x*y =
      termFromCoeffProd $
      coeffProdFromTerm x <> coeffProdFromTerm y

   abs = Function Format.Absolute
   signum = Function Format.Signum

instance Ord a => Fractional (Term a) where
   fromRational x = Sum $ Map.singleton one x
   -- (/) is not just unionWith (/) because the elements in the second map would remain unchanged
   recip = termFromCoeffProd . recipCoeffProduct . coeffProdFromTerm



instance Ord idx => Arith.Sum (Term idx) where
   (~+) = (+)
   (~-) = (-)
   negate = negate

instance Ord idx => Arith.Product (Term idx) where
   (~*) = (*)
   (~/) = (/)
   recip = recip
   constOne = Function Format.ConstOne

instance Ord idx => Arith.Constant (Term idx) where
   zero = zero
   fromInteger = fromInteger
   fromRational = fromRational


{- |
'evaluate' tries hard to avoid
multiplications with one and additions with zero.
Thus 'evaluate' can be used to generate symbolic expressions
in other representations without clutter.
-}
evaluate :: (Ord a, Fractional b) => (a -> b) -> Term a -> b
evaluate f =
   let term t =
          case t of
             Atom a -> f a
             Function fn x ->
                case fn of
                   Format.Absolute -> abs $ term x
                   Format.Signum -> signum $ term x
                   Format.ConstOne -> 1
             Sum s ->
                case NonEmpty.fetch $ Map.toList s of
                   Nothing -> 0
                   Just ss -> NonEmpty.sum $ fmap (uncurry prod) ss

       prod (Product p) c =
          case Map.partition (>0) $ Map.filter (0/=) p of
             (norm, rec) ->
                case fmap NonEmpty.product $ NonEmpty.fetch $ powers $ fmap negate rec of
                   Nothing -> normProd c norm
                   Just mp ->
                      case (c, Map.null norm) of
                         ( 1, True) -> recip mp
                         (-1, True) -> negate $ recip mp
                         _ -> normProd c norm / mp

       normProd c p =
          case powers p of
             ps ->
                case (c, fmap NonEmpty.product $ NonEmpty.fetch ps) of
                   ( 1, Just mp) -> mp
                   (-1, Just mp) -> negate mp
                   _ -> NonEmpty.product $ fromRational c !: ps

       powers =
          List.map (\(x, e) -> power e $ term x) . Map.toList

       {- |
       exponent must be positive (not zero or negative)
       -}
       power :: (Fractional a) => Integer -> a -> a
       power e t =
          NonEmpty.product $ t !: List.genericReplicate (e-1) t

   in  term


map :: (Ord b) => (a -> b) -> Term a -> Term b
map f =
   let mapTerm (Atom a) = Atom $ f a
       mapTerm (Function fn a) = Function fn $ mapTerm a
       mapTerm (Sum s) = Sum $ Map.mapKeys mapProduct s
       mapProduct (Product p) = Product $ Map.mapKeys mapTerm p
   in  mapTerm


{- |
for minimal use of parentheses
-}
data FormatContext = TopLevel | Power deriving (Show, Eq, Ord)

format ::
   (Ord a, Format.Format output) =>
   (FormatContext -> a -> output) ->
   FormatContext -> Term a -> output
format f =
   let term ctx t =
          case t of
             Atom a -> f ctx a
             Function fn x -> Format.function fn $ term TopLevel x
             Sum s ->
                case NonEmpty.fetch $ Map.toList s of
                   Nothing -> Format.integer 0
                   Just ss ->
                      (if ctx > TopLevel
                         then Format.parenthesize
                         else id) $
                      NonEmpty.foldl1 Format.plus $ fmap (uncurry prod) ss

       prod (Product p) c =
          case fmap (NonEmpty.foldl1 Format.multiply) $
               NonEmpty.fetch $ powers p of
             Nothing -> ratio c
             Just mp ->
                case c of
                   1 -> mp
                   -1 -> Format.negate mp
                   _ -> Format.multiply (ratio c) mp

       ratio x =
          if denominator x == 1
            then Format.integer $ numerator x
            else Format.ratio x

       powers =
          List.map (\(x, e) -> power e $ term Power x) . Map.toList

       power e t =
          case e of
             1 -> t
             _ -> Format.power t e

   in  term


instance Pointed Term where
   point = Atom

instance (Ord a, FormatValue a) => FormatValue (Term a) where
   formatValue = format (\_ -> formatValue) TopLevel
