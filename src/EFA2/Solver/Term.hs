module EFA2.Solver.Term where

import qualified Data.Map as Map
import Data.Map (Map, )

import Data.Monoid (Monoid(mappend, mempty), (<>), )

import Data.Maybe (fromMaybe, )


{-
The Ord instance can be misunderstood.
We need it only for the Map.
We might use a custom Map type.
-}
data Term a =
     Atom a
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

instance Ord a => Fractional (Term a) where
   fromRational x = Sum $ Map.singleton one x
   -- (/) is not just unionWith (/) because the elements in the second map would remain unchanged
   recip = termFromCoeffProd . recipCoeffProduct . coeffProdFromTerm
