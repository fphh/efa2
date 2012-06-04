

module Main where


import Data.Ratio
import qualified Data.List as L
import Data.Monoid

p1 = Prob [ ('a', 1%4),
            ('b', 1%8), 
            ('c', 5%8) ]

p2 = Prob [ ('d', 1%9),
            ('e', 2%9), 
            ('f', 6%9) ]



newtype Prob a = Prob { unProb :: [(a, Rational)] } deriving (Show)

instance Functor Prob where
         fmap f (Prob xs) = Prob (map (\(x, p) -> (f x, p)) xs)


join :: Prob (Prob a) -> Prob a
join (Prob ps) = Prob (concat $ map f ps)
  where f (Prob qs, q) = map (g q) qs
        g q (x, r) = (x, q*r)

instance Monad Prob where
         return x = Prob [(x, 1%1)]
         xs >>= f = join (fmap f xs)

sanityCheck :: Prob a -> Bool
sanityCheck (Prob xs) = sum (map f xs) == 1
  where f (_, p) = p

test :: Prob (Char, Char)
test = do
  a <- p1
  b <- p2
  return (a, b)


pretty :: (Show a) => Prob a -> String
pretty (Prob xs) = L.intercalate "\n" $ map f xs
  where f (x, p) = show x ++ " " ++ show p


main :: IO ()
main = do
  print (sanityCheck p1)
  print (sanityCheck p2)
  print (sanityCheck test)
  putStrLn (pretty test)