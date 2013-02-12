

module Main where

import qualified Data.List as L

type Pts = [(Double, Double)]

lagrange :: (Enum b, Fractional b) => b -> b -> b -> [(b, b)] -> [(b, b)]
lagrange from to step pts = res
  where xs = [from, from + step .. to]
        poly x = L.foldl' f 0 [0..length pts - 1]
          where f acc k = acc + lFak fpts f k x * s
                  where (as, (f, s):bs) = splitAt k pts
                        fpts = map fst (as ++ bs)
        res = map (\x -> (x, poly x)) xs

lFak :: Fractional a => [a] -> a -> Int -> a -> a
lFak pts b k x = num / denom
  where num = L.foldl' (f x) 1 pts
        denom = L.foldl' (f b) 1 pts
        f x acc v = (x-v)*acc


pts :: [(Double, Double)]
pts = [
  (1.0, 0.416),
  (4.0, 1.041),
  (6.0, 1.255),
  (8.0, 1.359),
  (10.0, 1.416) ]


main :: IO ()
main =
  let res = lagrange 1 10 0.1 pts
      f (x, y) = show x ++ " " ++ show y
  in  putStrLn $ L.intercalate "\n" (map f res)

