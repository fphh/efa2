

module EFA2.Utils.Utils where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

-- generalized unique
gunique :: (Ord a) => S.Set a -> [a] -> [a]
gunique s = go s
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs

unique :: (Ord a) => [a] -> [a]
unique = gunique S.empty


flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

reverseMap :: (Ord b) => M.Map a b -> M.Map b a
reverseMap = M.fromList . map flipPair . M.toList

diffByOne :: (Eq a, Ord a) => S.Set a -> S.Set a -> Bool
diffByOne s t = S.size (S.difference t s) == 1

eachWithEvery :: [a] -> [(a, [a])]
eachWithEvery xs = reverse res
  where f (before, y:ys, acc) _ = (y:before, ys, (y, before ++ ys):acc)
        (_, _, res) = L.foldl' f ([], xs, []) xs


sameValue :: a -> Double
sameValue _ = 1.0

