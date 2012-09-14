module EFA2.Utils.Utils where

import qualified Data.List.Match as Match
import qualified Data.List.HT as LH
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Tuple.HT (swap)


safeLookup :: (Ord k, Show k, Show v) => M.Map k v -> k -> v
safeLookup m k = case M.lookup k m of
                      Nothing -> error $ "safeLookup: " ++ show k ++ "\n" ++ show m
                      Just x -> x

checkJust :: String -> Maybe a -> a
checkJust _ (Just x) = x
checkJust str _ = error ("checkJust called from " ++ str)


{-
This function could become unnecessary
if we store the node set of a graph in a Set.
-}
unique :: (Ord a) => [a] -> [a]
unique = S.toList . S.fromList

reverseMap :: (Ord b) => M.Map a b -> M.Map b a
reverseMap = M.fromList . map swap . M.toList

for :: [a] -> (a -> b) -> [b]
for = flip map

sameValue :: a -> Double
sameValue = const 1.0

pairs :: [a] -> [(a, a)]
pairs = LH.mapAdjacent (,)

-- | Yet another zipping function. A somehow generalised version of 'dmap'.
-- 'prop_ysaf' checks for this property:
--
-- > length xs > 0 ==> yazf (,) xs xs == dmap (,) xs
yazf :: (a -> b -> c) -> [a] -> [b] -> [c]
yazf f xs ys = zipWith f (init xs) (tail ys)

{- |
@const2@ can also be written as @const . const@
-}
const2 :: a -> b -> c -> a
const2 x _ _ = x


-- | generate an list of indices for a list
listIdx :: [a] -> [Int]
listIdx list = Match.take list $ iterate (+1) 0

-- | generate a indexed List
idxList :: [a] -> [(Int,a)]
idxList list = zip [0..] list

-- | own list show function to provide newline with each element
myShowList :: Show a => [a] -> String
myShowList list = unlines (map show list)

-- | own list show function using specified show function for element
myShowListFunct :: [a] -> (a -> String) -> String
myShowListFunct list showFunct = unlines (map showFunct list)

diffByAtMostOne :: (Ord a) => S.Set a -> S.Set a -> Bool
diffByAtMostOne s t = (S.size t > 1) && (S.size (t S.\\ s) == 1)

hasSameVariable :: (Ord a) => S.Set a -> S.Set a -> Bool
hasSameVariable s t = not $ S.null (S.intersection s t)
