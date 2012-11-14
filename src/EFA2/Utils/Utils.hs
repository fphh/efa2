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


reverseMap :: (Ord b) => M.Map a b -> M.Map b a
reverseMap = M.fromList . map swap . M.toList

-- M.fromSet is available from containers-0.5
mapFromSet ::
   (Ord key) => (key -> a) -> S.Set key -> M.Map key a
mapFromSet f = M.fromAscList . map (\k -> (k, f k)) . S.toAscList

differenceMapSet ::
   (Ord key) => M.Map key a -> S.Set key -> M.Map key a
differenceMapSet m s = M.difference m (mapFromSet (const ()) s)

intersectionMapSet ::
   (Ord key) => M.Map key a -> S.Set key -> M.Map key a
intersectionMapSet m s = M.intersection m (mapFromSet (const ()) s)


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

{- |
integer division where the quotient is rounded up
(In contrast to that, 'div' rounds down.)
-}
divUp :: (Integral a) => a -> a -> a
divUp x y = - div (-x) y



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
