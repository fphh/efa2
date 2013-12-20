module EFA.Utility where

import qualified Data.Stream as Stream
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.Foldable as Fold
import Data.Traversable (Traversable, mapAccumL)
import Data.Stream (Stream)
import Data.Bool.HT (if')


checkJust :: String -> Maybe a -> a
checkJust _ (Just x) = x
checkJust str _ = error ("checkJust called from " ++ str)


zipWithTraversable ::
   (Traversable f) => (a -> b -> c) -> Stream a -> f b -> f c
zipWithTraversable f as0 =
   snd . mapAccumL (\(Stream.Cons a as) b -> (as, f a b)) as0

mapDiagonal ::
   (Traversable f) =>
   (a -> b) ->
   (a -> b) ->
   f a -> [f b]
mapDiagonal deflt diag xs =
   map (\j ->
      zipWithTraversable (\k -> if' (j==k) diag deflt)
         (Stream.iterate (+1) 0) xs) $
   Match.take (Fold.toList xs) [0::Int ..]


for :: [a] -> (a -> b) -> [b]
for = flip map

pairs :: [a] -> [(a, a)]
pairs = ListHT.mapAdjacent (,)

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

(>>!) :: (Monad m) => m () -> m a -> m a
x >>! y = do { x; y }


class Pointed f where
   point :: a -> f a
