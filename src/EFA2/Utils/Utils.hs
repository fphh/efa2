{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module EFA2.Utils.Utils where

import qualified Data.Vector.Unboxed as UV

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe

import Data.Graph.Inductive

import Debug.Trace

-- A debugging function for displaying arguments in pointless style.
showarg :: (Show a) => a -> a
showarg x = trace (show x) x

safeLookup :: (Ord k, Show k) => M.Map k v -> k -> v
safeLookup m k = case M.lookup k m of
                      Nothing -> error $ "safeLookup: " ++ show k
                      Just x -> x

-- generalized unique
gunique :: (Ord a) => S.Set a -> [a] -> [a]
gunique s = go s 
  where go _ [] = []
        go s (x:xs) | S.member x s = go s xs
                    | otherwise    = x : go (S.insert x s) xs

unique :: (Ord a) => [a] -> [a]
unique = gunique S.empty  -- 
-- unique = S.toList . S.fromList

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

thd3 :: (a, b, c) -> c
thd3 (_, _, x) = x

flipPair :: (a, b) -> (b, a)
flipPair (a, b) = (b, a)

reverseMap :: (Ord b) => M.Map a b -> M.Map b a
reverseMap = M.fromList . map flipPair . M.toList

for :: [a] -> (a -> b) -> [b]
for = flip map

sameValue :: a -> Double
sameValue = const 1.0

pairs :: [a] -> [(a, a)]
pairs xs = zipWith (,) xs (tail xs)

{-
class Transpose a where
      transpose :: [a] -> [a]

instance Transpose [a] where
         transpose [] = []
         transpose xs = map (flip map xs) fs
           where fs = take min $ head : (map (. tail) fs)
                 min = L.minimum $ map length xs

instance UV.Unbox a => Transpose (UV.Vector a) where
         transpose [] = []
         transpose xs = map (UV.fromList . flip map xs) fs
           where fs = take min $ map (flip (UV.!)) [0..]
                 min = L.minimum $ map UV.length xs
-}

{-
foldGraph :: Graph gr => (a -> ([Node], Node, [Node]) -> a) -> a -> gr b c -> a
foldGraph f start g = L.foldl' f start (zip3 ins ns outs)
  where ns = nodes g
        ins = map (pre g) ns
        outs = map (suc g) ns
-}

type InOutGraphFormat a = ([a], a, [a])

mkInOutGraphFormat :: Graph gr => (LNode a -> c) -> gr a b -> [InOutGraphFormat c]
mkInOutGraphFormat f g = zip3 (map (map f) ins) (map f ns) (map (map f) outs)
 where ns = labNodes g
       ins = map (h pre) ns
       outs = map (h suc) ns
       h next (n, _) = map (\p -> (p, fromJust (lab g p))) (next g n)

foldGraph :: Graph gr => (a -> InOutGraphFormat (LNode b) -> a) -> a -> gr b c -> a
foldGraph f start g = L.foldl' f start (mkInOutGraphFormat id g)

foldGraphNodes :: Graph gr => (a -> InOutGraphFormat Node -> a) -> a -> gr b c -> a
foldGraphNodes f start g = L.foldl' f start (mkInOutGraphFormat fst g)

foldGraphLabels :: Graph gr => (a -> InOutGraphFormat b -> a) -> a -> gr b c -> a
foldGraphLabels f start g = L.foldl' f start (mkInOutGraphFormat snd g)

mapGraph :: Graph gr => (InOutGraphFormat (LNode b) -> a) -> gr b c -> [a]
mapGraph f g = map f (mkInOutGraphFormat id g)

mapGraphNodes :: Graph gr => (InOutGraphFormat Node -> a) -> gr b c -> [a]
mapGraphNodes f g = map f (mkInOutGraphFormat fst g)

mapGraphLabels :: Graph gr => (InOutGraphFormat b -> a) -> gr b c -> [a]
mapGraphLabels f g = map f (mkInOutGraphFormat snd g)

{-
getLEdge :: Graph gr => gr a b -> Node -> Node -> Maybe (LEdge b)
getLEdge g from to
  | [x] <- res = Just x
  | otherwise = Nothing
  where res = filter f is
        is = inn g to
        f (x, _, _) = x /= from
-}

getLEdge :: (Graph gr) => gr a b -> Node -> Node -> Maybe (LEdge b)
getLEdge g from to =
  case filter f is of
       (x:_) -> Just x
       _ -> Nothing
  where is = inn g to
        f (x, _, _) = x == from

{-
class ContainerArithSingleton cont a where
      csingleton :: a -> cont a
      cappend :: cont a -> cont a -> cont a
      cconcat :: [cont a] -> cont a
      ctranspose :: [cont a] -> [cont a]
      chead :: cont a -> a
      ctail :: cont a -> cont a
      clast :: cont a -> a

class ContainerArith cont a b where
      cmap :: (a -> b) -> cont a -> cont b
      cfoldr :: (a -> b -> b) -> b -> cont a -> b
      czip :: cont a -> cont b -> cont (a, b)
      dmap :: (a -> a -> b) -> cont a -> cont b
      dmap' :: (a -> a -> b) -> cont a -> cont b

class ContainerArithZip cont a b c where
      czipWith :: (a -> b -> c) -> cont a -> cont b -> cont c

instance ContainerArithSingleton [] a where
      csingleton x = [x]
      cappend = (++)
      cconcat = concat
      ctranspose = L.transpose
      chead = head
      ctail = tail
      clast = last

instance ContainerArith [] a b where
         cmap = map
         cfoldr = foldr
         czip = zip
         dmap f l = zipWith f (init l) (tail l)
         dmap' f l = zipWith f (tail l) (init l)

instance ContainerArithZip [] a b c where
         czipWith = zipWith

instance (UV.Unbox a) => ContainerArithSingleton UV.Vector a where
         csingleton x = UV.singleton x
         cappend = (UV.++)
         cconcat = UV.concat
         ctranspose [] = []
         ctranspose xs = map (UV.fromList . flip map xs) fs
           where fs = take min $ map (flip (UV.!)) [0..]
                 min = L.minimum $ map UV.length xs
         chead = UV.head
         ctail = UV.tail
         clast = UV.last

instance (UV.Unbox a, UV.Unbox b) => ContainerArith UV.Vector a b where
         cmap = UV.map
         cfoldr = UV.foldr
         czip = UV.zip
         dmap f l = UV.zipWith f (UV.init l) (UV.tail l)  
         dmap' f l = UV.zipWith f (UV.tail l) (UV.init l)

instance (UV.Unbox a, UV.Unbox b, UV.Unbox c) => ContainerArithZip UV.Vector a b c where
         czipWith = UV.zipWith
-}

-- | mapping a function over a list with using to neighbouring elements 
-- dmap :: (a -> a -> b) -> [a] -> [b]
-- dmap f l = zipWith f (init l) (tail l)  

-- | mapping a function over a list with using to neighbouring elements 
--dmap' :: (a -> a -> b) -> [a] -> [b]
--dmap' f l = zipWith f (tail l) (init l)  


-- | generate an list of indices for a list  
listIdx :: [a] -> [Int]
listIdx list = take (length list) $ iterate (+1) 0

-- | generate a indexed List
idxList :: [a] -> [(Int,a)] 
idxList list = zip (listIdx list) list 

-- | own list show function to provide newline with each element
myShowList :: Show a => [a] -> String
myShowList list = unlines (map show list) 

-- | own list show function using specified show function for element
myShowListFunct :: [a] -> (a -> String) -> String
myShowListFunct list showFunct = unlines (map showFunct list)

-- | Yet another zipping function. A somehow generalised version of 'dmap'.
-- 'prop_ysaf' checks for this property:
-- > length xs > 0 ==> yazf (,) xs xs == dmap (,) xs
yazf :: (a -> b -> c) -> [a] -> [b] -> [c]
yazf f xs ys = zipWith f (init xs) (tail ys)

transClose :: Gr a b -> Gr a ()
transClose = efilter (\(x, y, _) -> x /= y) . trc

diffByAtMostOne :: (Ord a) => S.Set a -> S.Set a -> Bool
diffByAtMostOne s t = (S.size t > 1) && (S.size (t S.\\ s) == 1)

hasSameVariable :: (Ord a) => S.Set a -> S.Set a -> Bool
hasSameVariable s t = S.size (S.intersection s t) > 0


debugLevel = 0

-- mytrace for single values
mytrace dbgLevel function varName var = if debugLevel >= dbgLevel then trace ("myTrace: " ++ show function ++ "-" ++ show varName ++ " : " ++ show var) var else var

-- mytrace for lists
mytraceList dbgLevel function varName var = if debugLevel >= dbgLevel then trace ("myTraceList: " ++ show function ++ "-" ++ show varName ++ " : " ++ myShowList var) var else var
