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

safeLookup :: (Ord k, Show k, Show v) => M.Map k v -> k -> v
safeLookup m k = case M.lookup k m of
                      Nothing -> error $ "safeLookup: " ++ show k ++ "\n" ++ show m
                      Just x -> x

checkJust :: String -> Maybe a -> a
checkJust _ (Just x) = x
checkJust str _ = error ("checkJust called from " ++ str)

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

const2 x _ _ = x


type InOutGraphFormat a = ([a], a, [a])

mkInOutGraphFormat :: Graph gr => (LNode a -> c) -> gr a b -> [InOutGraphFormat c]
mkInOutGraphFormat f g = zip3 (map (map f) ins) (map f ns) (map (map f) outs)
 where ns = labNodes g
       ins = map (h pre) ns
       outs = map (h suc) ns
       h next (n, _) = map (\p -> (p, fromJust (lab g p))) (next g n)

getInOutGraphFormatFromNode :: Graph gr => (LNode a -> c) -> gr a b -> Node -> InOutGraphFormat c
getInOutGraphFormatFromNode f g n = (map f lins, f l, map f louts)
  where l = (n, fromJust $ lab g n)
        ins = pre g n
        lins = zip ins (map (fromJust . lab g) ins)
        outs = suc g n
        louts = zip outs (map (fromJust . lab g) outs)

-- | Breadth first.
mkInOutGraphFormatBfs :: Graph gr => (LNode a -> c) -> gr a b -> [InOutGraphFormat c]
mkInOutGraphFormatBfs f g = map (getInOutGraphFormatFromNode f g) ns
  where ns = bfs n g
        ((_, n, _, _), _) = matchAny g
  


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

getLEdge :: (Graph gr) => gr a b -> Node -> Node -> Maybe (LEdge b)
getLEdge g from to =
  case filter f is of
       (x:_) -> Just x
       _ -> Nothing
  where is = inn g to
        f (x, _, _) = x == from

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
--
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
mytrace dbgLevel function varName var | debugLevel >= dbgLevel = 
  trace ("myTrace: " ++ show function ++ "-" ++ show varName ++ " : " ++ show var) var 
mytrace _ _ _ var = var

-- mytrace for lists
mytraceList dbgLevel function varName var | debugLevel >= dbgLevel = 
  trace ("myTraceList: " ++ show function ++ "-" ++ show varName ++ " : " ++ myShowList var) var
mytraceList _ _ _ var = var
