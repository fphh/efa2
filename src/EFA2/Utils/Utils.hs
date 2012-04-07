

module EFA2.Utils.Utils where

import qualified Data.Vector.Unboxed as UV

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M

import Data.Graph.Inductive

import Debug.Trace

-- A debugging function for displaying arguments in pointless style.
showarg :: (Show a) => a -> a
showarg x = trace (show x) x


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

for :: [a] -> (a -> b) -> [b]
for = flip map

sameValue :: a -> Double
sameValue = const 1.0

pairs :: [a] -> [(a, a)]
pairs xs = zipWith (,) xs (tail xs)

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


foldGraph :: (a -> ([Node], Node, [Node]) -> a) -> a -> Gr b c -> a
foldGraph f start g = L.foldl' f start (zip3 ins ns outs)
  where ns = nodes g
        ins = map (pre g) ns
        outs = map (suc g) ns


mapGraph :: (([Node], Node, [Node]) -> a) -> Gr b c -> [a]
mapGraph f g = map f (zip3 ins ns outs)
  where ns = nodes g
        ins = map (pre g) ns
        outs = map (suc g) ns

-- | mapping a function over a list with using to neighbouring elements 
dmap :: (a -> a -> b) -> [a] -> [b]
dmap f l = zipWith f (init l) (tail l)  

-- | generate an list of indices for a list  
listIdx :: [a] -> [Int]
listIdx list = [0..(length list)-1]

-- | generate a indexed List
idxList :: [a] -> [(Int,a)] 
idxList list = zip (listIdx list) list 

-- | own list show function to provide newline with each element
myShowList :: Show a => [a] -> String
myShowList list = unlines (map show list) 

-- | own list show function using specified show function for element
myShowListFunct :: [a] -> (a -> String) -> String
myShowListFunct list showFunct = unlines (map showFunct list) 


transClose :: Gr a b -> Gr a ()
transClose = efilter (\(x, y, _) -> x /= y) . trc

