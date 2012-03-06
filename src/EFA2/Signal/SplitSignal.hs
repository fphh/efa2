

module EFA2.Signal.SplitSignal where


import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Vector.Unboxed as UV

import System.Random

import EFA2.Utils.Utils
import EFA2.Signal.SignalData

data Sign = PSign | ZSign | MSign deriving (Show, Eq)

sign :: (Eq a, Ord a, Num a) => a -> Sign
sign x | x > 0 = PSign
       | x == 0 = ZSign
       | x < 0 = MSign

makeSteps :: (UV.Unbox a, Ord a, Num a) => UV.Vector a -> [Int]
makeSteps ss | UV.length ss == 0 = []
makeSteps ss = 0 : (reverse (UV.length ss : (res $ UV.foldl f (s, 0, []) ss)))
  where s = sign (UV.head ss)
        res (_, _, acc) = acc
        f (s, i, acc) x = if sign x == s then (s, i+1, acc) else (sign x, i+1, i:acc)

makeStepList :: (UV.Unbox a, Ord a, Num a, Show a) => [UV.Vector a] -> [Int]
makeStepList xs = S.toAscList s
  where offs = map (makeSteps) xs
        s = S.unions $ map S.fromList offs

splitList :: (UV.Unbox a, Show a) => [Int] -> UV.Vector a -> [UV.Vector a]
splitList [] _ = []
splitList _ vec | UV.length vec == 0 = []
splitList os vec = map f idxs
  where idxs = zip os (zipWith (-) (tail os) os)
        f (from, to) = UV.slice from to vec


equalLengths :: (UV.Unbox a) => [UV.Vector a] -> Bool
equalLengths vec | length vec == 0 = True
equalLengths xs = and (map (== n) ns)
  where (n:ns) = map UV.length xs


toEqualLengths :: (UV.Unbox a) => [UV.Vector a] -> [UV.Vector a]
toEqualLengths xs = map (UV.take min) xs
  where min = L.minimum $ map UV.length xs

splitVectors :: (UV.Unbox a, Ord a, Num a) => [UV.Vector a] -> [[UV.Vector a]]
splitVectors ss = transpose $ map (splitList steps) ss'
  where steps = makeStepList ss
        ss' = toEqualLengths ss
