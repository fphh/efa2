{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, KindSignatures, GeneralizedNewtypeDeriving, FlexibleContexts #-}


module EFA2.Signal.Vector (module EFA2.Signal.Vector) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector as V

import qualified Data.List as L
import qualified Data.List.HT as LH

import Data.Maybe.HT (toMaybe)
import Data.Eq (Eq((==)))
import Data.Function ((.), ($), id, flip)
import Data.Maybe (Maybe)
import Prelude (Bool, Int, Ord, error, (++), (-), Show, show, not)


--------------------------------------------------------------
-- Singleton Class

{-# DEPRECATED head, tail "use viewL instead" #-}
{-# DEPRECATED last, init "use viewR instead" #-}

class Singleton vec d where
   maximum :: vec d -> d
   minimum :: vec d -> d
   singleton :: d -> vec d
   empty :: vec d
   append :: vec d -> vec d -> vec d
   concat :: [vec d] -> vec d
   head :: vec d -> d
   tail :: vec d -> vec d
   last :: vec d -> d
   init :: vec d -> vec d
   viewL :: vec d -> Maybe (d, vec d)
   viewR :: vec d -> Maybe (vec d, d)
   all :: (d -> Bool) -> vec d -> Bool
   any :: (d -> Bool) -> vec d -> Bool

instance (Ord d) => Singleton V.Vector d where
   maximum x = V.maximum x
   minimum x = V.minimum x
   singleton x = V.singleton x
   empty = V.empty
   append = (V.++)
   concat = V.concat
   head = V.head
   tail = V.tail
   last = V.last
   init = V.init
   viewL xs = toMaybe (not $ V.null xs) (V.head xs, V.tail xs)
   viewR xs = toMaybe (not $ V.null xs) (V.init xs, V.last xs)
   all = V.all
   any = V.any

instance (Ord d, UV.Unbox d) => Singleton UV.Vector d where
   maximum x = UV.maximum x
   minimum x = UV.minimum x
   singleton x = UV.singleton x
   empty = UV.empty
   append = (UV.++)
   concat = UV.concat
   head = UV.head
   tail = UV.tail
   last = UV.last
   init = UV.init
   viewL xs = toMaybe (not $ UV.null xs) (UV.head xs, UV.tail xs)
   viewR xs = toMaybe (not $ UV.null xs) (UV.init xs, UV.last xs)
   all = UV.all
   any = UV.any

instance (Ord d) => Singleton [] d where
   maximum x = L.maximum x
   minimum x = L.minimum x
   singleton x = [x]
   empty = []
   append = (++)
   concat = L.concat
   head = L.head
   tail = L.tail
   last = L.last
   init = L.init
   viewL = LH.viewL
   viewR = LH.viewR
   all = L.all
   any = L.any

------------------------------------------------------------
-- | Functor
class Walker vec a b where
   map :: (a -> b) -> vec a -> vec b
   foldr :: (a -> b -> b) -> b -> vec a -> b
   foldl :: (b -> a -> b) -> b -> vec a -> b
   zip :: vec a -> vec b -> vec (a, b)

instance Walker [] a b where
   map = L.map
   foldr = L.foldr
   foldl = L.foldl'
   zip = L.zip

instance (UV.Unbox a, UV.Unbox b) => Walker UV.Vector a b where
   map = UV.map
   foldr = UV.foldr
   foldl = UV.foldl'
   zip = UV.zip

instance Walker V.Vector a b where
   map = V.map
   foldr = V.foldr
   foldl = V.foldl'
   zip = V.zip


------------------------------------------------------------
-- | Zipper

class Zipper vec a b c where
   zipWith :: (a -> b -> c) -> vec a -> vec b -> vec c

instance Zipper V.Vector a b c  where
   zipWith f x y = V.zipWith f x y -- if V.lenCheck x y then V.zipWith f x y else error "Error in V.lenCheck V -- unequal Length"

instance (UV.Unbox a, UV.Unbox b, UV.Unbox c) => Zipper UV.Vector a b c  where
   zipWith f x y = UV.zipWith f x y --if V.lenCheck x y then UV.zipWith f x y else error "Error in V.lenCheck UV -- unequal Length"

instance Zipper [] a b c  where
   zipWith f x y = L.zipWith f x y -- if V.lenCheck x y then zipWith f x y else error "Error in V.lenCheck List -- unequal Length"


vdeltaMap :: (Singleton vec b, Zipper vec b b c) => (b -> b -> c) -> vec b -> vec c
vdeltaMap f l = zipWith f l (tail l)

vdeltaMapReverse :: (Singleton vec b, Zipper vec b b c) => (b -> b -> c) -> vec b -> vec c
vdeltaMapReverse f l = zipWith f (tail l) l

------------------------------------------------------------
-- | Zipper4

class Zipper4 vec a b c d e where
   zipWith4 :: (a -> b -> c -> d -> e) -> vec a -> vec b -> vec c -> vec d -> vec e

instance Zipper4 V.Vector a b c d e where
   zipWith4 f w x y z = V.zipWith4 f w x y z -- if V.lenCheck x y then V.zipWith f x y else error "Error in V.lenCheck V -- unequal Length"

instance (UV.Unbox a, UV.Unbox b, UV.Unbox c, UV.Unbox e, UV.Unbox d) => Zipper4 UV.Vector a b c d e   where
   zipWith4 f w x y z = UV.zipWith4 f w x y z --if V.lenCheck x y then UV.zipWith f x y else error "Error in V.lenCheck UV -- unequal Length"

instance Zipper4 [] a b c d e  where
   zipWith4 f w x y z = L.zipWith4 f w x y z -- if V.lenCheck x y then zipWith f x y else error "Error in V.lenCheck List -- unequal Length"

{-
vdeltaMap2:: (a -> a -> b -> b -> c)  -> vec a -> vec a -> vec b  -> vec b -> vec c
vdeltaMap2 f xs ys = zipWith4 f xs (tail xs) ys (tail ys)

vdeltaMapReverse2 :: (a -> a -> b -> b -> c) -> vec a -> vec a -> vec b  -> vec b -> vec c
vdeltaMapReverse2 f xs ys = zipWith4 f (tail xs) xs (tail ys) ys
-}
--------------------------------------------------------------
-- Vector conversion
class Box c1 c2 a where
   box :: c1 a -> c2 a
   unbox :: c2 a -> c1 a

instance UV.Unbox a => Box UV.Vector V.Vector a where
   box x = UV.convert x
   unbox x = V.convert x

instance Box [] [] a where
   box = id
   unbox = id

--------------------------------------------------------------
-- Vector conversion
class Convert c1 c2 a where
   convert :: c1 a -> c2 a

instance UV.Unbox a => Convert UV.Vector V.Vector a where
   convert x = UV.convert x

instance UV.Unbox a => Convert V.Vector UV.Vector a where
   convert x = V.convert x

instance UV.Unbox a => Convert UV.Vector UV.Vector a where
   convert x = V.convert x

instance Convert V.Vector V.Vector a where
   convert = id

instance Convert [] [] a where
   convert = id

instance UV.Unbox a => Convert [] UV.Vector a where
   convert x = UV.fromList x

instance Convert [] V.Vector a where
   convert x = V.fromList x

instance Convert V.Vector [] a where
   convert x = V.toList x

instance UV.Unbox a => Convert UV.Vector [] a where
   convert x = UV.toList x

--------------------------------------------------------------
-- Length & Length Check

class Length s where
   len :: s -> Int

instance Length (V.Vector d) where
   len x = V.length x

instance UV.Unbox d => Length  (UV.Vector d) where
   len x = UV.length x

instance Length  [d] where
   len x = L.length x


lenCheck ::
   (Length v1, Length v2) =>
   v1 -> v2 -> Bool
lenCheck x y = len x == len y


--------------------------------------------------------------
-- Transpose Classe
class Transpose v1 v2 d where
   transpose :: (v2 (v1 d)) -> (v2 (v1 d))


instance Transpose V.Vector V.Vector d where
   transpose xs = if all (== len0) lens then V.map (flip V.map xs) fs else error "Error in V.Transpose -- unequal length"
    where fs = V.map (flip (V.!)) $ V.fromList [0..len0-1]
          lens = V.map len xs
          len0 = V.head lens

instance (UV.Unbox d) => Transpose UV.Vector V.Vector d where
   transpose xs = if all (== len0) lens then V.map (unbox . flip V.map xs) fs else error "Error in V.Transpose -- unequal length"
    where fs = V.map (flip (UV.!)) $ V.fromList [0..len0-1]
          lens = V.map len xs
          len0 = V.head lens

instance Transpose [] [] d where
   transpose x = if L.all (== L.head lens) lens then L.transpose x else error "Error in V.Transpose -- unequal length"
                         where lens = map len x



class FromList v d where
   fromList :: [d] -> v d
   toList :: v d -> [d]

instance (UV.Unbox d) => FromList UV.Vector d where
   fromList x = UV.fromList x
   toList x = UV.toList x

instance FromList V.Vector d where
   fromList x = V.fromList x
   toList x = V.toList x

instance FromList [] d where
   fromList x = x
   toList x = x


class Sort v d where
   sort :: v d -> v d

instance Ord d => Sort [] d where
   sort = L.sort

instance (Ord d, UV.Unbox d) => Sort UV.Vector d where
   sort x = UV.fromList $ L.sort $ UV.toList x

instance Ord d => Sort V.Vector d where
   sort x = V.fromList $ L.sort $ V.toList x


class Filter v d where
   filter :: (d -> Bool) -> v d -> v d

instance Filter [] d where
   filter f x = L.filter f x

instance Filter V.Vector d where
   filter f x = V.filter f x

instance UV.Unbox d => Filter UV.Vector d where
   filter f x = UV.filter f x


class Lookup v d where
   lookUp :: v d -> [Int] -> v d

instance (Eq d) => Lookup [] d where
   lookUp xs = lookUpGen (V.fromList xs V.!? )

instance (Eq d) => Lookup V.Vector d where
   lookUp xs = V.fromList . lookUpGen (xs V.!?)

instance (UV.Unbox d, Eq d) => Lookup UV.Vector d where
   lookUp xs = UV.fromList . lookUpGen (xs UV.!?)

{-# INLINE lookUpGen #-}
lookUpGen :: Show i => (i -> Maybe a) -> [i] -> [a]
lookUpGen look idxs =
   case LH.partitionMaybe look idxs of
      (ys, []) -> ys
      (_, invalidIdxs) ->
         error $ "Error in vLookup - indices out of Range: " ++ show invalidIdxs


class Reverse v d where
   reverse :: v d -> v d

instance Reverse [] d where
   reverse = L.reverse

instance Reverse V.Vector d where
   reverse = V.reverse

instance  (UV.Unbox d) => Reverse UV.Vector d where
   reverse = UV.reverse
