{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeFamilies #-}
module EFA.Data.Vector.NonEmpty where

import qualified EFA.Data.Vector as DV

--import qualified Data.List as List
--import qualified Data.List.HT as ListHT
--import qualified Data.List.Match as Match

--import qualified Data.Set as Set

import qualified Data.NonEmpty as NonEmpty
import qualified Data.NonEmpty.Class as NonEmptyClass
--import qualified Data.Foldable as Fold

--import Data.Functor (Functor)
---import Data.Tuple.HT (mapFst)
--import Data.Maybe.HT (toMaybe)
--import Data.Ord (Ordering, (>=), (<=), (<), (>))
--import Data.Eq (Eq((==)))
--import Data.Function ((.), ($), id, flip)
--import Data.Maybe (Maybe(Just, Nothing), maybe, isJust, fromMaybe)
--import Data.Bool (Bool(False, True), (&&), not)
--import Data.Tuple (snd, fst)
--import Text.Show (Show, show)

import EFA.Equation.Arithmetic as Arith
import EFA.Utility(Caller,
                   merror,(|>),
                   ModuleName(..),FunctionName, genCaller)


import Prelude hiding (map, zip, zipWith, foldl, foldr, head, last)
import qualified Prelude as P

modul :: ModuleName
modul = ModuleName "Signal.Flow"

nc :: FunctionName -> Caller
nc = genCaller modul

newtype T0 v a = T0 (v a) deriving P.Show
newtype T1 v a = T1 (NonEmpty.T v a) deriving P.Show
newtype T2 v a = T2 (NonEmpty.T (NonEmpty.T v) a) deriving P.Show

neMap ::
  (DV.Walker vec,
   DV.Storage vec a,
   DV.Storage vec b) =>
  (a -> b) ->
  NonEmpty.T vec a ->
  NonEmpty.T vec b
neMap f (NonEmpty.Cons x xs) = NonEmpty.Cons (f x) (DV.map f xs) 

class Len (ne :: (* -> *) -> * -> *) vec a where
  len :: ne vec a -> Int
  
instance (DV.Len (vec a)) => Len T0 vec a where  
  len (T0 vec) = DV.len vec
  
instance (DV.Len (vec a)) => Len T1 vec a where  
  len (T1 (NonEmpty.Cons _ vec)) = DV.len vec + 1

instance(DV.Len (vec a))=> Len T2 vec a where  
  len (T2 (NonEmpty.Cons _ (NonEmpty.Cons _ vec))) = DV.len vec + 2

class Walker (ne :: (* -> *) -> * -> *) vec a b where
  map :: (a -> b) -> ne vec a -> ne vec b
  
instance 
  (DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec a)=>
 Walker T0 vec a b where
  map f (T0 vec) = T0 $ DV.map f vec
  
instance 
  (DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec a)=>
 Walker T1 vec a b where   
  map f (T1 ne1) = T1 $ neMap f ne1
  
instance 
  (DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec a) => Walker T2 vec a b where   
  map f (T2 (NonEmpty.Cons x ne1)) = T2 $ NonEmpty.Cons (f x) (neMap f ne1)  

class IWalker (ne :: (* -> *) -> * -> *) vec a b where
  imap :: (Int -> a -> b) -> ne vec a -> ne vec b
  
instance 
  (DV.Walker vec,
   DV.Storage vec b,
   DV.Storage vec a)=>
 IWalker T0 vec a b where
  imap f (T0 vec) = T0 $ DV.imap f vec
  
instance 
  (DV.Walker vec,DV.Zipper vec,
   DV.Storage vec Int,
   FromList T1 vec Int,
   DV.Storage vec (Int, a),
   DV.Len (vec a), 
   DV.Storage vec a,
   DV.Storage vec b) => 
  IWalker T1 vec a b where   
  imap f n = let ivec = fromList (nc "imap") [1..(len n-1)] in zipWith f ivec n
  
instance 
 (DV.Walker vec,
 DV.FromList vec,
 DV.Len (vec a),
 DV.Zipper vec,
 DV.Storage vec Int,
 DV.Storage vec b,
 DV.Storage vec a) =>
 IWalker T2 vec a b where   imap f n = let ivec = fromList (nc "imap") [1..(len n-1)] in zipWith f ivec n

class Fold (ne :: (* -> *) -> * -> *) vec a b where
  foldl :: (acc -> a -> acc) -> acc -> ne vec a -> acc
  
instance 
  (DV.Walker vec, 
   DV.Storage vec a) => 
  Fold T0 vec a b where
  foldl f acc (T0 vec) = DV.foldl f acc vec

instance 
  (DV.Walker vec, 
   DV.Storage vec a, 
   NonEmptyClass.Cons vec) => 
  Fold T1 vec a b where
  foldl f acc (T1 vec) = DV.foldl f acc $ NonEmpty.flatten vec

instance 
  (DV.Walker vec, 
   DV.Storage vec a, 
   NonEmptyClass.Cons vec) => 
  Fold T2 vec a b where
  foldl f acc (T2 vec) = DV.foldl f acc $ NonEmpty.flatten $ NonEmpty.flatten vec


neZipWith f (NonEmpty.Cons x vec) (NonEmpty.Cons x1 vec1) = 
  NonEmpty.Cons (f x x1) $ DV.zipWith f vec vec1

class Zipper (ne :: (* -> *) -> * -> *) vec a b c where
  zipWith :: (a -> b -> c) -> ne vec a -> ne vec b -> ne vec c
  
instance (DV.Zipper vec,
          DV.Storage vec c,
          DV.Storage vec b,
          DV.Storage vec a) => 
         Zipper T0 vec a b c where  
  zipWith f (T0 vec) (T0 vec1) = T0 $ DV.zipWith f vec vec1   
  
 
instance (DV.Zipper vec,
          DV.Storage vec c,
          DV.Storage vec b,
          DV.Storage vec a) => 
         Zipper T1 vec a b c where  
  zipWith f (T1 vec) (T1 vec1) = T1 $ neZipWith f vec vec1   
 
instance (DV.Zipper vec,
          DV.Storage vec c,
          DV.Storage vec b,
          DV.Storage vec a) => 
         Zipper T2 vec a b c where  
  zipWith f (T2 (NonEmpty.Cons x vec)) (T2 (NonEmpty.Cons x1 vec1)) = (T2 (NonEmpty.cons (f x x1) (neZipWith f vec vec1)))
  
  
class LookupUnsafe (ne :: (* -> *) -> * -> *) vec a where  
  lookupUnsafe :: ne vec a -> Int -> a 

instance (DV.LookupUnsafe vec a) => LookupUnsafe T0 vec a where
  lookupUnsafe (T0 vec) idx = DV.lookupUnsafe vec idx

instance (DV.LookupUnsafe vec a) => LookupUnsafe T1 vec a where
  lookupUnsafe (T1 (NonEmpty.Cons x vec)) idx = if idx == 0 then x else DV.lookupUnsafe vec (idx-1)

instance (DV.LookupUnsafe vec a) => LookupUnsafe T2 vec a where
  lookupUnsafe (T2 (NonEmpty.Cons x (NonEmpty.Cons x1 vec))) idx = case idx of 
    0 -> x 
    1 -> x1      
    _ -> DV.lookupUnsafe vec (idx-2)


class FromList (ne :: (* -> *) -> * -> *) vec a where
  fromList :: Caller -> [a] -> ne vec a
  
instance (DV.Storage vec a, DV.FromList vec) => FromList T0 vec a where   
  fromList _ xs = T0 $ DV.fromList xs 
  
instance (DV.Storage vec a, DV.FromList vec) => FromList T1 vec a where   
  fromList caller [] = merror caller modul "fromList" "List needs at least one Element" 
  fromList _ (x:xs) = T1 $ NonEmpty.Cons x $ DV.fromList xs   
  
instance (DV.Storage vec a, DV.FromList vec) => FromList T2 vec a where   
  fromList caller xs | length xs <2 =  merror caller modul "fromList" "List needs at least two Elements"
  fromList _ (x:(x1:xs)) = T2 $ NonEmpty.Cons x (NonEmpty.Cons x1 $ DV.fromList xs)


class Singleton (ne :: (* -> *) -> * -> *) vec a where
  head :: (ne :: (* -> *) -> * -> *) vec a -> a
  last :: (ne :: (* -> *) -> * -> *) vec a -> a
  minmax :: (ne :: (* -> *) -> * -> *) vec a -> (a,a)
  all :: (a -> Bool) -> ne vec a -> Bool
  any :: (a -> Bool) -> ne vec a -> Bool
  
instance (Ord a, DV.Storage vec a, DV.Singleton vec) => Singleton T0 vec a where  
  head (T0 xs) = DV.head xs 
  last (T0 xs) = DV.last xs
  minmax (T0 xs) = DV.minmax xs
  all f (T0 xs) = DV.all f xs
  any f (T0 xs) = DV.any f xs
  
instance (Ord a, DV.Storage vec a, DV.Singleton vec) => Singleton T1 vec a where  
  head (T1 (NonEmpty.Cons x _)) = x 
  last (T1 (NonEmpty.Cons _ xs)) = DV.last xs
  minmax (T1 (NonEmpty.Cons x xs)) = let (mi, ma) = DV.minmax xs in (min x mi, max x ma)
  all f (T1 (NonEmpty.Cons x xs)) = (f x) && DV.all f xs
  any f (T1 (NonEmpty.Cons x xs)) = (f x) || DV.any f xs
   
instance (Ord a, DV.Storage vec a, DV.Singleton vec) => Singleton T2 vec a where  
  head (T2 (NonEmpty.Cons x _)) = x 
  last (T2 (NonEmpty.Cons _ (NonEmpty.Cons _ xs))) = DV.last xs
  minmax (T2 (NonEmpty.Cons x (NonEmpty.Cons x1 xs))) = 
    let (mi, ma) = DV.minmax xs in (min (min x x1) mi, max (max x x1) ma)
  all f (T2 (NonEmpty.Cons x (NonEmpty.Cons x1 xs))) = f x && f x1 && DV.all f xs
  any f (T2 (NonEmpty.Cons x (NonEmpty.Cons x1 xs))) = f x || f x1 || DV.any f xs



class Reverse (ne :: (* -> *) -> * -> *) vec a where
  reverse :: ne vec a -> ne vec a
  
instance (DV.Reverse vec,DV.Storage vec a) => Reverse T0 vec a where  
  reverse (T0 xs) = T0 $ DV.reverse xs

instance (DV.Reverse vec,DV.Storage vec a, NonEmptyClass.Cons vec, DV.Singleton vec) => Reverse T1 vec a where  
  reverse (T1 ne) = let vec = NonEmpty.flatten ne in  T1 $ NonEmpty.Cons (DV.head vec) (DV.tail vec) 

instance (DV.Reverse vec,DV.Storage vec a, NonEmptyClass.Cons vec, DV.Singleton vec) => Reverse T2 vec a where  
  reverse (T2 ne) = 
    let vec = NonEmpty.flatten $ NonEmpty.flatten ne
        x = DV.head vec
        x1 = DV.head $ DV.tail vec
        xs = DV.tail $ DV.tail vec
    in T2 $ NonEmpty.Cons x (NonEmpty.Cons x1 xs)

class DeltaMap (ne :: (* -> *) -> * -> *) (ne1 :: (* -> *) -> * -> *) vec a b where
  deltaMap :: (a -> a -> b) -> ne vec a -> ne1 vec b
  
instance 
  (DV.Zipper vec,
   DV.Storage vec b,
   DV.Storage vec a,
   DV.Singleton vec) => 
  DeltaMap T0 T0 vec a b where  
  deltaMap f (T0 ne) = T0 $ DV.deltaMap f ne
  
instance 
  (DV.Zipper vec,NonEmptyClass.Cons vec,
   DV.Storage vec b,
   DV.Storage vec a,
   DV.Singleton vec) => 
  DeltaMap T1 T0 vec a b where  
  deltaMap f (T1 ne) = T0 $ DV.deltaMap f $ NonEmpty.flatten ne
   
                       
instance 
  (DV.Zipper vec,NonEmptyClass.Cons vec,
   DV.Storage vec b,
   DV.Storage vec a,
   DV.Singleton vec) => 
  DeltaMap T2 T1 vec a b where  
  deltaMap f (T2 ne) = 
    let xs = DV.deltaMap f $ NonEmpty.flatten $ NonEmpty.flatten ne 
    in T1 $ NonEmpty.Cons (DV.head xs) $ DV.tail xs
   
       
class Find (ne :: (* -> *) -> * -> *) vec a where
  findIndex :: (a -> Bool) -> ne vec a -> Maybe Int
  
instance (DV.Storage vec a, DV.Find vec) => Find T0 vec a where
  findIndex f (T0 vec) = DV.findIndex f vec
  
instance (DV.Storage vec a, DV.Find vec, NonEmptyClass.Cons vec) => Find T1 vec a where
  findIndex f (T1 ne) = DV.findIndex f $ NonEmpty.flatten ne
  
instance (DV.Storage vec a, DV.Find vec, NonEmptyClass.Cons vec) => Find T2 vec a where
  findIndex f (T2 ne) = DV.findIndex f  $ NonEmpty.flatten $ NonEmpty.flatten ne 
  
  
class Append  (ne :: (* -> *) -> * -> *) vec a where  
  append :: ne vec a -> ne vec a -> ne vec a
  
  
instance (DV.Storage vec a, DV.Singleton vec) => Append T0 vec a where  
  append (T0 vec) (T0 vec1) = T0 $ DV.append vec vec1
  
instance (DV.Storage vec a, DV.Singleton vec, NonEmptyClass.Cons vec) => Append T1 vec a where  
  append (T1 (NonEmpty.Cons x vec)) (T1 ne) = T1 $ NonEmpty.Cons x (DV.append vec $ NonEmpty.flatten ne)
  
instance (DV.Storage vec a, DV.Singleton vec, NonEmptyClass.Cons vec) => Append T2 vec a where  
  append (T2 (NonEmpty.Cons x (NonEmpty.Cons x1 vec))) (T2 ne) = T2 $ NonEmpty.Cons x (NonEmpty.Cons x1 (DV.append vec $ NonEmpty.flatten $  NonEmpty.flatten ne))

