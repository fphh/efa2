{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
module EFA.Utility.FixedLength (
   C, Position, List, switch,
   Wrap(Wrap, unwrap),
   WrapPos(WrapPos, unwrapPos),
   Zero, Succ(Stop, Succ),
   map, zipWith, sequenceA, repeat,
   index, update, indices, numFromPos,
   i0, i1, i2, i3, i4,
   ) where

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty

import qualified Control.Applicative as App
import qualified Data.Traversable as Trav
import Control.Applicative (Applicative, liftA2)
import Data.Traversable (Traversable, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Word (Word)

import Prelude hiding (map, zipWith, repeat)


class (list ~ List (Position list)) => C list where
   type Position list :: *
   type List position :: * -> *
   switch ::
      f Empty.T ->
      (forall list0. C list0 => f (NonEmpty.T list0)) ->
      f list

instance C Empty.T where
   type Position Empty.T = Zero
   type List Zero = Empty.T
   switch x _ = x

instance C list => C (NonEmpty.T list) where
   type Position (NonEmpty.T list) = Succ (Position list)
   type List (Succ position) = NonEmpty.T (List position)
   switch _ x = x


newtype Wrap list a = Wrap {unwrap :: list a}


newtype Map a b list = Map {runMap :: list a -> list b}

map :: C list => (a -> b) -> list a -> list b
map f =
   runMap $
   switch
      (Map $ \Empty.Cons -> Empty.Cons)
      (Map $ \(NonEmpty.Cons x xs) -> NonEmpty.Cons (f x) $ map f xs)


newtype Sequence f a list = Sequence {runSequence :: list (f a) -> f (list a)}

sequenceA :: (Applicative f, C list) => list (f a) -> f (list a)
sequenceA =
   runSequence $
   switch
      (Sequence $ \Empty.Cons -> App.pure Empty.Cons)
      (Sequence $ \(NonEmpty.Cons x xs) -> liftA2 NonEmpty.Cons x $ sequenceA xs)


newtype Repeat a list = Repeat {runRepeat :: list a}

repeat :: C list => a -> list a
repeat a =
   runRepeat $
   switch
      (Repeat $ Empty.Cons)
      (Repeat $ NonEmpty.Cons a $ repeat a)


newtype Zip a b c list = Zip {runZip :: list a -> list b -> list c}

zipWith :: C list => (a -> b -> c) -> list a -> list b -> list c
zipWith f =
   runZip $
   switch
      (Zip $ \Empty.Cons Empty.Cons -> Empty.Cons)
      (Zip $ \(NonEmpty.Cons a as) (NonEmpty.Cons b bs) ->
         NonEmpty.Cons (f a b) $ zipWith f as bs)


instance C list => Functor (Wrap list) where
   fmap f = Wrap . map f . unwrap

instance C list => Foldable (Wrap list) where
   foldMap = foldMapDefault

instance C list => Traversable (Wrap list) where
   sequenceA = fmap Wrap . sequenceA . unwrap

instance C list => Applicative (Wrap list) where
   pure = Wrap . repeat
   Wrap f <*> Wrap x = Wrap $ zipWith ($) f x


data Zero
data Succ pos = Stop | Succ pos deriving (Eq, Ord, Show)

instance Eq Zero where _==_ = True
instance Ord Zero where compare _ _ = EQ


newtype Update a list = Update {runUpdate :: Position list -> list a -> list a}

updatePos :: C list => (a -> a) -> Position list -> list a -> list a
updatePos f =
   runUpdate $
   switch
      (Update $ \ _ Empty.Cons -> Empty.Cons)
      (Update $ \pos0 (NonEmpty.Cons x xs) ->
          case pos0 of
             Stop -> NonEmpty.Cons (f x) xs
             Succ pos1 -> NonEmpty.Cons x $ updatePos f pos1 xs)

update :: C list => (a -> a) -> WrapPos list -> list a -> list a
update f (WrapPos k) = updatePos f k


newtype Index a list = Index {runIndex :: Position list -> list a -> a}

indexPos :: C list => Position list -> list a -> a
indexPos =
   runIndex $
   switch
      (Index $ \ _ {- Zero -} Empty.Cons -> error "impossible index")
      (Index $ \pos0 (NonEmpty.Cons x xs) ->
          case pos0 of
             Stop -> x
             Succ pos1 -> indexPos pos1 xs)

index :: C list => WrapPos list -> list a -> a
index (WrapPos k) = indexPos k

newtype Indices list = Indices {runIndices :: list (Position list)}

indicesPos :: C list => list (Position list)
indicesPos =
   runIndices $
   switch
      (Indices $ Empty.Cons)
      (Indices $ NonEmpty.Cons Stop $ map Succ indicesPos)

indices :: C list => list (WrapPos list)
indices = map WrapPos indicesPos


newtype WrapPos list = WrapPos {unwrapPos :: Position list}

newtype NumFromPos list = NumFromPos {runNumFromPos :: WrapPos list -> Word}

numFromPos :: C list => WrapPos list -> Word
numFromPos =
   runNumFromPos $
   switch
      (NumFromPos $ \(WrapPos _) -> error "numFromPos")
      (NumFromPos $ \(WrapPos n) ->
         case n of
            Stop -> 0
            Succ m -> 1 + numFromPos (WrapPos m))

newtype Compare a list =
           Compare {runCompare :: WrapPos list -> WrapPos list -> a}

instance (C list) => Eq (WrapPos list) where
   (==) =
      runCompare $
      switch
         (Compare $ \_ _ -> error "equalPos")
         (Compare $ \(WrapPos i) (WrapPos j) ->
             case (i,j) of
                (Succ k, Succ l) -> WrapPos k == WrapPos l
                (Stop, Stop) -> True
                _ -> False)

instance (C list) => Ord (WrapPos list) where
   compare =
      runCompare $
      switch
         (Compare $ \_ _ -> error "equalPos")
         (Compare $ \(WrapPos i) (WrapPos j) ->
             case (i,j) of
                (Succ k, Succ l) -> compare (WrapPos k) (WrapPos l)
                (Stop, Stop) -> EQ
                (Stop, Succ _) -> LT
                (Succ _, Stop) -> GT)


i0 :: WrapPos (NonEmpty.T list)
i0 = WrapPos Stop

i1 :: WrapPos (NonEmpty.T (NonEmpty.T list))
i1 = WrapPos $ Succ Stop

i2 :: WrapPos (NonEmpty.T (NonEmpty.T (NonEmpty.T list)))
i2 = WrapPos $ Succ $ Succ Stop

i3 :: WrapPos (NonEmpty.T (NonEmpty.T (NonEmpty.T (NonEmpty.T list))))
i3 = WrapPos $ Succ $ Succ $ Succ Stop

i4 :: WrapPos (NonEmpty.T (NonEmpty.T (NonEmpty.T (NonEmpty.T (NonEmpty.T list)))))
i4 = WrapPos $ Succ $ Succ $ Succ $ Succ Stop
