{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where

import qualified EFA2.Signal.Data as D
import qualified EFA2.Signal.Vector as SV
import qualified EFA2.Signal.Base as B
import EFA2.Signal.Data (Data(Data), (:>), Nil, Zip, Apply, List, List2, NestedList, Vec2, UVec, UVec2, UVec2L, DVal)
import EFA2.Signal.Base (BSum(..), BProd(..), DArith0(..), Val, ZeroCrossing)
import EFA2.Signal.Typ
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.Monoid (Monoid, mempty, mappend)

import qualified Data.List as L
import Data.Function ((.), ($))
import Prelude
          (Show, Eq, Ord, Bool,
           Int, error, (++), Num, Fractional, (+), (-), (/), (*))
import qualified Prelude as P


----------------------------------------------------------
-- | Signal & Company

newtype TC s t d = TC d  deriving (Show, Eq, Ord)

data Scalar

data Signal
data Sample

data FSignal
data FSample

data FDistrib
data FClass

data TestRow

----------------------------------------------------------------
-- Signal Zipwith with Rule of Signal Inheritance
class Arith s1 s2 s3 | s1 s2 -> s3

instance Arith Scalar Scalar Scalar

instance Arith Scalar Signal Signal
instance Arith Scalar Sample Sample
instance Arith Scalar FSignal FSignal
instance Arith Scalar FSample FSample
instance Arith Scalar FDistrib FDistrib
instance Arith Scalar FClass FClass

instance Arith Signal Scalar Signal
instance Arith Sample Scalar Sample
instance Arith FSignal Scalar FSignal
instance Arith FSample Scalar FSample
instance Arith FDistrib Scalar FDistrib
instance Arith FClass Scalar FClass

instance Arith Signal Signal Signal
instance Arith Sample Sample Sample
instance Arith FSignal FSignal FSignal
instance Arith FSample FSample FSample
instance Arith FDistrib FDistrib FDistrib
instance Arith FClass FClass FClass

instance Arith TestRow TestRow TestRow
instance Arith Scalar TestRow TestRow
instance Arith TestRow Scalar TestRow

zipWith ::
   (Arith s1 s2 s3, D.ZipWith c1 c2 d1 d2 d3) =>
   (d1 -> d2 -> d3) ->
   TC s1 typ1 (Data c1 d1) ->
   TC s2 typ2 (Data c2 d2) ->
   TC s3 typ3 (Data (Zip c1 c2) d3)
zipWith f (TC da1) (TC da2) =
   TC $ D.zipWith f da1 da2

----------------------------------------------------------------
-- Getyptes ZipWith
tzipWith ::
   (Arith s1 s2 s3, D.ZipWith c1 c2 d1 d2 d3) =>
   (TC Sample typ1 (Data Nil d1) ->
    TC Sample typ2 (Data Nil d2) ->
    TC Sample typ3 (Data Nil d3)) ->
   TC s1 typ1 (Data c1 d1) ->
   TC s2 typ2 (Data c2 d2) ->
   TC s3 typ3 (Data (Zip c1 c2) d3)
tzipWith f xs ys = zipWith g xs ys
   where g x y = fromSample $ f (toSample x) (toSample y)


{-
----------------------------------------------------------------
-- Signal crosswith with Rule of Signal Inheritance
class CrossArith s1 s2 s3 | s1 s2 -> s3
instance CrossArith Signal Sample Signal
instance CrossArith Sample Signal Sample
instance CrossArith FSignal FSample FSignal
instance CrossArith FSample FSignal FSample
instance CrossArith FDistrib FClass FDistrib
instance CrossArith FClass FDistrib FClass

class (CrossArith s1 s2 s3, DCrossWith c1 c2 c3 d1 d2 d3) => CrossWith s1 s2 s3 c1 c2 c3 d1 d2 d3 | s1 s2 -> s3, c1 c2 -> c3  where
       crossWith ::  (d1 -> d2 -> d3) -> TC s1 typ1 (c1 d1) -> TC s2 typ2 (c2 d2) -> TC s3 typ3 (c3 d3)

instance (CrossArith s1 s2 s3, DCrossWith c1 c2 c3 d1 d2 d3) => CrossWith s1 s2 s3 c1 c2 c3 d1 d2 d3 where
       crossWith f (TC da1) (TC da2) = TC $ dcrossWith f da1 da2
-}

----------------------------------------------------------
-- Normal Arithmetics - based on zip

(.*) ::
   (Arith s1 s2 s3, TProd t1 t2 t3, D.ZipWith c1 c2 a1 a2 a1, BProd a1 a2) =>
   TC s1 t1 (Data c1 a1) ->
   TC s2 t2 (Data c2 a2) ->
   TC s3 t3 (Data (Zip c1 c2) a1)
(.*) x y = zipWith (..*) x y

(./) ::
   (Arith s1 s2 s3, TProd t1 t2 t3, D.ZipWith c1 c2 a1 a2 a1, BProd a1 a2) =>
   TC s1 t3 (Data c1 a1) ->
   TC s2 t2 (Data c2 a2) ->
   TC s3 t1 (Data (Zip c1 c2) a1)
(./) x y = zipWith (../) x y

(.+) ::
   (Arith s1 s2 s3, TSum t1 t2 t3, D.ZipWith c1 c2 a a a, BSum a) =>
   TC s1 t1 (Data c1 a) ->
   TC s2 t2 (Data c2 a) ->
   TC s3 t3 (Data (Zip c1 c2) a)
(.+) x y = zipWith (..+) x y

(.-) ::
   (Arith s1 s2 s3, TSum t1 t2 t3, D.ZipWith c1 c2 a a a, BSum a) =>
   TC s1 t3 (Data c1 a) ->
   TC s2 t2 (Data c2 a) ->
   TC s3 t1 (Data (Zip c1 c2) a)
(.-) x y = zipWith (..-) x y


infix 7 .*, ./
infix 6 .+,.-


{-
----------------------------------------------------------
-- Cross Arithmetics - based on crossWith

class CrossProd s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (&*) :: TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (&/) :: TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3

instance  (CrossWith s1 s2 s3 v1 v2 v3 d1 d2 d3, BProd d1 d2 d3, TProd t1 t2 t3) =>  CrossProd s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (&*) x y = crossWith (..*) x y
          (&/) x y = crossWith (../) x y

class CrossSum s1 s2 s3 t1 t2 t3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (&+) :: TC s1 t1 c1 -> TC s2 t2 c2 -> TC s3 t3 c3
  (&-) :: TC s1 t3 c1 -> TC s2 t2 c2 -> TC s3 t1 c3

instance  (CrossWith s1 s2 s3 v1 v2 v3 d1 d2 d3, BSum d1 d2 d3, TProd t1 t2 t3) =>  CrossSum s1 s2 s3 t1 t2 t3 (v1 d1) (v2 d2) (v3 d3) where
          (&+) x y = crossWith (..+) x y
          (&-) x y = crossWith (..-) x y


infix 7 &*, &/
infix 6 &+, &-
-}

----------------------------------------------------------
-- Convenience Type Synonyms

-- generic
type Scal typ a = TC Scalar typ (DVal a)

type Sc = Scal (Typ UT UT UT) Val

type Sig1 typ a = TC Signal typ (UVec a)
type FSig1 typ a = TC FSignal typ (UVec a)
type Sig1L typ a = TC Signal typ (List a)
type Sig2L typ a = TC Signal typ (List2 a)

type Sig2 typ a = TC Signal typ (Vec2 a)
type FSig2 typ a = TC FSignal typ (Vec2 a)

type Test typ a = TC TestRow typ (DVal a)
type Test1 typ a = TC TestRow typ (UVec a)
type Test2 typ a = TC TestRow typ (UVec2 a)
type Test1L typ a = TC TestRow typ (List a)
type Test2L typ a = TC TestRow typ (List2 a)


type Samp typ a = TC Sample typ (DVal a)
type Samp1L typ a = TC Sample typ (List a)

-- specific
--type UTignal a = Sig1 (Typ UT UT UT) a
--type FUTSignal = FSig1 (Typ UT UT UT) Val

-- type UTSignal a = Sig1 (Typ UT UT UT) a
type PSig = Sig1 (Typ A P Tt) Val
type PSig2 = Sig2 (Typ A P Tt) Val
type PSigL = Sig1L (Typ A P Tt) Val
type PSig2L = Sig2L (Typ A P Tt) Val

type ESig = Sig1 (Typ A E Tt) Val
type ESig2 = Sig2 (Typ A E Tt) Val
type ESigL = Sig1L (Typ A E Tt) Val
type ESig2L = Sig2L (Typ A E Tt) Val

type NSig = Sig1 (Typ A N Tt) Val
type NSig2 = Sig2 (Typ A N Tt) Val
type NSigL = Sig1L (Typ A N Tt) Val
type NSig2L = Sig2L (Typ A N Tt) Val

type TSig = Sig1 (Typ A T Tt) Val
type TSigL = Sig1L (Typ A T Tt) Val
type FSig = FSig1 (Typ A F Tt) Val
type DTSig = FSig1 (Typ D T Tt) Val
type UTSig = Sig1 (Typ UT UT UT) Val

type UTFSig = FSig1 (Typ UT UT UT) Val
type UTSigL = Sig1L (Typ UT UT UT) Val

type PVal = Scal (Typ A P Tt) Val
type TVal = Scal (Typ A T Tt) Val
type FVal = Scal (Typ A F Tt) Val
type DTVal = Scal (Typ D T Tt) Val

type SignalIdx = Int

type PSamp2 = TC Sample (Typ A P Tt) (UVec2 Val)
type PSamp1 =  TC Sample (Typ A P Tt) (UVec Val)
type PSamp1L =  TC Sample (Typ A P Tt) (List Val)
type PSamp2L = TC Sample (Typ A P Tt) (UVec2L Val)
type PSamp2LL = TC Sample (Typ A P Tt) (List2 Val)
type PSamp = TC Sample (Typ A P Tt) (DVal Val)

-- Flow Signal Samples
type ESamp2 = TC FSample (Typ A E Tt) (UVec2 Val)
type ESamp1 =  TC FSample (Typ A E Tt) (UVec Val)
type ESamp1L =  TC FSample (Typ A E Tt) (List Val)
type ESamp2L = TC FSample (Typ A E Tt) (UVec2L Val)
type ESamp2LL = TC FSample (Typ A E Tt) (List2 Val)
type ESamp = TC FSample (Typ A E Tt) (DVal Val)

type FSamp2 = TC FSample (Typ A F Tt) (UVec2 Val)
type FSamp1 =  TC FSample (Typ A F Tt) (UVec Val)
type FSamp1L =  TC FSample (Typ A F Tt) (List Val)
type FSamp2L = TC FSample (Typ A F Tt) (UVec2L Val)
type FSamp2LL = TC FSample (Typ A F Tt) (List2 Val)
type FSamp = TC FSample (Typ A F Tt) (DVal Val)

type PFSamp2 = TC FSample (Typ A P Tt) (UVec2 Val)
type PFSamp1 =  TC FSample (Typ A P Tt) (UVec Val)
type PFSamp1L =  TC FSample (Typ A P Tt) (List Val)
type PFSamp2L = TC FSample (Typ A P Tt) (UVec2L Val)
type PFSamp2LL = TC FSample (Typ A P Tt) (List2 Val)
type PFSamp = TC FSample (Typ A P Tt) (DVal Val)

-- Time Sample
type DTSampleL = TC Sample (Typ D T Tt) (List Val)

-- type PSample =  TC Scalar (Typ A P Tt) (DVal Val)
type DTSamp =  TC Sample (Typ D T Tt) (DVal Val)
type TSamp =  TC Sample (Typ A T Tt) (DVal Val)
type TSamp1 =  TC Sample (Typ A T Tt) (UVec Val)
type TSamp1L =  TC Sample (Typ A T Tt) (List Val)
type TZeroSamp = TC Sample (Typ A T Tt) (Data Nil ZeroCrossing)
type TZeroSamp1L = TC Sample (Typ A T Tt) (Data ([] :> Nil) ZeroCrossing)

------------------------------------

type RSig = (TSigL, PSamp2LL)
type RSamp1 = (TSamp, PSamp1L)
type RSamp = (TSamp, PSamp)


rhead :: RSig -> RSamp1
rhead (t,ps) = (head t, head ps)

rtail :: RSig -> RSig
rtail (t,ps) = (tail t, tail ps)

rlast :: RSig -> RSamp1
rlast (t,ps) = (last t, last ps)

rinit :: RSig -> RSig
rinit (t,ps) = (init t, init ps)

rsingleton :: RSamp1 -> RSig
rsingleton (t,ps) = (singleton t, singleton ps)



-- xappend :: RSig -> RSig -> RSig
-- xappend (t1,ps1) (t2,ps2) = (t1.++t2, ps1.++ps2)

-- xconcat xs = L.foldl' (xappend) []

-- instance Monoid RSig where
--   mempty = (mempty,mempty)
--   mappend (t1,ps1) (t2,ps2) = (t1 .++ t2, ps1 .++ ps2)

----------------------------------------------------------
-- from/to List

unpack :: TC s t (Data c d) -> (Data c d)
unpack (TC x) = x

fromList :: D.FromList c d => NestedList c d -> TC s t (Data c d)
fromList x = TC $ D.fromList x

toList :: D.FromList c d => TC s t (Data c d) -> NestedList c d
toList (TC x) = D.toList x

{-
fromList :: SV.FromList c d => [d] -> TC s t (Data (c :> Nil) d)
fromList x = TC $ D.fromList x

toList :: SV.FromList c d => TC s t (Data (c :> Nil) d) -> [d]
toList (TC x) = D.toList x
-}

-- subsumed by general fromList
fromList2 ::
   (SV.FromList c1 (c2 d), SV.FromList c2 d) =>
   [[d]] -> TC s t (Data (c1 :> c2 :> Nil) d)
fromList2 x = TC $ D.fromList x

-- subsumed by general toList
toList2 ::
   (SV.FromList c1 (c2 d), SV.FromList c2 d) =>
   TC s t (Data (c1 :> c2 :> Nil) d) -> [[d]]
toList2 (TC x) = D.toList x

fromVal :: SV.FromList c d => Int -> d -> TC s t (Data (c :> Nil) d)
fromVal len x = fromList (L.replicate len x)

fromScalar :: TC Scalar typ (Data Nil d) -> d
fromScalar (TC (Data x)) = x

toScalar :: d -> TC Scalar typ (Data Nil d)
toScalar x = TC $ Data x

toSample :: d -> TC Sample typ (Data Nil d)
toSample x = TC $ Data x

fromSample :: TC Sample typ (Data Nil d) -> d
fromSample (TC (Data x)) = x


class Const s c d where
      toConst :: Int -> d -> TC s (Typ UT UT UT) (Data c d)

instance (SV.FromList v1 Val) => Const Signal (v1 :> Nil) Val where
         toConst len x =  fromVal len x

instance (SV.FromList v1 Val) => Const FSignal (v1 :> Nil) Val where
         toConst len x =  fromVal len x

instance Const Scalar Nil Val where
         toConst _len x =  toScalar x

{-
getSigVec :: TC Signal typ (Data (v1 :> Nil) d) -> v1 d
getSigVec (TC (Data (D1 x))) = x

vec2Sig :: v1 d -> TC Signal typ (Data (v1 :> Nil) d)
vec2Sig x = (TC (Data (D1 x)))

{-
fromSigList :: [TC Signal typ (Data (v1 :> Nil) d)] -> TC Signal typ (Data (V.Vector :> v1 :> Nil) d)
fromSigList slist = TC $ Data $ D2 $ V.fromList vecList
            where vecList = map getSigVec slist

toSigList :: TC Signal typ (Data (V.Vector :> v1 :> Nil) d) -> [TC Signal typ (Data (v1 :> Nil) d)]
toSigList (TC (Data (D2 x))) = map vec2Sig vecList
            where vecList = V.toList x
-}
-}
----------------------------------------------------------
-- Zip

zip ::
   (Arith s s s, c ~ Zip c c, D.ZipWith c c d1 d2 (d1, d2)) =>
   TC s typ (Data c d1) ->  TC s typ (Data c d2) ->  TC s typ (Data c (d1,d2))
zip x y = zipWith (,) x y

----------------------------------------------------------
-- SMap
map :: D.Map c d1 d2 => (d1 -> d2) -> TC s typ (Data c d1) -> TC s typ (Data c d2)
map f (TC x) = TC $ D.map f x

----------------------------------------------------------
-- Getyptes SMap
tmap ::
   (D.Map c d1 d2) =>
   (TC Sample typ1 (Data Nil d1) -> TC Sample typ2 (Data Nil d2)) ->
   TC s typ1 (Data c d1) -> TC s typ2 (Data c d2)
tmap f xs = changeType $ map (fromSample . f . toSample) xs

----------------------------------------------------------
-- DeltaMap

deltaMap, deltaMapReverse ::
   (SV.Singleton v2 (Apply v1 d1),
    v1 ~ Zip v1 v1, D.ZipWith (v2 :> v1) (v2 :> v1) d1 d1 d2) =>
   (d1 -> d1 -> d2) ->
   TC Signal typ (Data (v2 :> v1) d1) ->
   TC FSignal typ (Data (v2 :> v1) d2)
deltaMap f x = changeSignalType $ zipWith f  x (tail x)
deltaMapReverse f x = changeSignalType $ zipWith f (tail x) x

{-
----------------------------------------------------------
-- Getyptes DeltaMap

class TDeltaMap s1 s2 c d1 d2 where
      tdeltaMap :: (TC Scalar typ1 (Data Nil d1) -> TC Scalar typ1 (Data Nil d1) -> TC Scalar typ2 (Data Nil d2)) -> TC s1 typ1 (c d1) -> TC s2 typ2 (c d2)
      tdeltaMapReverse :: (TC Scalar typ1 (Data Nil d1) -> TC Scalar typ1 (Data Nil d1) -> TC Scalar typ2 (Data Nil d2)) -> TC s1 typ1 (c d1) -> TC s2 typ2 (c d2)

instance (SDeltaMap s1 s2 c d1 d2) => TDeltaMap s1 s2 c d1 d2 where
      tdeltaMap f xs = changeType $ deltaMap g xs where g x y = fromScalar $ f (toScalar x) (toScalar y)
      tdeltaMapReverse f xs = changeType $ deltaMapReverse g xs where g x y = fromScalar $ f (toScalar x) (toScalar y)


----------------------------------------------------------
-- Doppeltes Getyptes DeltaMap -- deltaMap ueber powerRecord
{-
class  TDeltaMap2 s1 s2 s3 c1 c2 c3 d1 d2 d3 where
      tdeltaMap2 :: (TC Scalar typ1 (Data Nil (d1,d1))-> TC Scalar typ2 (Data Nil (d2,d2)) -> TC Scalar typ3 (Data Nil d3))  ->  TC s1 typ1 (c1 d1) ->  TC s2 typ2 (c2 d2) ->  TC s3 typ3 (c3 d3)
      tdeltaMap2Reverse :: (TC Scalar typ1 (Data Nil (d1,d1))-> TC Scalar typ2 (Data Nil (d2,d2)) -> TC Scalar typ3 (Data Nil d3))  ->  TC s1 typ1 (c1 d1) ->  TC s2 typ2 (c2 d2) ->  TC s3 typ3 (c3 d3)

instance (Arith s1 s1 s1, D.ZipWith c1 c1 c1 (d1, d1) (d2, d2) d3, D.ZipWith c1 c1 c1 d1 d1 (d1, d1), D.ZipWith c1 c1 c1 d2 d2 (d2, d2), STail s1 c1 d1, STail s1 c1 d2) =>  TDeltaMap2 s1 s1 s1 c1 c1 c1 d1 d2 d3 where
  tdeltaMap2 f xs ys = changeSignalType $ (tzipWith f dxs dys)
    where dxs = zipWith (,) xs (tail xs)
          dys = zipWith (,) ys (tail ys)
  tdeltaMap2Reverse f xs ys = changeSignalType $ (tzipWith f dxs dys)
    where dxs = zipWith (,) (tail xs) xs
          dys = zipWith (,) (tail ys) ys


class  TDeltaMap2 s1 s2 s3 c1 c2 c3 d1 d2 d3 where
      tdeltaMap2 :: (TC Scalar typ1 (Data Nil (d1,d1))-> TC Scalar typ2 (Data Nil (d2,d2)) -> TC Scalar typ3 (Data Nil d3))  ->  TC s1 typ1 (c1 d1) ->  TC s2 typ2 (c2 d2) ->  (TC s3 typ3 (c3 d3),
      tdeltaMap2Reverse :: (TC Scalar typ1 (Data Nil (d1,d1))-> TC Scalar typ2 (Data Nil (d2,d2)) -> TC Scalar typ3 (Data Nil d3))  ->  TC s1 typ1 (c1 d1) ->  TC s2 typ2 (c2 d2) ->  TC s3 typ3 (c3 d3)

instance (Arith s1 s1 s1, D.ZipWith c1 c1 c1 (d1, d1) (d2, d2) d3, D.ZipWith c1 c1 c1 d1 d1 (d1, d1), D.ZipWith c1 c1 c1 d2 d2 (d2, d2), STail s1 c1 d1, STail s1 c1 d2) =>  TDeltaMap2 s1 s1 s1 c1 c1 c1 d1 d2 d3 where
  tdeltaMap2 f xs ys = changeSignalType $ (tzipWith f dxs dys)
    where dxs = zipWith (,) xs (tail xs)
          dys = zipWith (,) ys (tail ys)
  tdeltaMap2Reverse f xs ys = changeSignalType $ (tzipWith f dxs dys)
    where dxs = zipWith (,) (tail xs) xs
          dys = zipWith (,) (tail ys) ys
-}
-}
---------------------------------------------------------
-- sFold

foldl :: (FoldType s, D.Fold c d1 d2) => (d1 -> d2 -> d1) -> d1 -> TC s typ (Data c d2) -> d1
foldr :: (FoldType s, D.Fold c d1 d2) => (d2 -> d1 -> d1) -> d1 -> TC s typ (Data c d2) -> d1
foldl f x (TC y) = D.foldl f x y
foldr f x (TC y) = D.foldr f x y

class FoldType s where
instance FoldType Signal where
instance FoldType FSignal where
instance FoldType FSample where
instance FoldType FDistrib where
instance FoldType FClass where

{-
  ----------------------------------------------------------
-- sTFold
class TFold s1 s2 c1 c2 d1 d2 where
  tfoldl :: (TC s1 typ1 (c1 d1) -> TC s2 typ2 (c2 d2) -> TC s1 typ1 (c1 d1)) ->  TC s1 typ1 (c1 d1) -> TC s2 typ2 (c2 d2) -> TC s1 typ1 (c1 d1)

instance (SFold Scalar (Data Nil) d1 d2) => TFold Scalar s2 (Data Nil) c2 d1 d2 where
   tfoldl f a x = toScalar $ foldl g (fromScalar a) x where g a x = fromScalar $ f (toScalar a) (unpack x)
-}

----------------------------------------------------------
-- SHead & STail

head, last ::
   (HeadType s1 s2, SV.Singleton v1 (Apply v2 d)) =>
   TC s1 typ (Data (v1 :> v2) d) -> TC s2 typ (Data v2 d)
head (TC x) = TC $ D.head x
last (TC x) = TC $ D.last x

class HeadType s1 s2 | s1 -> s2
instance HeadType Signal Sample
instance HeadType FSignal FSample
instance HeadType Sample Sample
instance HeadType TestRow TestRow


init, tail ::
   (TailType s, SV.Singleton v2 (Apply v1 d)) =>
   TC s typ (Data (v2 :> v1) d) -> TC s typ (Data (v2 :> v1) d)
init (TC x) = TC $ D.init x
tail (TC x) = TC $ D.tail x

class TailType s where
instance TailType Signal where
instance TailType FSignal where
instance TailType Sample where


----------------------------------------------------------
-- STranspose

transpose1 ::
   (TransposeType s1 s2) =>
   TC s1 typ (Data (v1 :> Nil) d) ->
   TC s2 typ (Data (v1 :> Nil) d)
transpose1 (TC x) = TC $ D.transpose1 x

transpose2 ::
   (TransposeType s1 s2, SV.Transpose v1 v2 d) =>
   TC s1 typ (Data (v2 :> v1 :> Nil) d) ->
   TC s2 typ (Data (v2 :> v1 :> Nil) d)
transpose2 (TC x) = TC $ D.transpose2 x

class TransposeType s1 s2 | s1 -> s2, s2 -> s1
instance TransposeType Signal Sample
instance TransposeType Sample Signal
instance TransposeType FSignal FSample
instance TransposeType FSample FSignal

instance TransposeType TestRow TestRow

----------------------------------------------------------
-- Monoid

instance Monoid c => Monoid (TC s typ c) where
   mempty = TC $ mempty
   mappend (TC x) (TC y) = TC $ mappend x y


append ::
   D.Append c1 c2 d =>
   TC s1 t (Data c1 d) -> TC s2 t (Data c2 d) ->
   TC s3 t (Data (Zip c1 c2) d)
append (TC x) (TC y) = TC $ D.append x y

class AppendType s1 s2 s3 | s1 s2 -> s3
instance AppendType Signal Signal Signal
instance AppendType Signal Sample Signal
instance AppendType Sample Signal Signal


class (SV.Singleton v (Apply c d)) => Singleton s1 s2 v c d | s2 -> s1 where
   singleton :: TC s1 t (Data c d) -> TC s2 t (Data (v :> c) d)
   singleton (TC x) = TC $ D.singleton x

instance (SV.Singleton v1 d) => Singleton Sample Signal v1 Nil d where

instance (SV.Singleton v2 (v1 d)) => Singleton Sample Signal v2 (v1 :> Nil) d where

instance (SV.Singleton v2 (v1 d)) => Singleton Sample Sample v2 (v1 :> Nil) d where


-- (.++) :: SAppend s1 s2 s3 c1 c2 c3 d =>  (TC s1 typ (c1 d)) -> (TC s2 typ (c2 d)) -> (TC s3 typ (c3 d))
-- (.++) x y = append x y

(.++) :: Monoid a => a -> a -> a
(.++) x y = mappend x y

infix 5 .++


----------------------------------------------------------
-- All & Co

class All s c d where
  all :: (d -> Bool) -> TC s typ (Data c d) -> Bool
  any :: (d -> Bool) -> TC s typ (Data c d) -> Bool

instance D.All c d => All s c d where
  all f (TC x) = D.all f x
  any f (TC x) = D.any f x


----------------------------------------------------------
-- signal sign

sigSign ::
   (Ord d1, Num d1, Box s c c d1, D.Map c d1 B.Sign) =>
   TC s typ (Data c d1) -> TC s typ (Data c B.Sign)
sigSign x = map B.sign $ box x

----------------------------------------------------------
-- box / unbox a signal

class Box s1 c1 c2 d1 where
   box :: TC s1 typ (Data c1 d1) -> TC s1 typ (Data c2 d1)
   unbox :: TC s1 typ (Data c2 d1) -> TC s1 typ (Data c1 d1)

instance (UV.Unbox d1) => Box Signal (UV.Vector :> Nil) (V.Vector :> Nil) d1 where
   box (TC (Data x)) = TC $ Data $ SV.box x
   unbox (TC (Data x)) = TC $ Data $ SV.unbox x

instance (UV.Unbox d1) => Box Scalar c1 c1 d1 where
   box x = x
   unbox x = x

-- | Convert between List and different Vector formats
convert :: (D.Convert c1 c2 d) => TC s typ (Data c1 d) -> TC s typ (Data c2 d)
convert (TC x) = TC $ D.convert x


----------------------------------------------------------
-- sum all signal value

class (BSum d1, Num d1) => SigSum s1 s2 c1 c2 d1 | s1 -> s2, c1 -> c2  where
   sigSum :: TC s1 typ (Data c1 d1) -> TC s2 typ (Data c2 d1)

instance (BSum d1, Num d1, SV.Walker v1 d1 d1) => SigSum Signal Scalar (v1 :> Nil) Nil d1 where
   sigSum x = TC $ Data $ foldl (..+) 0 x

instance (BSum d1, Num d1, SV.Walker v1 d1 d1) => SigSum FSignal Scalar (v1 :> Nil) Nil d1 where
   sigSum x = TC $ Data $ foldl (..+) 0 x

----------------------------------------------------------
-- Delta and 2Point Average of Signal

deltaSig ::
    (z ~ Apply v1 Val, SV.Zipper v2 z z z, SV.Singleton v2 z,
     v1 ~ Zip v1 v1, D.ZipWith v1 v1 Val Val Val, DSucc delta1 delta2) =>
    TC Signal (Typ delta1 t1 p1) (Data (v2 :> v1) Val) ->
    TC FSignal (Typ delta2 t1 p1) (Data (v2 :> v1) Val)
deltaSig x = changeDelta $ deltaMapReverse (..-) x

avSig ::
    (z ~ Apply v1 Val, SV.Zipper v2 z z z, SV.Singleton v2 z,
     v1 ~ Zip v1 v1, D.ZipWith v1 v1 Val Val Val) =>
    TC Signal (Typ delta1 t1 p1) (Data (v2 :> v1) Val) ->
    TC FSignal (Typ delta1 t1 p1) (Data (v2 :> v1) Val)
avSig x = changeDelta $ deltaMapReverse (\ x1 x2 -> (x1..+x2)../ (2::Val)) x

sort ::
   (SV.Sort v1 d) =>
   TC s1 typ (Data (v1 :> Nil) d) ->
   TC s1 typ (Data (v1 :> Nil) d)
sort (TC x) = TC $ D.sort x


----------------------------------------------------------
-- Part & Full Integrate

-- DeltaSig Signal FSignal (Data (v1 :> Nil)) A D Val =>
-- | Partial Signal Integration
sigPartInt ::  TSig -> PSig -> FSig
sigPartInt time power = (deltaSig time) .* (avSig power)
-- czipWith (*) dTime $ D.map (\ p1 p2 -> (p1+p2)/2) power


-- | Partial Signal Integration
sigFullInt ::  TSig -> PSig -> FSig
sigFullInt time power = fromList [fromScalar $ sigSum $ sigPartInt time power]

-- csingleton (cfoldr (+) 0  $ czipWith (*) dTime $ D.map (\ p1 p2 -> (p1+p2)/2) power)

----------------------------------------------------------
-- | make untyped
untype ::  TC s1 (Typ delta1 t1 p1) (c1 d1) -> TC s1 (Typ UT UT UT) (c1 d1)
untype (TC x) = TC x

-- | make typed
setType ::  TC s1 (Typ UT UT UT) (c1 d1) -> TC s1 (Typ delta1 t1 p1) (c1 d1)
setType (TC x) = TC x

-- |
setTypeTestRow ::  TC sig ty val -> TC TestRow ty val
setTypeTestRow (TC x) = TC x

-- | change the Type
changeType :: TC s typ1 (Data c d) -> TC s typ2 (Data c d)
changeType (TC x) = TC x

-- | change the Type
changeDelta :: TC s (Typ delta1 t p) (Data c d) -> TC s (Typ delta2 t p) (Data c d)
changeDelta (TC x) = TC x

-- | change the signal type
changeSignalType :: TC s1 typ (Data c d) ->  TC s2 typ (Data c d)
changeSignalType (TC x) = TC x

neg :: (DArith0 d, D.Map c d d) => TC s typ (Data c d) -> TC s typ (Data c d)
neg = map B.neg

rec :: (DArith0 d, D.Map c d d) => TC s typ (Data c d) -> TC s typ (Data c d)
rec = map B.rec


-- | data ConversiSon function
fromSigList ::
   SV.FromList v2 (Apply v1 d) =>
   [TC s typ (Data v1 d)] -> TC s typ (Data (v2 :> v1) d)
fromSigList xs =
   TC $ Data $ SV.fromList $ L.map (\(TC (Data x)) -> x) xs

-- | data Conversion function
toSigList ::
   SV.FromList v2 (Apply v1 d) =>
   TC s typ (Data (v2 :> v1) d) -> [TC s typ (Data v1 d)]
toSigList (TC (Data xs)) =
   SV.map (TC . Data) $ SV.toList xs


fromCells ::
   (SV.FromList v1 d, SV.FromList v2 (v1 d)) =>
   [[TC s typ (Data Nil d)]] -> TC s typ (Data (v2 :> v1 :> Nil) d)
fromCells xss =
   fromList2 $ L.map (L.map (\(TC (Data x)) -> x)) xss

toCells ::
   (SV.FromList v2 (v1 d), SV.FromList v1 d, SV.FromList v2 [d],
    SV.Walker v2 (v1 d) [d]) =>
   TC s typ (Data (v2 :> v1 :> Nil) d) -> [[TC s typ (Data Nil d)]]
toCells xss = L.map (L.map (TC . Data)) $ toList2 xss

filter ::  (SV.Filter v1 d) => (d -> Bool) -> TC s typ (Data (v1 :> Nil) d) -> TC s typ (Data (v1 :> Nil) d)
filter f (TC x) = TC $ D.filter f x


sampleAverage :: Fractional d => TC Sample typ (Data Nil d) -> TC Sample typ (Data Nil d) -> TC Sample typ (Data Nil d)
sampleAverage (TC (Data x)) (TC (Data y)) = TC $ Data $ (x+y)/2


sign ::
   (D.Map c d B.Sign, Ord d, Num d) =>
   TC s typ (Data c d) -> TC s (Typ A SZ UT) (Data c B.Sign)
sign x = changeType $ map B.sign x


untuple :: TC Sample typ (Data Nil (d,d)) -> (TC Sample typ (Data Nil d), TC Sample typ (Data Nil d))
untuple (TC (Data (x,y))) = (TC $ Data x, TC $ Data y)

maximum, minimum ::
   (D.Maximum c d) =>
   TC s typ (Data c d) -> TC Scalar typ (Data Nil d)
maximum (TC x) = TC $ Data $ D.maximum x
minimum (TC x) = TC $ Data $ D.minimum x


subSignal1D :: (SV.Lookup v d)=> TC s typ (Data (v :> Nil) d) -> [SignalIdx] -> TC s typ (Data (v :> Nil) d)
subSignal1D (TC (Data x)) idxs = TC $ Data $ SV.lookUp x idxs

length :: (SV.Length (D.Apply c d)) => TC s typ (Data c d) -> Int
length (TC x) = D.length x


makeDelta ::  TC s (Typ A p t) (Data c d) -> TC s (Typ D p t) (Data c d)
makeDelta (TC x) = TC x

makeAbsolute ::  TC s (Typ D p t) (Data c d) -> TC s (Typ A p t) (Data c d)
makeAbsolute (TC x) = TC x

reverse :: (D.Reverse c d) => TC s t (Data c d) ->  TC s t (Data c d)
reverse (TC x) = TC $ D.reverse x
