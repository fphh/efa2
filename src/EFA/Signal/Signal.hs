{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}


module EFA.Signal.Signal where

import qualified EFA.Signal.Data as D
import qualified EFA.Signal.Vector as SV
import qualified EFA.Signal.Base as B
import EFA.Signal.Data (Data(Data), (:>), Nil, Zip, Apply, List, List2, NestedList, Vec2, UVec, UVec2, UVec2L)
import EFA.Signal.Base (ZeroCrossing)
import EFA.Signal.Typ

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic (Sum, (~+), (~-), Product, (~*), (~/), Constant)

import EFA.Report.Report (Table(..), TableData(..), ROpt(RAll), toDoc, autoFormat)
import EFA.Report.Base
          (UnitScale(..), DisplayFormat(..),
           DispStorage(..), DispStorage1(..),
           getUnitScale, dispLength)
import EFA.Report.Typ
          (TDisp, DisplayType, getDisplayFormat, getDisplayUnit)
import EFA.Report.Format (Format)

import qualified EFA.Report.Format as Format
import qualified EFA.Report.FormatValue as FV
import qualified EFA.Report.Base as ReportBase
import qualified EFA.Report.Report as Report
import qualified EFA.Report.Typ as Typ

import qualified Data.Map as Map ; import Data.Map (Map)
import qualified Data.Set as Set ; import Data.Set (Set)
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT

import Data.Monoid (Monoid, mempty, mappend, mconcat)
import Data.Tuple.HT (mapFst, mapPair)
import Data.Ord (comparing)
import Data.Zip (transposeClip)
import Control.Applicative (liftA2)

import Text.Printf (PrintfArg, printf)

import qualified Data.List as List
import Data.Function (id, (.), ($))
import Prelude
          (Show, Read, Eq, Ord, Maybe, Bool, error, fmap,
           Enum, toEnum, fromEnum,
           String, (++),
           Int, Double, (-))
import qualified Prelude as P


----------------------------------------------------------
-- | Signal & Company

newtype TC s t d = TC d  deriving (Show, Read, Eq, Ord)

data Scalar

data Signal
data Sample

data FSignal
data FSample

data FDistrib
data FClass

data TestRow


data SignalSample
data FSignalSample
data FDistribSample
data TestRowSample


----------------------------------------------------------------
-- | Signal Types and their Samples

type family SigSample s
type instance SigSample Signal = SignalSample
type instance SigSample FSignal = FSignalSample
type instance SigSample FDistrib = FDistribSample
type instance SigSample TestRow = TestRowSample

type family UnSample s
type instance UnSample SignalSample =  Signal
type instance UnSample FSignalSample = FSignal
type instance UnSample FDistribSample = FDistrib
type instance UnSample TestRowSample = TestRow


{-
class GetSample signal sample where
  signal -> sample

instance GetSample Signal Sample where
-}





newtype SignalIdx = SignalIdx Int deriving (Show, Eq, Ord)

instance Enum SignalIdx where
   fromEnum = unSignalIdx
   toEnum = SignalIdx

unSignalIdx :: SignalIdx -> Int
unSignalIdx (SignalIdx x) = x


data Range = Range SignalIdx SignalIdx
   deriving (Eq, Show)

rangeSingleton :: SignalIdx -> Range
rangeSingleton n = Range n n

rangeIsSingleton :: Range -> Bool
rangeIsSingleton (Range from to) = from P.== to


typ :: TC s t d -> t
typ _ = error "Signal.typ: got phantom type"

app :: TC s t d -> s
app _ = error "Signal.app: got phantom type"

readNested ::
   ((SV.Storage v2 (Apply v1 a), D.Storage v1 a) => TC s t (Data (v2 :> v1) a) -> b) ->
   (D.Storage (v2 :> v1) a => TC s t (Data (v2 :> v1) a) -> b)
readNested f x@(TC y) =
   case D.constraints y of D.ComposeConstraints -> f x

readNested2 ::
   ((SV.Storage v3 (Apply (v2 :> v1) a), SV.Storage v2 (Apply v1 a), D.Storage v1 a) => TC s t (Data (v3 :> v2 :> v1) a) -> b) ->
   (D.Storage (v3 :> v2 :> v1) a => TC s t (Data (v3 :> v2 :> v1) a) -> b)
readNested2 f x@(TC y) =
   case D.constraints y of
      D.ComposeConstraints ->
         case D.constraints (D.subData y (P.error "Signal.subData")) of
            D.ComposeConstraints -> f x

writeNested ::
   ((SV.Storage v2 (Apply v1 a), D.Storage v1 a) => TC s t (Data (v2 :> v1) a)) ->
   (D.Storage (v2 :> v1) a => TC s t (Data (v2 :> v1) a))
writeNested x =
   let z@(TC y) = case D.constraints y of D.ComposeConstraints -> x
   in  z


type instance D.Value (TC s t d) = D.Value d


----------------------------------------------------------------
-- Signal Zipwith with Rule of Signal Inheritance
-- type family
type family Arith s1 s2 :: *

type instance Arith Scalar Scalar = Scalar

type instance Arith Scalar Signal = Signal
type instance Arith Scalar Sample = Sample
type instance Arith Scalar FSignal = FSignal
type instance Arith Scalar FSample = FSample
type instance Arith Scalar FDistrib = FDistrib
type instance Arith Scalar FClass = FClass

type instance Arith Signal Scalar = Signal
type instance Arith Sample Scalar = Sample
type instance Arith FSignal Scalar = FSignal
type instance Arith FSample Scalar = FSample
type instance Arith FDistrib Scalar = FDistrib
type instance Arith FClass Scalar = FClass

type instance Arith Signal Signal = Signal
type instance Arith Sample Sample = Sample
type instance Arith FSignal FSignal = FSignal
type instance Arith FSample FSample = FSample
type instance Arith FDistrib FDistrib = FDistrib
type instance Arith FClass FClass = FClass

type instance Arith TestRow TestRow = TestRow
type instance Arith Scalar TestRow = TestRow
type instance Arith TestRow Scalar = TestRow


zipWith ::
   (D.ZipWith c, D.Storage c d1, D.Storage c d2, D.Storage c d3) =>
   (d1 -> d2 -> d3) ->
   TC s1 typ1 (Data c d1) ->
   TC s2 typ2 (Data c d2) ->
   TC (Arith s1 s2) typ3 (Data c d3)
zipWith f (TC da1) (TC da2) =
   TC $ D.zipWith f da1 da2

{-
zipWithGeneric ::
   (D.ZipWith c, D.Storage c d1, D.Storage c d2, D.Storage c d3) =>
   (d1 -> d2 -> d3) ->
   TC s1 typ1 (Data c d1) ->
   TC s2 typ2 (Data c d2) ->
   TC s3 typ3 (Data c d3)
zipWithGeneric f (TC da1) (TC da2) =
   TC $ D.zipWith f da1 da2
-}


----------------------------------------------------------------
-- Getyptes ZipWith
tzipWith ::
   (D.ZipWith c, D.Storage c d1, D.Storage c d2, D.Storage c d3) =>
   (TC Sample typ1 (Data Nil d1) ->
    TC Sample typ2 (Data Nil d2) ->
    TC Sample typ3 (Data Nil d3)) ->
   TC s1 typ1 (Data c d1) ->
   TC s2 typ2 (Data c d2) ->
   TC (Arith s1 s2) typ3 (Data c d3)
tzipWith f xs ys = zipWith g xs ys
   where g x y = fromSample $ f (toSample x) (toSample y)
{-
tzipWithGeneric ::
   (D.ZipWith c, D.Storage c d1, D.Storage c d2, D.Storage c d3) =>
   (TC s1 typ1 (Data Nil d1) ->
    TC s2 typ2 (Data Nil d2) ->
    TC s3 typ3 (Data Nil d3)) ->
   TC s1 typ1 (Data c d1) ->
   TC s2 typ2 (Data c d2) ->
   TC s3 typ3 (Data c d3)
tzipWithGeneric f xs ys = zipWithGeneric g xs ys
   where g x y = fromSampleGeneric $ f (toSampleGeneric x) (toSampleGeneric y)
-}

tzipWithSimple ::  (D.ZipWith c, D.Storage c d) =>
   (TC Sample typ (Data Nil d) ->
    TC Sample typ (Data Nil d) ->
    TC Sample typ (Data Nil d)) ->
    TC Signal typ (Data c d) ->
    TC Signal typ (Data c d) ->
    TC Signal typ (Data c d)
tzipWithSimple f xs ys = zipWith g xs ys
   where g x y = fromSample $ f (toSample x) (toSample y)
----------------------------------------------------------------
-- Signal crosswith with Rule of Signal Inheritance
type family CrossArith s1 s2
type instance CrossArith Signal Sample = Signal
type instance CrossArith Sample Signal = Sample
type instance CrossArith FSignal FSample = FSignal
type instance CrossArith FSample FSignal = FSample
type instance CrossArith FDistrib FClass = FDistrib
type instance CrossArith FClass FDistrib = FClass

crossWith ::
   (D.CrossWith c1 c2,
    D.Storage c1 d1, D.Storage c2 d2, D.Storage (D.Cross c1 c2) d3) =>
   (d1 -> d2 -> d3) ->
   TC s1 typ1 (Data c1 d1) ->
   TC s2 typ2 (Data c2 d2) ->
   TC (CrossArith s1 s2) typ3 (Data (D.Cross c1 c2) d3)
crossWith f (TC da1) (TC da2) = TC $ D.crossWith f da1 da2


----------------------------------------------------------
-- Normal Arithmetics - based on zip

(.*) ::
   (TProd t1 t2 t3, D.ZipWith c, D.Storage c a, Product a) =>
   TC s1 t1 (Data c a) ->
   TC s2 t2 (Data c a) ->
   TC (Arith s1 s2) t3 (Data c a)
(.*) x y = zipWith (~*) x y

(./) ::
   (TProd t1 t2 t3, D.ZipWith c, D.Storage c a, Product a) =>
   TC s1 t3 (Data c a) ->
   TC s2 t2 (Data c a) ->
   TC (Arith s1 s2) t1 (Data c a)
(./) x y = zipWith (~/) x y

(.+) ::
   (TSum t1 t2 t3, D.ZipWith c, D.Storage c a, Sum a) =>
   TC s1 t1 (Data c a) ->
   TC s2 t2 (Data c a) ->
   TC (Arith s1 s2) t3 (Data c a)
(.+) x y = zipWith (~+) x y

(.-) ::
   (TSum t1 t2 t3, D.ZipWith c, D.Storage c a, Sum a) =>
   TC s1 t3 (Data c a) ->
   TC s2 t2 (Data c a) ->
   TC (Arith s1 s2) t1 (Data c a)
(.-) x y = zipWith (~-) x y


infixl 7 .*, ./
infixl 6 .+,.-


----------------------------------------------------------
-- Cross Arithmetics - based on crossWith

(&*) ::
   (TProd t1 t2 t3, D.CrossWith c1 c2,
    D.Storage c1 a, D.Storage c2 a, D.Storage (D.Cross c1 c2) a, Product a) =>
   TC s1 t1 (Data c1 a) ->
   TC s2 t2 (Data c2 a) ->
   TC (CrossArith s1 s2) t3 (Data (D.Cross c1 c2) a)
(&*) = crossWith (~*)

(&/) ::
   (TProd t1 t2 t3, D.CrossWith c1 c2,
    D.Storage c1 a, D.Storage c2 a, D.Storage (D.Cross c1 c2) a, Product a) =>
   TC s1 t3 (Data c1 a) ->
   TC s2 t2 (Data c2 a) ->
   TC (CrossArith s1 s2) t1 (Data (D.Cross c1 c2) a)
(&/) = crossWith (~/)


(&+) ::
   (TSum t1 t2 t3, D.CrossWith c1 c2,
    D.Storage c1 a, D.Storage c2 a, D.Storage (D.Cross c1 c2) a, Sum a) =>
   TC s1 t3 (Data c1 a) ->
   TC s2 t2 (Data c2 a) ->
   TC (CrossArith s1 s2) t1 (Data (D.Cross c1 c2) a)
(&+) = crossWith (~+)

(&-) ::
   (TSum t1 t2 t3, D.CrossWith c1 c2,
    D.Storage c1 a, D.Storage c2 a, D.Storage (D.Cross c1 c2) a, Sum a) =>
   TC s1 t3 (Data c1 a) ->
   TC s2 t2 (Data c2 a) ->
   TC (CrossArith s1 s2) t1 (Data (D.Cross c1 c2) a)
(&-) = crossWith (~-)


infix 7 &*, &/
infix 6 &+, &-

----------------------------------------------------------
-- New Synonyms           ]

-- Time Signals
type TSignal v a = TC Signal (Typ A T Tt) (Data (v :> Nil) a)
type PSignal v a = TC Signal (Typ A P Tt) (Data (v :> Nil) a)
type UTSignal v a = TC Signal (Typ UT UT UT) (Data (v :> Nil) a)
type UTSignal2 v2 v1 a = TC Signal (Typ UT UT UT) (Data (v2 :> v1 :> Nil) a)
type NSignal2 v2 v1 a = TC Signal (Typ A N Tt) (Data (v2 :> v1 :> Nil) a)
type PSignal2 v2 v1 a = TC Signal (Typ A P Tt) (Data (v2 :> v1 :> Nil) a)
type XSignal2 v2 v1 a = TC Signal (Typ A X Tt) (Data (v2 :> v1 :> Nil) a)

type NSignal v a = TC Signal (Typ A N Tt) (Data (v :> Nil) a)

type NTestRow v a = TC TestRow (Typ A N Tt) (Data (v :> Nil) a)
type PTestRow v a = TC TestRow (Typ A P Tt) (Data (v :> Nil) a)
type FTestRow v a = TC TestRow (Typ A F Tt) (Data (v :> Nil) a)
type UTTestRow v a = TC TestRow (Typ UT UT UT) (Data (v :> Nil) a)


-- Flow Signals
type FFSignal v a = TC FSignal (Typ A F Tt) (Data (v :> Nil) a)
type UTFSignal v a = TC FSignal (Typ UT UT UT) (Data (v :> Nil) a)
type PFSignal v a =  TC FSignal (Typ A P Tt) (Data (v :> Nil) a)
type NFSignal v a =  TC FSignal (Typ A N Tt) (Data (v :> Nil) a)
type DTFSignal v a =  TC FSignal (Typ D T Tt) (Data (v :> Nil) a)


-- Distributions
type UTDistr v a = TC FDistrib (Typ UT UT UT) (Data (v :> Nil) a)
type FDistr v a = TC FDistrib (Typ A F Tt) (Data (v :> Nil) a)
type PDistr v a = TC FDistrib (Typ A P Tt) (Data (v :> Nil) a)
type NDistr v a = TC FDistrib (Typ A N Tt) (Data (v :> Nil) a)

type UTTestRow2 v2 v1 a = TC TestRow (Typ UT UT UT)  (Data (v2 :> v1 :> Nil) a)
type NTestRow2 v2 v1 a = TC TestRow (Typ A N Tt)  (Data (v2 :> v1 :> Nil) a)
type FTestRow2 v2 v1 a = TC TestRow (Typ A F Tt)  (Data (v2 :> v1 :> Nil) a)
type PTestRow2 v2 v1 a = TC TestRow (Typ A P Tt)  (Data (v2 :> v1 :> Nil) a)

----------------------------------------------------------
-- Convenience Type Synonyms

-- generic Type Synonyms

type Scal typ a = TC Scalar typ (D.Scalar a)

type Sc = Scal (Typ UT UT UT) Double

type Sig1 typ a = TC Signal typ (UVec a)
type Sig2 typ a = TC Signal typ (Vec2 a)
type Sig1L typ a = TC Signal typ (List a)
type Sig2L typ a = TC Signal typ (List2 a)

type Samp typ a = TC Sample typ (D.Scalar a)
type Samp1L typ a = TC Sample typ (List a)

type FSig1 typ a = TC FSignal typ (UVec a)
type FSig2 typ a = TC FSignal typ (Vec2 a)
type FSig1L typ a = TC FSignal typ (List a)
type FSig2L typ a = TC FSignal typ (List2 a)

type Test typ a = TC TestRow typ (D.Scalar a)
type Test1 typ a = TC TestRow typ (UVec a)
type Test2 typ a = TC TestRow typ (UVec2 a)
type Test1L typ a = TC TestRow typ (List a)
type Test2L typ a = TC TestRow typ (List2 a)



-- specific Type Synonyms

-- #######################
-- Time Signals
-- time
type TSig = Sig1 (Typ A T Tt) Double
type TSigL = Sig1L (Typ A T Tt) Double

--power
type PSig = Sig1 (Typ A P Tt) Double
type PSig2 = Sig2 (Typ A P Tt) Double
type PSigL = Sig1L (Typ A P Tt) Double
type PSig2L = Sig2L (Typ A P Tt) Double


-- untyped
type UTSig = Sig1 (Typ UT UT UT) Double
type UTSigL = Sig1L (Typ UT UT UT) Double

-- #######################
-- Time Samples
-- Time Sample
type DTSampleL = TC Sample (Typ D T Tt) (List Double)
type DTSamp =  TC Sample (Typ D T Tt) (D.Scalar Double)
type TSamp =  TC Sample (Typ A T Tt) (D.Scalar Double)
type TSamp1 =  TC Sample (Typ A T Tt) (UVec Double)
type TSamp1L =  TC Sample (Typ A T Tt) (List Double)
type TZeroSamp = TC Sample (Typ A T Tt) (Data Nil ZeroCrossing)
type TZeroSamp1L = TC Sample (Typ A T Tt) (Data ([] :> Nil) ZeroCrossing)

type PSamp2 = TC Sample (Typ A P Tt) (UVec2 Double)
type PSamp1 =  TC Sample (Typ A P Tt) (UVec Double)
type PSamp1L =  TC Sample (Typ A P Tt) (List Double)
type PSamp2L = TC Sample (Typ A P Tt) (UVec2L Double)
type PSamp2LL = TC Sample (Typ A P Tt) (List2 Double)
type PSamp = TC Sample (Typ A P Tt) (D.Scalar Double)


-- #######################
-- Flow Signals

-- time
type DTFSig = FSig1 (Typ D T Tt) Double

-- energy Flow
type FFSig = FSig1 (Typ A F Tt) Double
type FFSig2 = FSig2 (Typ A F Tt) Double
type FFSigL = FSig1L (Typ A F Tt) Double
type FFSig2L = FSig2L (Typ A F Tt) Double

-- mean Power
type PFSig = FSig1 (Typ A P Tt) Double
type PFSig2 = FSig2 (Typ A P Tt) Double
type PFSigL = FSig1L (Typ A P Tt) Double
type PFSig2L = FSig2L (Typ A P Tt) Double

-- efficiency
type NFSig = FSig1 (Typ A N Tt) Double
type NFSig2 = FSig2 (Typ A N Tt) Double
type NFSigL = FSig1L (Typ A N Tt) Double
type NFSig2L = FSig2L (Typ A N Tt) Double

-- untyped
type UTFSig = FSig1 (Typ UT UT UT) Double

-- ######################
-- Flow Signal Samples
type ESamp2 = TC FSample (Typ A E Tt) (UVec2 Double)
type ESamp1 =  TC FSample (Typ A E Tt) (UVec Double)
type ESamp1L =  TC FSample (Typ A E Tt) (List Double)
type ESamp2L = TC FSample (Typ A E Tt) (UVec2L Double)
type ESamp2LL = TC FSample (Typ A E Tt) (List2 Double)
type ESamp = TC FSample (Typ A E Tt) (D.Scalar Double)

type FSamp2 = TC FSample (Typ A F Tt) (UVec2 Double)
type FSamp1 =  TC FSample (Typ A F Tt) (UVec Double)
type FSamp1L =  TC FSample (Typ A F Tt) (List Double)
type FSamp2L = TC FSample (Typ A F Tt) (UVec2L Double)
type FSamp2LL = TC FSample (Typ A F Tt) (List2 Double)
type FSamp = TC FSample (Typ A F Tt) (D.Scalar Double)

type PFSamp2 = TC FSample (Typ A P Tt) (UVec2 Double)
type PFSamp1 =  TC FSample (Typ A P Tt) (UVec Double)
type PFSamp1L =  TC FSample (Typ A P Tt) (List Double)
type PFSamp2L = TC FSample (Typ A P Tt) (UVec2L Double)
type PFSamp2LL = TC FSample (Typ A P Tt) (List2 Double)
type PFSamp = TC FSample (Typ A P Tt) (D.Scalar Double)


-- ########################
-- Scalars
type PVal = Scal (Typ A P Tt) Double
type TVal = Scal (Typ A T Tt) Double
type FVal = Scal (Typ A F Tt) Double
type DTVal = Scal (Typ D T Tt) Double


----------------------------------------------------------
-- from/to List

unpack :: TC s t a -> a
unpack (TC x) = x

fromList :: (D.FromList c, D.Storage c d) => NestedList c d -> TC s t (Data c d)
fromList x = TC $ D.fromList x

toList :: (D.FromList c, D.Storage c d) => TC s t (Data c d) -> NestedList c d
toList (TC x) = D.toList x

{-
fromList :: SV.FromList c d => [d] -> TC s t (Data (c :> Nil) d)
fromList x = TC $ D.fromList x

toList :: SV.FromList c d => TC s t (Data (c :> Nil) d) -> [d]
toList (TC x) = D.toList x
-}

-- subsumed by general fromList
fromList2 ::
   (SV.FromList c1, SV.Storage c1 (c2 d), SV.FromList c2, SV.Storage c2 d) =>
   [[d]] -> TC s t (Data (c1 :> c2 :> Nil) d)
fromList2 x = TC $ D.fromList x

-- subsumed by general toList
toList2 ::
   (SV.FromList c1, SV.Storage c1 (c2 d), SV.FromList c2, SV.Storage c2 d) =>
   TC s t (Data (c1 :> c2 :> Nil) d) -> [[d]]
toList2 (TC x) = D.toList x

fromVal :: (SV.FromList c, SV.Storage c d) => Int -> d -> TC s t (Data (c :> Nil) d)
fromVal n x = fromList (List.replicate n x)

fromScalar :: TC Scalar typ (Data Nil d) -> d
fromScalar (TC (Data x)) = x

toScalar :: d -> TC Scalar typ (Data Nil d)
toScalar x = TC $ Data x

toSample :: d -> TC Sample typ (Data Nil d)
toSample x = TC $ Data x

fromSample :: TC Sample typ (Data Nil d) -> d
fromSample (TC (Data x)) = x

{-
toSampleGeneric :: d -> TC (SigSample s) typ (Data Nil d)
toSampleGeneric x = TC $ Data x

fromSampleGeneric :: TC (SigSample s) typ (Data Nil d) -> d
fromSampleGeneric (TC (Data x)) = x

class FromToSample
  toSampleGeneric ::  TC (SigSample s) typ (Data Nil d) -> d
  fromSampleGeneric :: TC (SigSample s) typ (Data Nil d) -> d

-}


class ConstSignal s
instance ConstSignal Signal
instance ConstSignal FSignal

class Const s c where
   toConst :: D.Storage c d => Int -> d -> TC s (Typ UT UT UT) (Data c d)

instance (SV.FromList v1, ConstSignal s) => Const s (v1 :> Nil) where
   toConst n x = writeNested (fromVal n x)

instance Const Scalar Nil where
   toConst _len x = toScalar x


unconsData :: TC s typ (Data c d) -> Apply c d
unconsData (TC (Data x)) = x

consData :: Apply c d -> TC s typ (Data c d)
consData x = TC (Data x)

liftData ::
   (Apply c0 a0 -> Apply c1 a1) ->
   TC s typ (Data c0 a0) -> TC s typ (Data c1 a1)
liftData f = consData . f . unconsData


fromSigList ::
   (SV.Storage v (Apply c d), SV.FromList v) =>
   [TC s typ (Data c d)] -> TC s typ (Data (v :> c) d)
fromSigList =
   consData . SV.fromList . fmap unconsData

toSigList ::
   (SV.Storage v (Apply c d), SV.FromList v) =>
   TC s typ (Data (v :> c) d) -> [TC s typ (Data c d)]
toSigList =
   fmap consData . SV.toList . unconsData


----------------------------------------------------------
-- Zip

zip ::
   (D.ZipWith c, D.Storage c d1, D.Storage c d2, D.Storage c (d1, d2)) =>
   TC s typ (Data c d1) ->
   TC s typ (Data c d2) ->
   TC (Arith s s) typ (Data c (d1,d2))
zip x y = zipWith (,) x y

unzip ::
   (D.ZipWith c, D.Storage c d1, D.Storage c d2, D.Storage c (d1, d2)) =>
   TC s typ (Data c (d1,d2)) ->
   (TC s typ (Data c d1),TC s typ (Data c d2))
unzip x = (map P.fst x, map P.snd x)

---------------------------------------------------------
-- SMap
map ::
   (D.Map c, D.Storage c d1, D.Storage c d2) =>
   (d1 -> d2) -> TC s typ (Data c d1) -> TC s typ (Data c d2)
map f (TC x) = TC $ D.map f x


-- TODO map2 "soll generischer werden (auch fuer Funktionen (w1 d1 -> w2 d2)"
map2 ::
  (SV.Walker v, SV.Storage w d1,
    SV.Storage v (w d1), SV.Storage v d2) =>
  (w d1 -> d2) ->
  TC s typ (Data (v :> w :> Nil) d1) ->
  TC s typ (Data (v :> Nil) d2)
map2 f (TC x) = TC $ D.map2 f x

----------------------------------------------------------
-- Getyptes SMap
tmap ::
   (D.Map c, D.Storage c d1, D.Storage c d2) =>
   (TC Sample typ1 (Data Nil d1) -> TC Sample typ2 (Data Nil d2)) ->
   TC s typ1 (Data c d1) -> TC s typ2 (Data c d2)
tmap f xs = changeType $ map (fromSample . f . toSample) xs

----------------------------------------------------------
-- DeltaMap

deltaMap ::
   (SV.Singleton v2, D.ZipWith (v2 :> v1),
    SV.Storage v2 (Apply v1 d1), SV.Storage v2 (Apply v1 d2),
    D.Storage v1 d1, D.Storage v1 d2) =>
   (d1 -> d1 -> d2) ->
   TC Signal typ (Data (v2 :> v1) d1) ->
   TC FSignal typ (Data (v2 :> v1) d2)
deltaMap f (TC x) = TC $ D.deltaMap f x


---------------------------------------------------------
-- sFold

foldl ::
   (FoldType s, D.Fold c, D.Storage c d1, D.Storage c d2) =>
   (d1 -> d2 -> d1) -> d1 -> TC s typ (Data c d2) -> d1
foldr ::
   (FoldType s, D.Fold c, D.Storage c d1, D.Storage c d2) =>
   (d2 -> d1 -> d1) -> d1 -> TC s typ (Data c d2) -> d1
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

{-
----------------------------------------------------------
-- Head & Tail

{-# DEPRECATED head, tail "use viewL instead" #-}
{-# DEPRECATED last, init "use viewR instead" #-}

head, last ::
   (SV.Singleton v1, SV.Storage v1 (Apply v2 d)) =>
   TC s typ (Data (v1 :> v2) d) -> TC (Head s) typ (Data v2 d)
head (TC x) = TC $ D.head x
last (TC x) = TC $ D.last x
-}

type family Head s
type instance Head Signal = Sample
type instance Head FSignal = FSample
type instance Head Sample = Sample
type instance Head TestRow = TestRow

{-
init, tail ::
   (TailType s, SV.Singleton v1, SV.Storage v1 (Apply v2 d)) =>
   TC s typ (Data (v1 :> v2) d) -> TC s typ (Data (v1 :> v2) d)
init (TC x) = TC $ D.init x
tail (TC x) = TC $ D.tail x
--}

class TailType s where
instance TailType Signal where
instance TailType FSignal where
instance TailType Sample where

viewL ::
   (TailType s, SV.Singleton v1, SV.Storage v1 (Apply v2 d)) =>
   TC s typ (Data (v1 :> v2) d) ->
   Maybe (TC (Head s) typ (Data v2 d), TC s typ (Data (v1 :> v2) d))
viewL (TC x) =
   P.fmap (mapPair (TC, TC)) $ D.viewL x

viewR ::
   (TailType s, SV.Singleton v1, SV.Storage v1 (Apply v2 d)) =>
   TC s typ (Data (v1 :> v2) d) ->
   Maybe (TC s typ (Data (v1 :> v2) d), TC (Head s) typ (Data v2 d))
viewR (TC x) =
   P.fmap (mapPair (TC, TC)) $ D.viewR x

----------------------------------------------------------
-- STranspose

transpose1 ::
   (TransposeType s1 s2) =>
   TC s1 typ (Data (v1 :> Nil) d) ->
   TC s2 typ (Data (v1 :> Nil) d)
transpose1 (TC x) = TC $ D.transpose1 x

transpose2 ::
   (TransposeType s1 s2, SV.Transpose v1 v2, SV.Storage v1 d) =>
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
   mconcat = TC . mconcat . List.map unpack


append ::
   (D.Append c1 c2, D.Storage c1 d, D.Storage c2 d) =>
   TC s1 t (Data c1 d) -> TC s2 t (Data c2 d) ->
   TC (Append s1 s2) t (Data (Zip c1 c2) d)
append (TC x) (TC y) = TC $ D.append x y

type family Append s1 s2
type instance Append Signal Signal = Signal
type instance Append Signal Sample = Signal
type instance Append Sample Signal = Signal


type family SingletonSource s
type instance SingletonSource Signal = Sample
type instance SingletonSource Sample = Sample

class (SV.Singleton v) => Singleton s v (c :: * -> *) where
instance (SV.Singleton v1) => Singleton Signal v1 Nil where
instance (SV.Singleton v2) => Singleton Signal v2 (v1 :> Nil) where
instance (SV.Singleton v2) => Singleton Sample v2 (v1 :> Nil) where

singleton ::
   (Singleton s v c, SV.Storage v (Apply c d)) =>
   TC (SingletonSource s) t (Data c d) -> TC s t (Data (v :> c) d)
singleton (TC x) = TC $ D.singleton x


-- (.++) :: SAppend s1 s2 s3 c1 c2 c3 d =>  (TC s1 typ (c1 d)) -> (TC s2 typ (c2 d)) -> (TC s3 typ (c3 d))
-- (.++) x y = append x y

(.++) :: Monoid a => a -> a -> a
(.++) x y = mappend x y

infix 5 .++


----------------------------------------------------------
-- All & Co

class All s c where
  all, any :: D.Storage c d => (d -> Bool) -> TC s typ (Data c d) -> Bool

instance D.All c => All s c where
  all f (TC x) = D.all f x
  any f (TC x) = D.any f x


----------------------------------------------------------
-- signal sign

sign ::
   (Ord d, Constant d, D.Map c, D.Storage c d, D.Storage c B.Sign) =>
   TC s typ (Data c d) -> TC s typ (Data c B.Sign)
sign x = map B.sign x

-- | Convert between List and different Vector formats
convert ::
   (D.Convert c1 c2, D.Storage c1 d, D.Storage c2 d) =>
   TC s typ (Data c1 d) -> TC s typ (Data c2 d)
convert (TC x) = TC $ D.convert x


----------------------------------------------------------
-- sum all signal value

type family SumType s :: *
type instance SumType Signal = Scalar
type instance SumType FSignal = Scalar

sum ::
   (FoldType s, SV.Storage v d, SV.Walker v, Constant d) =>
   TC s typ (Data (v :> Nil) d) ->
   TC (SumType s) typ (Data Nil d)
sum = TC . Data . foldl (~+) Arith.zero


----------------------------------------------------------
-- Delta and 2Point Average of Signal

delta ::
    (z ~ Apply v1 a, SV.Zipper v2, SV.Walker v2, SV.Singleton v2, SV.Storage v2 z,
     D.ZipWith v1, D.Storage v1 a, Sum a,
     DSucc delta1 delta2) =>
    TC Signal (Typ delta1 t1 p1) (Data (v2 :> v1) a) ->
    TC FSignal (Typ delta2 t1 p1) (Data (v2 :> v1) a)
delta x = changeDelta $ deltaMap (P.flip (~-)) x

average ::
    (z ~ Apply v1 a, SV.Zipper v2, SV.Walker v2, SV.Singleton v2, SV.Storage v2 z,
     D.ZipWith v1, D.Storage v1 a, Constant a) =>
    TC Signal (Typ delta1 t1 p1) (Data (v2 :> v1) a) ->
    TC FSignal (Typ delta1 t1 p1) (Data (v2 :> v1) a)
average x =
   changeDelta $
   deltaMap (\ x1 x2 -> (x1~+x2) ~/ P.asTypeOf (Arith.fromInteger 2) x1) x

sort ::
   (SV.Sort v, SV.Storage v d, Ord d) =>
   TC s1 typ (Data (v :> Nil) d) ->
   TC s1 typ (Data (v :> Nil) d)
sort (TC x) = TC $ D.sort x


sortBy ::
   (SV.Storage v d, Ord d, SV.SortBy v) =>
   (d -> d -> P.Ordering) ->
   TC s1 typ (Data (v :> Nil) d) ->
   TC s1 typ (Data (v :> Nil) d)
sortBy f (TC x) = TC $ D.sortBy f x


sortTwo ::
   (Ord d1,
    SV.Storage v d1, SV.Storage v d2, SV.Storage v (d1, d2),
    SV.SortBy v, SV.Zipper v, SV.Walker v) =>
   (TC s t1 (Data (v :> Nil) d1), TC s t2 (Data (v :> Nil) d2)) ->
   (TC s t1 (Data (v :> Nil) d1), TC s t2 (Data (v :> Nil) d2))
sortTwo (TC x, TC y) =
   mapPair (TC, TC) $ D.unzip $ D.sortBy (comparing P.fst) $ D.zip x y



----------------------------------------------------------
-- Part & Full Integrate

-- DeltaSig Signal FSignal (Data (v1 :> Nil)) A D Double =>
-- | Partial Signal Integration
partIntegrate ::
               (Constant d1,
                SV.Zipper v1,
                SV.Walker v1,
                SV.Singleton v1,
                SV.Storage v1 d1) =>
               TC Signal (Typ A T Tt) (Data (v1 :> Nil) d1) ->
               TC Signal (Typ A P Tt) (Data (v1 :> Nil) d1) ->
               TC FSignal (Typ A F Tt) (Data (v1 :> Nil) d1)
partIntegrate time power  =  delta time  .*  average power
-- czipWith (*) dTime $ D.map (\ p1 p2 -> (p1+p2)/2) power


-- | Partial Signal Integration
fullIntegrate ::   (SV.FromList v1,
                 SV.Zipper v1,
                 SV.Walker v1,
                 SV.Storage v1 d1,
                 SV.Singleton v1,
                 Constant d1) =>
                TC Signal (Typ A T Tt) (Data (v1 :> Nil) d1) ->
                TC Signal (Typ A P Tt) (Data (v1 :> Nil) d1) ->
                TC Scalar (Typ A F Tt) (Data Nil d1)
fullIntegrate time power = sum $ partIntegrate time power

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

neg ::
   (Sum d, D.Map c, D.Storage c d) =>
   TC s typ (Data c d) -> TC s typ (Data c d)
neg = map Arith.negate

rec ::
   (Product d, D.Map c, D.Storage c d) =>
   TC s typ (Data c d) -> TC s typ (Data c d)
rec = map Arith.recip



{-
-- | data Conversion function
fromSigList ::
   (SV.Storage v2 (Apply v1 d), SV.FromList v2) =>
   [TC s typ (Data v1 d)] -> TC s typ (Data (v2 :> v1) d)
fromSigList xs =
   TC $ Data $ SV.fromList $ List.map (\(TC (Data x)) -> x) xs

-- | data Conversion function
toSigList ::
   (SV.Storage v2 (Apply v1 d), SV.FromList v2) =>
   TC s typ (Data (v2 :> v1) d) -> [TC s typ (Data v1 d)]
toSigList (TC (Data xs)) =
   SV.map (TC . Data) $ SV.toList xs
-}

fromCells ::
   (SV.FromList v1, SV.FromList v2, SV.Storage v2 (v1 d), SV.Storage v1 d) =>
   [[TC s typ (Data Nil d)]] -> TC s typ (Data (v2 :> v1 :> Nil) d)
fromCells xss =
   fromList2 $ List.map (List.map (\(TC (Data x)) -> x)) xss

toCells ::
   (SV.FromList v1, SV.FromList v2, SV.Storage v2 (v1 d), SV.Storage v1 d) =>
   TC s typ (Data (v2 :> v1 :> Nil) d) -> [[TC s typ (Data Nil d)]]
toCells xss = List.map (List.map (TC . Data)) $ toList2 xss

filter ::
   (SV.Filter v, SV.Storage v d) =>
   (d -> Bool) -> TC s typ (Data (v :> Nil) d) -> TC s typ (Data (v :> Nil) d)
filter f (TC x) = TC $ D.filter f x

mapMaybe ::
   (SV.Filter v, SV.Walker v, SV.Storage v a, SV.Storage v b) =>
   (a -> Maybe b) ->
   TC s typ (Data (v :> Nil) a) -> TC s typ (Data (v :> Nil) b)
mapMaybe f (TC (Data x)) = TC $ Data $ SV.mapMaybe f x


sampleAverage ::
   (Constant d) =>
   TC Sample typ (Data Nil d) ->
   TC Sample typ (Data Nil d) ->
   TC Sample typ (Data Nil d)
sampleAverage (TC (Data x)) (TC (Data y)) =
   TC $ Data $ (x~+y) ~/ Arith.fromInteger 2


abs ::
   (Sum d, Ord d, D.Storage c d, D.Map c) =>
   TC s t (Data c d) -> TC s t (Data c d)
abs x = map Arith.abs x


hasSignChange :: (SV.Storage v1 B.Sign,
                  SV.Walker v1,
                  SV.Storage v1 d,
                  SV.Singleton v1,
                  TailType s,
                  Ord d,
                  Constant d) =>
                 TC s typ (Data (v1 :> Nil) d) -> Bool
hasSignChange x = P.not $ all (P.== hss) $ sign x
  where (TC (Data hss)) = hs
        hs = case viewL x of
          P.Just (h,_) -> sign h
          P.Nothing -> error "Empty Signal in Signal.consistentSign"


{-
sign x = changeType $ map f x
         where f x = if x > 10^(-12) then 1
                                     else if x < -10^(-12) then -1
                                                           else 0

sign x = changeType $ map f x
  where f x = if B.abs x > 10^(-12) then B.sign x else B.sign 0
-}

untuple ::
   TC Sample typ (Data Nil (d,d)) ->
   (TC Sample typ (Data Nil d), TC Sample typ (Data Nil d))
untuple (TC (Data (x,y))) = (TC $ Data x, TC $ Data y)

maximum, minimum ::
   (D.Maximum c, D.Storage c d, Ord d) =>
   TC s typ (Data c d) -> TC Scalar typ (Data Nil d)
maximum (TC x) = TC $ Data $ D.maximum x
minimum (TC x) = TC $ Data $ D.minimum x

minmax ::
   (D.Maximum c, D.Storage c d, Ord d) =>
   TC s typ (Data c d) -> TC Scalar typ (Data Nil (d, d))
minmax (TC x) = TC $ Data $ D.minmax x

-- | fit Signal range from 0 to 1 // ~*
norm ::  (D.Map c,
          Product d,
          Ord d,
          D.Storage c d,
          D.Maximum c) =>
         TC s typ (Data c d) -> TC s typ (Data c d)
norm x =  if max P./= min
          then  map (\y -> (y ~- min) ~/ (max ~- min)) x
               else map (\ _ -> min) x

          where (TC (Data max)) = maximum x
                (TC (Data min)) = minimum x


equalBy ::
   (SV.Walker v, SV.Storage v a, SV.Storage v b) =>
   (a -> b -> Bool) ->
   TC s t (Data (v :> Nil) a) ->
   TC s t (Data (v :> Nil) b) ->
   Bool
equalBy f (TC x) (TC y) = D.equalBy f x y


subSignal1D ::
   (SV.Lookup v, SV.Storage v d, Eq d) =>
   TC s typ (Data (v :> Nil) d) -> [SignalIdx] -> TC s typ (Data (v :> Nil) d)
subSignal1D (TC (Data x)) idxs = TC $ Data $ SV.lookUp x $ P.map unSignalIdx idxs

getSample2D ::
  (SV.FromList v1, SV.FromList v2, SV.Walker v2,
  SV.Storage v2 (v1 d), Eq d, SV.Storage v1 d,
  SV.Lookup v1, Eq (v1 d), SV.Lookup v2) =>
  TC s typ (Data (v2 :> v1 :> Nil) d) -> SignalIdx -> SignalIdx -> d
getSample2D (TC (Data x)) (SignalIdx idx) (SignalIdx idy) =
  P.head $ SV.toList $ P.flip SV.lookUp [idy] $ P.head $ SV.toList $ SV.lookUp x [idx]

{-
subSignal2D ::
   TC s typ (Data (v2 :> v1 :> Nil) d) -> ([SignalIdx], [SignalIdx]) -> TC s typ (Data (v2 :> v1 :> Nil) d)
subSignal2D (TC (Data x)) (idxs,idys) = TC $ Data $ SV.map (P.flip SV.lookUp idys) $ SV.lookUp x idxs
-}

getColumn :: (SV.Storage v2 (v1 d),
              SV.Singleton v2,
              Eq (v1 d),
              SV.Lookup v2) =>
   TC s typ (Data (v2 :> v1 :> Nil) d) -> SignalIdx -> TC s typ (Data (v1 :> Nil) d)
getColumn (TC (Data x)) idx = TC $ Data $
                                  P.fst $
                                  P.maybe (error "Error in Signal/subSignal2D1D empty list") id $
                                  SV.viewL $
                                  SV.lookUp x $ [unSignalIdx idx]


len :: (SV.Len (D.Apply c d)) => TC s typ (Data c d) -> Int
len (TC x) = D.len x


makeDelta ::  TC s (Typ A p t) (Data c d) -> TC s (Typ D p t) (Data c d)
makeDelta (TC x) = TC x

makeAbsolute ::  TC s (Typ D p t) (Data c d) -> TC s (Typ A p t) (Data c d)
makeAbsolute (TC x) = TC x

reverse :: (D.Reverse c, D.Storage c d) => TC s t (Data c d) ->  TC s t (Data c d)
reverse (TC x) = TC $ D.reverse x



----------------------------------------------------------
-- Report instances

getDisplayType :: (TDisp t) => TC s t d  -> Typ.DisplayType
getDisplayType = Typ.getDisplayType . typ

tdisp :: (TDisp t) => TC s t d  -> String
tdisp = Typ.tdisp . typ

udisp :: (TDisp t) => TC s t d  -> String
udisp = Typ.udisp . typ

-- | Display single values
vdisp ::
   (TDisp t, Constant a, PrintfArg a) =>
   TC s t (Data Nil a)  -> String
vdisp x@(TC (Data val)) = printf f $ Arith.fromRational s ~* val
  where t = getDisplayType x
        u = getDisplayUnit t
        (UnitScale s) = getUnitScale u
        (DisplayFormat f) = Typ.getDisplayFormat dispLength t u

-- | Display single values
srdisp ::
   (TDisp t, SV.FromList v, SV.Storage v a, Constant a, PrintfArg a) =>
   TC s t (Data (v :> Nil) a)  -> [String]
srdisp xs = fmap g $ toList xs -- (f l)
  where g x = printf f (Arith.fromRational s ~* x)
        t = getDisplayType xs
        u = getDisplayUnit t
        (UnitScale s) = getUnitScale u
        (DisplayFormat f) = Typ.getDisplayFormat dispLength t u


class DispApp s where
   dispApp :: s -> String

instance DispApp Scalar where
   dispApp _ = "Scal"

instance DispApp Signal where
   dispApp _ = "Sig"

instance DispApp FSignal where
   dispApp _ = "FSig"

instance DispApp TestRow where
   dispApp _ = "Test"


sigDisp ::
   (DispApp s, DispStorage c) =>
   TC s t (Data c d) -> String
sigDisp =
   let aux ::
          (DispApp s, DispStorage c) =>
          s -> TC s t (Data c d) -> String
       aux s (TC x) = dispApp s ++ dispStorage x
   in  aux (error "sigDisp s")


instance
   (DispApp s, TDisp t, ToTable c, D.Storage c a,
    Ord a, Constant a, PrintfArg a) =>
       Report.ToTable (TC s t (Data c a)) where
   toTable os (ti,x) = [toTable os (ti,x)]


class DispStorage c => ToTable c where
   toTable ::
      (DispApp s, TDisp t, D.Storage c a,
       Ord a, Constant a, PrintfArg a) =>
      Report.ROpts -> (String, TC s t (Data c a)) -> Table

instance
   (DispStorage1 v, SV.FromList v, SV.Singleton v) =>
       ToTable (v :> Nil) where
   toTable os (ti,x) =
      Table {
         tableTitle = "",
         tableFormat = autoFormat td,
         tableData = td,
         tableSubTitle = ""
      }
     where td = TableData {
                   tableBody = [fmap (toDoc id ) (f x) ],
                   titleRow = [],
                   titleCols = [fmap (toDoc id) [ti, sigDisp x, tdisp x]],
                   endCols = [[toDoc id $ udisp x]]
                }

           f y =
              if List.elem RAll os
                then readNested srdisp y
                else [vdisp (minimum x) ++ " - " ++ vdisp (maximum y)]

instance
   (DispStorage1 v1, StorageCollection v1 ~ v2,
    SV.FromList v1, SV.FromList v2) =>
       ToTable (v2 :> v1 :> Nil) where
   toTable _os (ti,xss) =
      Table {
         tableTitle = ti ++ "   " ++ sigDisp xss ++ tdisp xss ++ udisp xss,
         tableFormat = autoFormat td,
         tableData = td,
         tableSubTitle = ""
      }
     where td = TableData {
                   tableBody =
                      readNested2 (fmap (fmap (toDoc id . f)) . toCells) xss,
                   titleRow = [],
                   titleCols = [],
                   endCols = []
                }

           f x = vdisp x
--           f x | otherwise = [(vdisp min) ++ " - " ++ (vdisp max)]


-- | display a single value
dispSingle ::  ReportBase.Disp a => a -> DisplayType -> String
dispSingle x t = ReportBase.disp f s x ++ " " ++ P.show u
  where u = getDisplayUnit t
        s = getUnitScale u
        f = getDisplayFormat dispLength t u


-- | display a single value
dispRange :: ReportBase.Disp a => a -> a -> DisplayType -> String
dispRange x y t =
   ReportBase.disp f s x ++ " - " ++ ReportBase.disp f s y ++ " " ++ P.show u
  where u = getDisplayUnit t
        s = getUnitScale u
        f = getDisplayFormat dispLength t u

dispAll :: ReportBase.Disp a => [a] -> DisplayType -> String
dispAll xs t = (P.unwords $ fmap (ReportBase.disp f s) xs) ++  " " ++ P.show u
  where u = getDisplayUnit t
        s = getUnitScale u
        f = getDisplayFormat dispLength t u

dispAll2 :: ReportBase.Disp a => [[a]] -> DisplayType -> String
dispAll2 = error "to be implemented"


class SDisplay v where
  disp ::
     (DispApp s, TDisp t, ReportBase.Disp d, Ord d, D.Storage v d) =>
     TC s t (Data v d) -> String

{-
instance (TDisp t, ReportBase.Disp d, SV.Singleton v1 d, D.FromList (Data Nil) d)
         => SDisplay (TC Scalar t (Data Nil d)) where
  sdisp x@(TC (Data v))  = "Sig-D0 " -- ++ tdisp x ++ ": " ++ dispAll (toList x) dtyp -- dispRange dmin dmax dtyp
--    where dtyp = getDisplayType x
--          dmin = SV.minimum v
--          dmax = SV.maximum v

-}
instance SDisplay Nil where
  disp x@(TC (Data v))  = "Sig-D0 " ++ dispSingle v dtyp --  ++ ": " ++ dispAll (toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = getDisplayType x


instance (SV.Singleton v1, SV.FromList v1) => SDisplay (v1 :> Nil) where
   disp x@(TC v) = dispApp (app x) ++ "-D1 " ++ tdisp x ++ ": " ++ dispAll (toList x) dtyp -- dispRange dmin dmax dtyp
      where dtyp = getDisplayType x
            _dmin = D.minimum v
            _dmax = D.maximum v

instance
   (SV.Singleton v1, SV.Singleton v2, SV.Walker v2,
    SV.FromList v1, SV.FromList v2) =>
      SDisplay (v2 :> v1 :> Nil) where
  disp x@(TC v)  = "Sig-D2 " ++ tdisp x ++ ": " ++ dispAll2 (toList x) dtyp -- dispRange dmin dmax dtyp
    where dtyp = getDisplayType x
          _dmin = D.minimum v
          _dmax = D.maximum v


class FormatValue a where
   formatValue ::
      (DispApp s, TDisp t, Format output) =>
      TC s t a -> output

instance
   (Ord a, D.Storage v a, ReportBase.Disp a, SDisplay v) =>
      FormatValue (Data v a) where
   formatValue = Format.literal . disp

instance
   (TDisp t, DispApp s, FormatValue a) =>
      FV.FormatValue (TC s t a) where
   formatValue = formatValue

{-# DEPRECATED findIndex "don't use that for implementing an argmax" #-}
findIndex ::
  (SV.Storage v1 d1, SV.Find v1) =>
  (d1 -> Bool) -> TC s1 t1 (Data (v1 :> Nil) d1) -> Maybe SignalIdx
findIndex f (TC xs) = fmap SignalIdx $ D.findIndex f xs


findIndices ::(SV.Walker v1,
               SV.Storage v1 SignalIdx,
               SV.Storage v1 Int,
               SV.Storage v1 d1,
               SV.Find v1,
               Ord d1,
               SV.Unique v1 d1) =>
  (d1 -> Bool) -> TC s1 t1 (Data (v1 :> Nil) d1) -> TC s1 (Typ UT UT UT) (Data (v1 :> Nil) SignalIdx)
findIndices f (TC xs) = map (SignalIdx) $ TC $ D.findIndices f xs

unique ::  (Ord d1, SV.Unique v1 d1) => TC s1 t1 (Data (v1 :> Nil) d1) ->  TC s1 t1 (Data (v1 :> Nil) d1)
unique (TC x) = TC $ D.unique x


interp1Lin :: (Show d1,
               Product d1,
               Ord d1,
               SV.Storage v1 d1,
               SV.Find v1,
               SV.Singleton v1,
               SV.Lookup v1,
               Show (v1 d1)) =>
              String ->
              TC Signal t1 (Data (v1 :> Nil) d1) ->
              TC Signal t2 (Data (v1 :> Nil) d1) ->
              TC Sample t1 (Data Nil d1) ->
              TC Sample t2 (Data Nil d1)
interp1Lin caller xSig ySig (TC (Data xVal)) =
  toSample $ ((y2 ~- y1) ~/ (x2 ~- x1)) ~* (xVal ~- x1) ~+ y1
  where sIdx@(SignalIdx idx) =
          P.maybe (error msg) id $ findIndex (P.> xVal) xSig
        -- prevent negativ index when interpolating on first element
        TC (Data x1) = getSample xSig $ SignalIdx $ 
          if idx P.== 0 then error msg else idx-1
        TC (Data x2) = getSample xSig sIdx
        -- prevent negativ index when interpolating on first element
        TC (Data y1) = getSample ySig $ SignalIdx $
          if idx P.== 0 then error msg else idx-1
        TC (Data y2) = getSample ySig sIdx
        msg = "interp1Lin - Out of Range: " ++ caller ++ ": "++ P.show xVal
          ++ "\nxsig = " ++ P.show xSig
          ++ "\nysig = " ++ P.show ySig




getSample ::  (SV.Singleton v1,
               Eq d1,
               SV.Storage v1 d1,
               SV.Lookup v1) =>
              TC Signal t1 (Data (v1 :> Nil) d1) ->
              SignalIdx ->
              TC Sample t1 (Data Nil d1)
getSample x =
  P.fst
  . P.maybe (error "Error in EFA.Signal.Signal/getSample - Empty List") id
  . viewL
  . subSignal1D x
  . (:[])


{-
-- | get nested vector v1
getColumn :: TC Signal t1 (Data (v2 :> v1 :> Nil) d1) -> SignalIdx -> TC Signal t1 (Data (v1 :> Nil) d1)
getColumn idx =
   P.fst
  . P.maybe (error "Error in EFA.Signal.Signal/getColumn - Empty List") id
  . viewL
  . subSignal2D1D idx
  . (:[])
-}

-- | get a signal slice with startIndex and Number of elements
slice ::  (SV.Slice v1, SV.Storage v1 d1) =>  SignalIdx -> Int -> TC s1 t1 (Data (v1 :> Nil) d1) -> TC s1 t1 (Data (v1 :> Nil) d1)
slice (SignalIdx start) num (TC x) = TC $ D.slice start num x


-- | Interpolate an x-y - Lookup-Curve with a signal. Also can be used to resample a signal with a new time vector
interp1LinSig ::  (Show d1,
                     Product d1,
                     Ord d1,
                     SV.Storage v1 d1,
                     SV.Find v1,
                     SV.Singleton v1,
                     SV.Lookup v1,
                     SV.Walker v1,
                     Show (v1 d1)) =>
                   String ->
                   TC Signal t1 (Data (v1 :> Nil) d1) ->
                   TC Signal t2 (Data (v1 :> Nil) d1) ->
                   TC Signal t1 (Data (v1 :> Nil) d1) ->
                   TC Signal t2 (Data (v1 :> Nil) d1)
interp1LinSig caller xSig ySig xSigLookup = tmap f xSigLookup
  where f x = interp1Lin caller xSig ySig x



-- | Interpolate a 3-signal x-y surface, where in x points are aligned in rows
-- | TODO - interp2WinProfile verallgemeinern, so dass es statt mit Signal auch mit TestRow klappt
interp2WingProfile :: (Show (v1 d1), Show (v2 (v1 d1)),
                       SV.Storage v1 d1, Show d1,
                       Ord d1,
                       SV.Find v1,
                       Eq (v1 d1),
                       SV.Storage v2 (v1 d1),
                       SV.Singleton v2,
                       SV.Lookup v2 ,
                       SV.Singleton v1,
                       SV.Lookup v1,
                       Product d1) =>
                      String ->
                      TC Signal t1 (Data (v1 :> Nil) d1) ->
                      TC Signal t2 (Data (v2 :> v1 :> Nil) d1) ->
                      TC Signal t3 (Data (v2 :> v1 :> Nil) d1) ->
                      TC Sample t1 (Data Nil d1) ->
                      TC Sample t2 (Data Nil d1) ->
                      TC Sample t3 (Data Nil d1)
interp2WingProfile caller xSig ySig zSig xLookup yLookup =
   TC $ Data $ (z2 ~- z1) ~/ (x2 ~- x1) ~* (fromSample xLookup ~- x1) ~+ z1
   where
        -- find indices in x-axis
        xIdx2@(SignalIdx idx) =
          P.maybe (error msg) id $ findIndex (P.>= fromSample xLookup) xSig
        xIdx1 = SignalIdx $ if idx P.== 0 then error msg else idx-1

        -- get y and z data columns
        yRow1 = getColumn ySig xIdx1
        yRow2 = getColumn ySig xIdx2
        zRow1 = getColumn zSig xIdx1
        zRow2 = getColumn zSig xIdx2

        -- interpolate on y in these data columns
        newCaller = "interp2WingProfile, " ++ caller
        z1 = fromSample $ interp1Lin newCaller yRow1 zRow1 yLookup
        z2 = fromSample $ interp1Lin newCaller yRow2 zRow2 yLookup
        x1 = fromSample $ getSample xSig xIdx1
        x2 = fromSample $ getSample xSig xIdx2
        msg = "interp2WingProfile - Out of Range: " ++ caller ++ ": "
              ++ P.show xLookup ++ ", " ++ P.show yLookup ++ "\n"
              ++ P.show xSig ++ "\n" ++ P.show ySig


interp2WingProfileWithSignal :: (SV.Zipper v3,
                                 SV.Walker v3,
                                 SV.Storage v3 d,
                                 Eq (v1 d),
                                 Ord d,
                                 Show d,
                                 Show (v2 (v1 d)),
                                 Show (v1 d),
                                 SV.Storage v2 (v1 d),
                                 SV.Storage v1 d,
                                 SV.Singleton v1,
                                 SV.Singleton v2,
                                 SV.Lookup v1,
                                 SV.Lookup v2,
                                 SV.Find v1,
                                 Product d) =>
                                String ->
                    TC Signal t1 (Data (v1 :> Nil) d) ->
                    TC Signal t2 (Data (v2 :> v1 :> Nil) d) ->
                    TC Signal t3 (Data (v2 :> v1 :> Nil) d) ->
                    TC Signal t1 (Data (v3 :> Nil) d) ->
                    TC Signal t2 (Data (v3 :> Nil) d) ->
                    TC Signal t3 (Data (v3 :> Nil) d)
interp2WingProfileWithSignal caller x1d y2d z2d xSig ySig = tzipWith
         (\x y -> interp2WingProfile
           (caller ++ " - interp2WingProfileWithSignal")
           x1d y2d z2d x y) xSig ySig



-- | Scale Signal by a given Number
scale ::
  (Product d1, D.Map c1, D.Storage c1 d1) =>
  d1 -> TC s1 t1 (Data c1 d1) ->  TC s1 t1 (Data c1 d1)
scale fact = map (fact ~*)

-- | Scale Signal by a given Number
offset :: 
  (Sum d1, D.Map c1, D.Storage c1 d1) =>
  d1 -> TC s1 t1 (Data c1 d1) ->  TC s1 t1 (Data c1 d1)
offset offs = map (offs ~+)


-- | Reshape 2d to 1d
concat ::   (SV.Storage v2 (v1 (Apply c d)),
             SV.Storage v1 (Apply c d),
             SV.Singleton v1,
             SV.FromList v2) =>
           TC s t (Data (v2 :> v1 :> c) d) -> TC s t (Data (v1 :> c) d)
concat (TC x) = TC (D.concat x)

 {-
-- | Calculate an efficiency Time Signal from two Power time signals,
-- | p1 >= 0 means eta=p2/p1 else p1/p2
calcEta ::  (SV.Storage v1 d1,
         SV.Storage v1 (d1, d1),
         SV.Zipper v1, SV.Walker v1,Ord d1,Num d1, Product d1) =>
        TC Signal (Typ A P Tt) (Data (v1 :> Nil) d1) ->
        TC Signal (Typ A P Tt) (Data (v1 :> Nil) d1) ->
        TC Signal (Typ A N Tt) (Data (v1 :> Nil) d1)
calcEta p1 p2 = tmap f $ zip p1 p2
  where f :: (Ord d1,Num d1,Product d1)  => TC Sample (Typ A P Tt) (Data Nil (d1,d1)) ->
             TC Sample (Typ A N Tt) (Data Nil d1)
        f xy = if x P.>= (toSample 0)
               then y./x
               else x./y
          where (x,y) = unzip xy
-}

-- * Distributions


-- | Class Type
newtype Class a = Class a deriving (Show,Ord,Eq)

-- | A Simple classification Method with even class-Size
classifyEven :: (P.RealFrac d) => d -> d -> d -> Class d
classifyEven interval offs x = Class (P.fromIntegral((P.round ((x P.+ offs) P./ interval))::P.Integer) P.* interval P.- offs)

-- | Calculate a 1-d distribution -- collect signal Indices in classes
genDistribution1D :: (SV.Unique v (Class d),
                      SV.Storage v ([Class d],[SignalIdx]),
                      SV.Storage v d,
                      SV.FromList v,
                      Ord (Class d),
                      SV.Walker v,
                      SV.Storage v (Class d),
                      SV.Storage v Int,
                      SV.Storage v SignalIdx,
                      SV.Find v) =>
                     (d -> Class d) -> UTFSignal v d -> UTDistr v ([Class d], [SignalIdx])
genDistribution1D classify sig = changeSignalType $ map count $ unique classSig
  where classSig = map classify sig
        count cl = ([cl], toList $ findIndices (cl P.==) classSig)

genDistributionND ::
   (Ord d, SV.Storage v (Class d),
    SV.Storage v d, SV.Walker v, SV.FromList v) =>
   (d -> Class d) -> [UTFSignal v d] -> Map [Class d] (Set SignalIdx)
genDistributionND classify =
   Map.fromListWith Set.union .
   P.flip List.zip (fmap Set.singleton [SignalIdx 0 ..]) .
   List.transpose . fmap (toList . map classify)



-- | combine an amount of N 1d-Distributions in an N-d distribution
combineDistributions :: (SV.Storage v ([Class d], [SignalIdx]),
                         SV.FromList v,SV.Filter v) =>
                        [UTDistr v ([Class d], [SignalIdx])] -> UTDistr v ([Class d],[SignalIdx])
combineDistributions [] =  error("Error - empty list in combineDistributions")
combineDistributions (d:ds) = P.foldl f d ds
  where f acc e = filter (P.not . P.null . P.snd) $ combineWith g acc e
        g (classes1,indices1) (classes2,indices2) = (classes1++classes2,List.intersect indices1 indices2)


combineWith :: (SV.Storage v d3,
                SV.FromList v,
                SV.Storage v d1,
                SV.Storage v d2) =>
               (d1 -> d2 -> d3) -> TC s t (Data (v :> Nil) d1) -> TC s t (Data (v :> Nil) d2) ->  TC s t (Data (v :> Nil) d3)
combineWith f xs ys =
  fromList $ liftA2 f (toList xs) (toList ys)


{-calcDistributionValues ::
  (Num d,
   SV.Walker v,
   SV.Storage v ([Class d], [SignalIdx]),
   Eq d,
   SV.Storage v d,
   SV.Lookup v,
   Sum d) =>
  UTDistr v ([Class d],[SignalIdx]) ->
  TC FSignal t (Data (v :> Nil) d) ->
  TC FDistrib t (Data (v :> Nil) d)-}
calcDistributionValues :: (Eq d1, SV.Walker v, SV.Storage v d1, SV.Lookup v,
                           Constant d1, D.Storage c d1, D.Storage c (a, [SignalIdx]),
                           D.Map c, FoldType s, SumType s ~ Scalar) =>
                          TC FDistrib (Typ UT UT UT) (Data c (a, [SignalIdx]))
                          -> TC s typ (Data (v :> Nil) d1)
                          -> TC FDistrib (Typ delta1 t1 p1) (Data c d1)
calcDistributionValues d s = setType $ map f d
  where f = fromScalar . sum . subSignal1D s . P.snd

-- | etrigger or respectively ptrigger is the power signal used for classification
-- | usually local ein or eout is used
-- | pDist is the effective average trigger power which should be used as
-- | abscissa for efficiency over power
etaDistribution1D :: (P.RealFrac d,
                     Constant d,
                     SV.Zipper v,
                     SV.Lookup v,
                     SV.Walker v,
                     SV.Storage v d,
                     SV.FromList v,
                     SV.Find v,
                     SV.Unique v (Class d),
                     SV.Storage v SignalIdx,
                     SV.Storage v Int,
                     SV.Storage v (Class d),
                     SV.Storage v ([Class d], [SignalIdx]),
                     SV.Storage v (d, d),
                     SV.Singleton v,
                     SV.Storage v (d, (d, d))) =>
                    d -> d -> DTFSignal v d ->  FFSignal v d -> FFSignal v d  -> FFSignal v d ->
                    (PDistr v d, FDistr v d,FDistr v d, NDistr v d)
etaDistribution1D interval offs dtime  ein eout etrigger  = (pDist, einDist, eoutDist, nDist)
  where dist = genDistribution1D (classifyEven interval offs) $ untype ptrigger
        ptrigger = etrigger./dtime
        einDist = calcDistributionValues dist ein
        eoutDist = calcDistributionValues dist eout
        etriggerDist = calcDistributionValues dist etrigger
        dtimeDist = calcDistributionValues dist dtime
        nDist = calcEtaWithSign etriggerDist einDist eoutDist
        pDist = etriggerDist./ dtimeDist


-- | Calculate an efficiency Time Signal from two Power time signals,
-- | pSign >= 0 means eta=p2/p1 else p1/p2 // PSign is usually p1 or p2
calcEtaWithSign :: (SV.Storage v1 d1,
                    SV.Storage v1 (d1, (d1, d1)),
                    Constant d1,
                    Ord d1,
                    SV.Zipper v1,
                    SV.Walker v1,
                    SV.Storage v1 (d1, d1)) =>
                   TC s (Typ A F Tt) (Data (v1 :> Nil) d1) ->
                   TC s (Typ A F Tt) (Data (v1 :> Nil) d1) ->
                   TC s (Typ A F Tt) (Data (v1 :> Nil) d1) ->
                   TC s (Typ A N Tt) (Data (v1 :> Nil) d1)
calcEtaWithSign pSign p1 p2 = changeType $ map f $ changeSignalType $ zip pSign $ changeSignalType $ zip p1 p2
  where f (z,(x,y)) = if z P.>= Arith.zero then y ~/ x else x ~/ y


-- | Index of maximum

data ArgMax = ArgMax0 | ArgMax1 deriving (Eq, Enum, Show)

zipArgMax ::
   (Ord d, D.ZipWith c, D.Storage c d, D.Storage c ArgMax) =>
   TC s typ0 (Data c d) ->
   TC s typ0 (Data c d) ->
   TC (Arith s s) typ1 (Data c ArgMax)
zipArgMax =
   zipWith
      (\x y -> if x P.>= y then ArgMax0 else ArgMax1)

zipArgMaximum ::
   (Ord d, SV.FromList v, SV.Storage v d, SV.Storage v Int) =>
   NonEmpty.T [] (TC s typ0 (Data (v :> Nil) d)) ->
   TC (Arith s s) typ1 (Data (v :> Nil) Int)
zipArgMaximum =
   consData . SV.fromList .
   List.map (P.fst . SV.argMaximum) . transposeClip .
   fmap (SV.toList . unconsData)


argMaximum ::
   (Ord d, SV.FromList v, SV.Storage v d) =>
   TC s t (Data (NonEmpty.T v :> Nil) d) -> (SignalIdx, d)
argMaximum = argMaximumKey id

argMaximumKey ::
   (Ord d, SV.FromList v, SV.Storage v a) =>
   (a -> d) -> TC s t (Data (NonEmpty.T v :> Nil) a) -> (SignalIdx, a)
argMaximumKey f (TC (Data xs)) =
   mapFst SignalIdx $ SV.argMaximumKey f $
   NonEmpty.mapTail SV.toList xs

argMaximum2 ::
   (Ord d, SV.FromList v1, SV.FromList v2,
    SV.Storage v1 d, SV.Storage v2 (NonEmpty.T v1 d)) =>
   TC s t (Data (NonEmpty.T v2 :> NonEmpty.T v1 :> Nil) d) ->
   ((SignalIdx, SignalIdx), d)
argMaximum2 = argMaximumKey2 id

argMaximumKey2 ::
   (Ord d, SV.FromList v1, SV.FromList v2,
    SV.Storage v1 a, SV.Storage v2 (NonEmpty.T v1 a)) =>
   (a -> d) ->
   TC s t (Data (NonEmpty.T v2 :> NonEmpty.T v1 :> Nil) a) ->
   ((SignalIdx, SignalIdx), a)
argMaximumKey2 f (TC (Data xs)) =
   mapFst (mapPair (SignalIdx, SignalIdx)) $ SV.argMaximumKey2 f $
   fmap (NonEmpty.mapTail SV.toList) $
   NonEmpty.mapTail SV.toList xs


variation2D :: (SV.Storage v2 (v1 d),
                SV.Storage v1 d,
                SV.FromList v1,
                SV.FromList v2) =>
               TC s typ (Data (v1 :> Nil) d) ->
               TC s typ (Data (v1 :> Nil) d) ->
               (TC s typ (Data (v2 :> v1 :> Nil) d),
                TC s typ (Data (v2 :> v1 :> Nil) d))
variation2D xs ys =
   (fromList2 $ Match.replicate (toList ys) (toList xs),
    fromList2 $ List.map (Match.replicate (toList xs)) (toList ys))


mergeBy ::
  (SV.FromList v, SV.Storage v d) =>
  (d -> d -> Bool) ->
  TC s typ (Data (v :> Nil) d) ->
  TC s typ (Data (v :> Nil) d) ->
  TC s typ (Data (v :> Nil) d)
mergeBy p (TC x) (TC y) = TC $ D.fromList $ ListHT.mergeBy p xs ys
  where xs = D.toList x
        ys = D.toList y
