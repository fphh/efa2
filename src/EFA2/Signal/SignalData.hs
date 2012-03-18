{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes,  ImpredicativeTypes,  FlexibleContexts, GADTs, TypeFamilies, TypeSynonymInstances,IncoherentInstances    #-}

module EFA2.Signal.SignalData (module EFA2.Signal.SignalData) where

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F

import Text.Printf

-----------------------------------------------------------------------------------
-- Base Data Type

type Val = Double

-----------------------------------------------------------------------------------
-- Vector Convenience

type UVec a = UV.Vector a

uvmap :: (UV.Unbox a, UV.Unbox b) => (a -> b) -> UV.Vector a -> UV.Vector b 
uvmap = UV.map

uvzip :: (UV.Unbox a, UV.Unbox b, UV.Unbox c) => (a -> b -> c) -> UV.Vector a -> UV.Vector b -> UV.Vector c
uvzip = UV.zipWith

uvdiffMap :: (UV.Unbox a, UV.Unbox b) => (a -> a -> b) -> UV.Vector a -> UV.Vector b 
uvdiffMap f v = uvzip f (UV.tail v) (UV.init v)

-----------------------------------------------------------------------------------
-- Flow Signal
  
-- time
newtype TSample = TSample Val
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- time step data
newtype DTSample = DTSample Val
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- energy flow data or storage content data 
newtype ESample = ESample Val
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- average power of energy flow
newtype PSample = PSample Val
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- average power of energy flow
newtype PESample = PESample Val
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- average efficiency of energy flow
newtype NSample = NSample Val
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- energy flow mix data
newtype MSample = MSample Val
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- energy flow Split Share Data
newtype XSample = XSample Val
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- energy flow Collection Share Data Data
newtype YSample = YSample Val
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

-- Logical State Signal with several Values
newtype ZSample = ZSample Val
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)



--------------------------------------------------------------------------------------------
-- Sample Access Class

class (Eq a, Ord a, Fractional a, Num a, Enum a, Show a, Real a, Floating a, RealFloat a, RealFrac a, UV.Unbox a) => Sample a where
  fromSample :: a -> Val
  toSample :: Val -> a
  
instance Sample PSample where fromSample (PSample x) = x; toSample x = PSample x                               
instance Sample TSample where fromSample (TSample x) = x; toSample x = TSample x
instance Sample PESample where fromSample (PESample x) = x; toSample x = PESample x
instance Sample ESample where fromSample (ESample x) = x; toSample x = ESample x
instance Sample DTSample where fromSample (DTSample x) = x; toSample x = DTSample x
instance Sample NSample where fromSample (NSample x) = x; toSample x = NSample x
instance Sample XSample where fromSample (XSample x) = x; toSample x = XSample x
instance Sample YSample where fromSample (YSample x) = x; toSample x = YSample x
instance Sample MSample where fromSample (MSample x) = x; toSample x = MSample x
instance Sample ZSample where fromSample (ZSample x) = x; toSample x = ZSample x                              
-- also for Val !!
instance Sample Val where  fromSample (x) = x; toSample x = x

--------------------------------------------------------------------------------------------
-- Sample Calculation Class

class (Sample a, Sample b, Sample c) => SampleSum a b c  | a b -> c where
  (.+.) :: a -> b -> c
  (.+.) x y = toSample (fromSample x * fromSample y) 
  (.-.) :: a -> b -> c
  (.-.) x y = toSample (fromSample x * fromSample y) 
  
instance  SampleSum PESample PESample PESample
instance  SampleSum PSample PSample PSample

-- with Double
instance  SampleSum PSample Val PSample
instance  SampleSum Val PSample PSample


class (Sample a, Sample b, Sample c) => SampleProd a b c  | a b -> c where
  (.*.) :: a -> b -> c
  (.*.) x y = toSample (fromSample x * fromSample y) 

instance  SampleProd PESample NSample PESample
instance  SampleProd PESample XSample PESample
instance  SampleProd PESample YSample PESample
instance  SampleProd PESample MSample PESample

instance  SampleProd ESample NSample ESample
instance  SampleProd ESample XSample ESample
instance  SampleProd ESample YSample ESample
instance  SampleProd ESample MSample ESample

instance  SampleProd PESample DTSample ESample

-- with Double
instance  SampleProd PSample Val PSample
instance  SampleProd Val PSample PSample

class (Sample a, Sample b, Sample c) => SampleDiv a b c  | a b -> c where
  (./.) :: a -> b -> c
  (./.) x y = toSample (fromSample x * fromSample y) 


instance SampleDiv PSample TSample ESample 

-- with Double
instance  SampleDiv PSample Val PSample
instance  SampleDiv Val PSample PSample

--------------------------------------------------------------------------------------------
-- Data Containers 

data Signal a = Signal (UVec a) deriving (Show, Eq)
data FSignal a = FSignal (UVec a) deriving (Show, Eq)
data Distrib a = Distrib (UVec a) deriving (Show, Eq)
data Flow a = Flow a deriving (Show, Eq)


--------------------------------------------------------------------------------------------
-- Data Container Access Class

class (Sample b) => Data a b where
  fromData :: a -> UVec b
  toData :: UVec b -> a

instance  (Sample b) => Data (Signal b) b where
  fromData (Signal x) = x
  toData x = Signal x

instance  (Sample b) => Data (FSignal b) b where
  fromData (FSignal x) = x
  toData x = FSignal x
  
instance  (Sample b) => Data (Distrib b) b where  
  fromData (Distrib x) = x
  toData x = Distrib x
  
--------------------------------------------------------------------------------------------
-- Generic Product Class with Instances

class Sum x y z  | x y -> z where  (.+) :: x -> y ->  z; (.-) :: x -> y ->  z 
instance (SampleSum a b c) => Sum (Signal a) (Signal b) (Signal c) where 
  (.+) (Signal x) (Signal y) = Signal (uvzip (.+.) x y); (.-) (Signal x) (Signal y) = Signal (uvzip (.-.) x y)

class Prod x y z  | x y -> z where  (.*) :: x -> y ->  z
instance (SampleProd a b c) => Prod (Signal a) (Signal b) (Signal c) where (.*) (Signal x) (Signal y) = Signal (uvzip (.*.) x y) 

class Div x y z  | x y -> z where  (./) :: x -> y ->  z
instance (SampleDiv a b c) => Div (Signal a) (Signal b) (Signal c) where (./) (Signal x) (Signal y) =  Signal (uvzip (./.) x y)



-----------------------------------------------------------------------------------
-- TypeSafe Indexing

-- Index Types
newtype SignalIdx = SignalIdx Int deriving (Show, Eq, Ord, Num)
newtype FSignalIdx  = FSignalIdx Int deriving (Show, Eq, Ord, Num) 
newtype DistribIdx  = DistribIdx Int deriving (Show, Eq, Ord, Num) 

-- Index Access Class
class Index a where fromIdx :: a -> Int; toIdx :: Int -> a
instance Index SignalIdx where fromIdx (SignalIdx x) = x; toIdx x = (SignalIdx x)
instance Index FSignalIdx where fromIdx (FSignalIdx x) = x; toIdx x = (FSignalIdx x)
instance Index DistribIdx where fromIdx (DistribIdx x) = x; toIdx x = (DistribIdx x)

-- Indexible Data Containers
class (Indexible a b) where 
instance Indexible (Signal a) b
instance Indexible (FSignal a) b
instance Indexible (Distrib a) b

-- Class to combine Index and Container & perform safe lookup
class (Index a, Indexible b c) => IndexPair a b c  | a -> b , b-> a , a  b -> c where
  (!) :: b -> a -> c 

instance ( UV.Unbox c) => IndexPair SignalIdx (Signal c) c where 
  (!) (Signal vect) (SignalIdx idx) =  maybe err id (vect UV.!? idx)
    where err = error ("Error in SignalIndexing - Index out of Range :" ++ show idx)

  
instance ( UV.Unbox c) => IndexPair FSignalIdx (FSignal c) c where 
  (!) (FSignal vect) (FSignalIdx idx) = maybe err id (vect UV.!? idx) 
    where err = error ("Error in FSignalIndexing - Index out of Range :" ++ show idx)


instance ( UV.Unbox c) => IndexPair DistribIdx (Distrib c) c where 
  (!) (Distrib vect) (DistribIdx idx) = maybe err id (vect UV.!? idx) 
    where err = error ("Error in DistribIndexing - Index out of Range :" ++ show idx)


-----------------------------------------------------------------------------------
-- Mapping and Zipping
          
class (Sample a, Sample b, Data (cont a) a, Data (cont b) b) => DataMap cont a b where          
  dmap :: (a -> b) -> cont a -> cont b 
  dmap f d = toData $ uvmap f (fromData d)  
  
-- careful reduces Signal length by one and should change container type // therefore disabled  
--  diffMap :: (a -> a -> b) -> cont a -> cont b 
--  diffMap f d = toData $ uvzip f (UV.tail (fromData d)) (UV.init (fromData d))

instance (Sample a, Sample b) => DataMap Signal a b 
instance (Sample a, Sample b) => DataMap FSignal a b 
instance (Sample a, Sample b) => DataMap Distrib a b


class  (Sample a, Sample b, Sample c, Data (cont a) a, Data (cont b) b, Data (cont c) c) => DataZipWith cont a b c where          
  dzipWith :: (a -> b -> c) -> cont a -> cont b -> cont c
  dzipWith f d1 d2 = toData $ uvzip f (fromData d1) (fromData d2)
  
instance (Sample a, Sample b, Sample c) => DataZipWith Signal a b c 
instance (Sample a, Sample b, Sample c) => DataZipWith FSignal a b c 
instance (Sample a, Sample b, Sample c) => DataZipWith Distrib a b c 

class (Sample a, Data (cont a) a) => DataAll cont a where  
  dall  :: (a -> Bool) -> cont a -> Bool 
  dall  f d = UV.all f (fromData d)
  
instance (Sample a) => DataAll Signal a  
instance (Sample a) => DataAll FSignal a  
instance (Sample a) => DataAll Distrib a

-- class (Sample a, Data (cont a) a, Index b, Indexible a b) => DataFindIndices cont a b where
--   dfindIndices :: (a -> Bool) -> cont a -> [b] 
--   dfindIndices f d = map (toIdx) $ UV.toList (UV.findIndices f (fromData d)) 
  
class  (Sample a, Data (cont a) a) => DataLength cont a  where
  dlength :: cont a -> Int 
--   dlength d = UV.length (fromData d) -- funktioniert nicht
  
instance (Sample a) => DataLength Signal a  where dlength (Signal v) = UV.length v 
instance (Sample a) => DataLength FSignal a where dlength (FSignal v) = UV.length v 
instance (Sample a) => DataLength Distrib a where dlength (Distrib v) = UV.length v
  
class (Sample a, Data (cont a) a) => DataHead cont a where
  dhead :: cont a -> a
  dhead d = UV.head (fromData d)

  dlast :: cont a -> a
  dlast d = UV.last (fromData d)

instance (Sample a) => DataHead Signal a  
instance (Sample a) => DataHead FSignal a 
instance (Sample a) => DataHead Distrib a 

class (Sample a, Data (cont a) a) => DataTail cont a where
  dinit :: cont a -> cont a
--  dinit d = toData (UV.init (fromData d))

  dtail :: cont a -> cont a
--  dtail d = toData (UV.tail (fromData d))

instance (Sample a) => DataTail Signal a where dinit (Signal v) = Signal $ UV.init v;  dtail (Signal v) = Signal $ UV.tail v  
instance (Sample a) => DataTail FSignal a where dinit (FSignal v) = FSignal $ UV.init v; dtail (FSignal v) = FSignal $ UV.tail v 
instance (Sample a) => DataTail Distrib a where dinit (Distrib v) = Distrib $ UV.init v; dtail (Distrib v) = Distrib $ UV.tail v 

class  DataSlice cont a b where
  dslice :: cont a -> (b,b) -> cont a
  
instance (Sample a) => DataSlice Signal a SignalIdx 
     where dslice (Signal v) (SignalIdx idx1, SignalIdx idx2) = Signal $ UV.slice (idx1+1) (idx2-(idx1+1)) v
           
instance (Sample a) => DataSlice FSignal a FSignalIdx 
     where dslice (FSignal v) (FSignalIdx idx1, FSignalIdx idx2) = FSignal $ UV.slice (idx1+1) (idx2-(idx1+1)) v
           
instance (Sample a) => DataSlice Distrib a DistribIdx 
     where dslice (Distrib v) (DistribIdx idx1, DistribIdx idx2) = Distrib  $ UV.slice (idx1+1) (idx2-(idx1+1)) v

class DataCat cont a where
  (.++) :: cont a -> cont a -> cont a
  
instance (Sample a) => DataCat Signal a where (.++) (Signal v1) (Signal v2) = Signal (v1 UV.++ v2) 
instance (Sample a) => DataCat FSignal a where (.++) (FSignal v1) (FSignal v2) = FSignal (v1 UV.++ v2) 
instance (Sample a) => DataCat Distrib a where (.++) (Distrib v1) (Distrib v2) = Distrib (v1 UV.++ v2) 

class (Data (cont a) a) => DataFromList cont a where
  dfromList :: [a] -> cont a
  dfromList l = toData (UV.fromList l)
  dtoList :: cont a -> [a]
  dtoList l = UV.toList (fromData l)
  
instance (Sample a) => DataFromList Signal a 
instance (Sample a) => DataFromList FSignal a 
instance (Sample a) => DataFromList Distrib a 

-----------------------------------------------------------------------------------
-- Type Synonyms for Convenience

type Time = Signal TSample
type DTime = FSignal DTSample

type Power = Signal PSample

type EFSignal = FSignal ESample
type NFSignal = FSignal NSample
type XFSignal = FSignal XSample
type YFSignal = FSignal YSample
type MFSignal = FSignal MSample


-----------------------------------------------------------------------------------
-- Display Functions -- For Graphical Output

class Display a where  disp :: a -> String
instance Display PSample where disp s = (printf "%6.7f" (fromSample s/1000)) ++ " kWh"  
instance Display PESample where disp s = (printf "%6.7f" (fromSample s/1000)) ++ " kWh"  
instance Display TSample where disp s = (printf "%6.7f" (fromSample s)) ++ " s"  
instance Display DTSample where disp s = (printf "%6.7f" (fromSample s))  ++ " s"  

instance Display ESample where disp s = (printf "%6.7f" (fromSample s/1000/3600)) ++ " kWh"  

instance Display NSample where disp s = (printf "%6.7f" (fromSample s)) ++ "-- "  
instance Display XSample where disp s = (printf "%6.7f" (fromSample s)) ++ "-- "  
instance Display YSample where disp s = (printf "%6.7f" (fromSample s)) ++ "-- "  
instance Display MSample where disp s = (printf "%6.7f" (fromSample s)) ++ "-- "  
  
