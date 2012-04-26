{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances, FunctionalDependencies, FlexibleInstances, UndecidableInstances, FlexibleContexts, StandaloneDeriving, ExistentialQuantification, KindSignatures, ScopedTypeVariables, TemplateHaskell #-}


module EFA2.Signal.SignalData where


import qualified Data.Vector.Unboxed as UV

import EFA2.Signal.TH

-- type Val = Double
type Vec = UV.Vector
type List = []
newtype Value a = Value { unValue :: a } deriving (Show, Eq, Ord, Num, Fractional)

newtype Signal (cont :: * -> *) = Signal cont deriving (Show, Eq, Num)
newtype FSignal (cont :: * -> *)  = FSignal cont deriving (Show, Eq, Num)
newtype Distrib (cont :: * -> *)  = Distrib cont deriving (Show, Eq, Num)


newtype Flow' (cont :: * -> *) dim = Flow' (dim cont) deriving (Show, Eq, Num)
type Flow = Flow' Value

class Container (sig :: (* -> *) -> ((* -> *) -> *) -> *) cont dim where
      fromContainer :: sig cont dim -> dim cont
      toContainer :: dim cont -> sig cont dim

instance Container Signal cont dim where
         fromContainer (Signal v) = v
         toContainer v = Signal v

instance Container FSignal cont dim where
         fromContainer (FSignal v) = v
         toContainer v = FSignal v

instance Container Distrib cont dim where
         fromContainer (Distrib v) = v
         toContainer v = Distrib v

instance Container Flow' cont dim where
         fromContainer (Flow' v) = v
         toContainer v = Flow' v

from :: (Container sig cont dim, Sample cont dim) => sig cont dim -> cont Val
from = fromSample . fromContainer

to :: (Container sig cont dim, Sample cont dim) => cont Val -> sig cont dim
to = toContainer . toSample

fromVSig :: (Container sig Vec dim, Sample Vec dim) => sig Vec dim -> [Val]
fromVSig = UV.toList . from

toVSig :: (Container sig Vec dim, Sample Vec dim) => [Val] -> sig Vec dim
toVSig = to . UV.fromList

fromSig :: (Container sig Value dim, Sample Value dim) => sig Value dim -> Val
fromSig = unValue . from

toSig :: (Container sig Value dim, Sample Value dim) => Val -> sig Value dim
toSig = to . Value

toLSig :: (Container sig List dim, Sample List dim) => [Val] -> sig List dim
toLSig = to

class Additive cont dim where
      (.+) :: (Container sig cont dim) => sig cont dim -> sig cont dim -> sig cont dim
      (.-) :: (Container sig cont dim) => sig cont dim -> sig cont dim -> sig cont dim
      neg :: (Container sig cont dim) => sig cont dim -> sig cont dim


instance (Sample Value dim) => Additive Value dim where
         x .+ y = to $ from x + from y
         x .- y = to $ from x + from y
         neg = to . negate . from

instance (Sample List dim) => Additive List dim where
         x .+ y = to $ zipWith (+) (from x) (from y)
         x .- y = to $ zipWith (-) (from x) (from y)
         neg x = to (map negate (from x))

instance (Sample Vec dim) => Additive Vec dim where
         x .+ y = to $ UV.zipWith (+) (from x) (from y)
         x .- y = to $ UV.zipWith (-) (from x) (from y)
         neg x = to (UV.map negate (from x))

{-
class Reciprocal cont dim where
      reciprocal :: (Container sig cont dim) => sig cont dim -> sig cont dim

instance Reciprocal cont dim
-}

class Multiplicative cont dima dimb dimc | dima dimb -> dimc, dima dimc -> dimb, dimb dimc -> dima where
      (.*) :: (Container sig cont dima, Container sig cont dimb, Container sig cont dimc) 
              => sig cont dima -> sig cont dimb -> sig cont dimc
      (./) :: (Container sig cont dima, Container sig cont dimb, Container sig cont dimc) 
              => sig cont dimc -> sig cont dimb -> sig cont dima

instance Multiplicative Value PSample NSample PSample where
         x .* y = to $ from x * from y
         x ./ y = to $ from x / from y

instance Multiplicative List PSample NSample PSample where
         x .* y = to $ zipWith (*) (from x) (from y)
         x ./ y = to $ zipWith (/) (from x) (from y)

instance Multiplicative Vec PSample NSample PSample where
         x .* y = to $ UV.zipWith (*) (from x) (from y)
         x ./ y = to $ UV.zipWith (/) (from x) (from y)

--instance (Multiplicative cont dima dimb dimc) => Multiplicative cont dimb dima dimc


class (Container sig cont dima, Container sig cont dimb, Container sig cont dimc,
       Additive cont dima, Additive cont dimb, Additive cont dimc,
       Multiplicative cont dima dimb dimc) => Arithmetic sig cont dima dimb dimc

-- takes all necessary constraints in.
instance (Container sig cont dima, Container sig cont dimb, Container sig cont dimc,
          Additive cont dima, Additive cont dimb, Additive cont dimc,
          Multiplicative cont dima dimb dimc) => Arithmetic sig cont dima dimb dimc

-- has to be enhanced...
--instance Arithmetic Signal Value ASample BSample BSample
--instance Arithmetic FSignal Value ASample BSample BSample

--instance Arithmetic Signal List ASample BSample BSample
--instance Arithmetic FSignal List ASample BSample BSample

--instance Arithmetic Signal Vec ASample BSample BSample
--instance Arithmetic FSignal Vec ASample BSample BSample

{-
a1 :: Signal Value ASample
a1 = toSig 8

favec :: FSignal Vec ASample
favec = toVSig [5, 6, 7]

f1 :: FSignal Value ASample
f1 = toSig 9

fb :: Signal Value BSample
fb = toSig 16

flow :: Flow BSample
flow = toSig 51

fbvec :: FSignal Vec BSample
fbvec = toVSig [1, 2, 3]

testf :: (Arithmetic sig cont dima dimb dimc) => sig cont dima -> sig cont dimb -> sig cont dimc
testf x y = (x .+ x) .* y

test1 = testf favec fbvec

-- will not work, ok!
-- test2 = testf fbvec favec

test3 = testf a1 fb

-- will not work, ok!
-- test4 = testf fb a1

-}