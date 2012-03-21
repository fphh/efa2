{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeSynonymInstances, FunctionalDependencies, FlexibleInstances, UndecidableInstances, FlexibleContexts, StandaloneDeriving, ExistentialQuantification, KindSignatures, ScopedTypeVariables #-}


module Main where


import qualified Data.Vector.Unboxed as UV


type Val = Double
type Vec = UV.Vector
type List = []
newtype Single a = Single { unSingle :: a } deriving (Show, Eq, Ord, Num, Fractional)

newtype ASample cont = ASample { unASample :: cont Val }
deriving instance Show (cont Val) => Show (ASample cont)
deriving instance Eq (cont Val) => Eq (ASample cont)
deriving instance Num (cont Val) => Num (ASample cont)

newtype BSample cont = BSample { unBSample :: cont Val }
deriving instance Show (cont Val) => Show (BSample cont)
deriving instance Eq (cont Val) => Eq (BSample cont)
deriving instance Num (cont Val) => Num (BSample cont)

newtype Signal (cont :: * -> *) dim = Signal (dim cont) deriving (Show, Eq, Num)
newtype FSignal (cont :: * -> *) dim = FSignal (dim cont) deriving (Show, Eq, Num)
newtype Distrib (cont :: * -> *) dim = Distrib (dim cont) deriving (Show, Eq, Num)

newtype Flow' (cont :: * -> *) dim = Flow' (dim cont) deriving (Show, Eq, Num)
type Flow = Flow' Single

class Sample cont dim where
      fromSample :: dim cont -> cont Val
      toSample :: cont Val -> dim cont

instance Sample cont ASample where
         fromSample = unASample
         toSample = ASample

instance Sample cont BSample where
         fromSample = unBSample
         toSample = BSample

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

fromSig :: (Container sig Single dim, Sample Single dim) => sig Single dim -> Val
fromSig = unSingle . from

toSig :: (Container sig Single dim, Sample Single dim) => Val -> sig Single dim
toSig = to . Single

toLSig :: (Container sig List dim, Sample List dim) => [Val] -> sig List dim
toLSig = to

class Additive cont dim where
      (.+) :: (Container sig cont dim) => sig cont dim -> sig cont dim -> sig cont dim
      (.-) :: (Container sig cont dim) => sig cont dim -> sig cont dim -> sig cont dim
      neg :: (Container sig cont dim) => sig cont dim -> sig cont dim


instance (Sample Single dim) => Additive Single dim where
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


class Multiplicative cont dima dimb dimc | dima dimb -> dimc, dima dimc -> dimb, dimb dimc -> dima where
      (.*) :: (Container sig cont dima, Container sig cont dimb, Container sig cont dimc) 
              => sig cont dima -> sig cont dimb -> sig cont dimc
      (./) :: (Container sig cont dima, Container sig cont dimb, Container sig cont dimc) 
              => sig cont dimc -> sig cont dimb -> sig cont dima

instance Multiplicative Single ASample BSample BSample where
         x .* y = to $ from x * from y
         x ./ y = to $ from x / from y

instance Multiplicative List ASample BSample BSample where
         x .* y = to $ zipWith (*) (from x) (from y)
         x ./ y = to $ zipWith (/) (from x) (from y)

instance Multiplicative Vec ASample BSample BSample where
         x .* y = to $ UV.zipWith (*) (from x) (from y)
         x ./ y = to $ UV.zipWith (/) (from x) (from y)

-- takes all necessary constraints in.
class (Container sig cont dima, Container sig cont dimb, Container sig cont dimc,
       Additive cont dima, Additive cont dimb, Additive cont dimc,
       Multiplicative cont dima dimb dimc) => Arithmetic sig cont dima dimb dimc


-- has to be enhanced...
instance Arithmetic Signal Single ASample BSample BSample
instance Arithmetic FSignal Single ASample BSample BSample

instance Arithmetic Signal List ASample BSample BSample
instance Arithmetic FSignal List ASample BSample BSample

instance Arithmetic Signal Vec ASample BSample BSample
instance Arithmetic FSignal Vec ASample BSample BSample

a1 :: Signal Single ASample
a1 = toSig 8

favec :: FSignal Vec ASample
favec = toVSig [5, 6, 7]

f1 :: FSignal Single ASample
f1 = toSig 9

fb :: Signal Single BSample
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