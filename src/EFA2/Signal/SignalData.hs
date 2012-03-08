{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances,  RankNTypes #-}

module EFA2.Signal.SignalData where

import qualified Data.Map as M
import qualified Data.List as L

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Foldable as F


newtype Time = Time { unTime :: Double } deriving (Show, Eq, Num, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

newtype DTime =  DTime { unDTime :: Double } deriving (Show, Eq, Num, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

newtype Index = Index Int deriving (Eq, Ord, Num, Enum, Show)

newtype PSample = PSample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

newtype PEta = PEta Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

newtype ESample = ESample Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

newtype EEta = EEta Double
  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)

type Signal a = UV.Vector a

--newtype EFlow = Eflow [ESample] 
--  deriving (Eq, Ord, Fractional, Num, Enum, Show, Real, Floating, RealFloat, RealFrac, UV.Unbox, GV.Vector UV.Vector, MV.MVector UV.MVector)


class (Sample a, Eta b) => SameUnit a b | a -> b, b -> a
instance SameUnit ESample EEta 
instance SameUnit PSample PEta



-- When indexing for eta signals we don't care about xy or yx,
-- because it's the same edge in the graph.
-- The difference is only important when indexing for sample signals,
-- because there are two sample signals per edge.

data PPosIndex = PPosIndex !Int !Int deriving (Show, Ord, Eq)

data EtaIndex = EtaIndex !Int !Int deriving (Show)


instance Eq EtaIndex where
         (EtaIndex a b) == (EtaIndex x y) = f a b == f x y
           where f u v = if u < v then [u, v] else [v, u]

instance Ord EtaIndex where
         compare as@(EtaIndex a b) bs@(EtaIndex x y)
           | as == bs = EQ
           | otherwise = compare (a, b) (x, y)

type EtaEnv a = (Eta a) => M.Map EtaIndex (Signal a)

type PPosEnv a = (Sample a) => M.Map PPosIndex (Signal a)


class IndexC a where
      mkIdx :: Int -> Int -> a

instance IndexC EtaIndex where
         mkIdx = EtaIndex

instance IndexC PPosIndex where
         mkIdx = PPosIndex


class Sample a where
      fromSample :: a -> Double
      toSample :: Double -> a

instance Sample PSample where
         fromSample (PSample x) = x
         toSample x = PSample x

instance Sample ESample where
         fromSample (ESample x) = x
         toSample x = ESample x

class Eta a where
         fromEta :: a -> Double
         toEta :: Double -> a

instance Eta EEta where
         fromEta (EEta x) = x
         toEta x = EEta x

instance Eta PEta where
         fromEta (PEta x) = x
         toEta x = PEta x


