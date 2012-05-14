{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances, UndecidableInstances,KindSignatures, TypeOperators,  GADTs #-}

module EFA2.Signal.Signal (module EFA2.Signal.Signal) where
import EFA2.Signal.Vector
import EFA2.Signal.Base
import EFA2.Signal.Data


----------------------------------------------------------
-- | EFA data containers
{-
newtype TSignal d = TSignal a deriving (Show, Eq, Ord)

newtype Scalar d = Scalar a deriving (Show, Eq, Ord)

newtype Signal d = Signal a deriving (Show, Eq, Ord)
newtype Sample d = SampleVec a deriving (Show, Eq, Ord)

newtype Distrib d = Distrib a deriving (Show, Eq, Ord)
newtype Class d = Class a deriving (Show, Eq, Ord)
-}

newtype TC s d = TC d  deriving (Show, Eq, Ord)

data Signal

----------------------------------------------------------
-- Signal Aritmetics

class SArith s1 s2 s3 c1 c2 c3 | s1 s2 -> s3, c1 c2 -> c3   where
  (.*) :: TC s1 c1 -> TC s2 c2 -> TC s3 c3
--  (./) :: s1 -> s2 -> s3
--  (.+) :: s1 -> s2 -> s3
--  (.-) :: s1 -> s2 -> s3
     
instance  (DZipWith v1 v2 d1 d2 d3, DArith d1 d2 d3) =>  SArith Signal Signal Signal (v1 d1) (v2 d2) (v2 d3) where
          (.*) (TC x) (TC y) = TC $ dzipWith (..*) x y
--          (./) x y = sipWith (../) x y
--          (.+) x y = sipWith (..+) x y
--          (.-) x y = sipWith (..-) x y
