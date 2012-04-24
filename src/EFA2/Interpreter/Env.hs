
module EFA2.Interpreter.Env where

import Control.Monad.Error

import qualified Data.Map as M

-- | Variable types of the solver. The solver, in fact, is
-- ignorant of the provenance of the variables. However, to
-- facilitate life, we introduce variable types, that make
-- it easy to express things needed in energy flow analysis,
-- that is:
--
-- * a section number
-- * a data record number
-- * two numbers to identify a place in the topology
data PowerIdx = PowerIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)
data EtaIdx = EtaIdx !Int !Int !Int !Int deriving  (Show)
data XIdx = XIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)
data DEtaIdx = DEtaIdx !Int !Int !Int !Int deriving  (Show)
data DPowerIdx = DPowerIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)

-- | This variable type can be used to express arbitrary relations.
-- You can variables also make dependent on section and record.
data VarIdx = VarIdx !Int !Int !Int deriving (Show, Ord, Eq)

-- EtaIdx x y == EtaIdx y x
instance Eq EtaIdx where
         (EtaIdx s1 r1 a b) == (EtaIdx s2 r2 x y) = (s1, r1, f a b) == (s2, r2, f x y)
           where f u v = if u < v then (u, v) else (v, u)

instance Ord EtaIdx where
         compare as@(EtaIdx s1 r1 a b) bs@(EtaIdx s2 r2 x y)
           | as == bs && s1 == s2 && r1 == r2 = EQ
           | otherwise = compare (s1, r1, (f a b)) (s2, r2, (f x y))
               where f u v = if u < v then (u, v) else (v, u)

instance Eq DEtaIdx where
         (DEtaIdx s1 r1 a b) == (DEtaIdx s2 r2 x y) = (s1, r1, f a b) == (s2, r2, f x y)
           where f u v = if u < v then (u, v) else (v, u)

instance Ord DEtaIdx where
         compare as@(DEtaIdx s1 r1 a b) bs@(DEtaIdx s2 r2 x y)
           | as == bs && s1 == s2 && r1 == r2 = EQ
           | otherwise = compare (s1, r1, (f a b)) (s2, r2, (f x y))
               where f u v = if u < v then (u, v) else (v, u)






-- Environments
type PowerMap a = M.Map PowerIdx a
type EtaMap a = M.Map EtaIdx a
type DPowerMap a = M.Map DPowerIdx a
type DEtaMap a = M.Map DEtaIdx a
type XMap a = M.Map XIdx a
type VarMap a = M.Map VarIdx a


data Envs a = Envs { powerMap :: PowerMap a,
                     dpowerMap :: DPowerMap a,
                     etaMap :: EtaMap a,
                     detaMap :: DEtaMap a,
                     xMap :: XMap a,
                     varMap :: VarMap a } deriving (Show)

