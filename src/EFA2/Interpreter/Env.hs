{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module EFA2.Interpreter.Env where

import Control.Monad.Error

import qualified Data.Map as M
import qualified Data.List as L

-- | Variable types of the solver. The solver, in fact, is
-- ignorant of the provenance of the variables. However, to
-- facilitate life, we introduce variable types, that make
-- it easy to express things needed in energy flow analysis,
-- that is:
--
-- * a section number
-- * a data record number
-- * two numbers to identify a place in the topology 
--   (for equation generation, we use the underlying fgl node ids.

-- | Energy variables.
data EnergyIdx = EnergyIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)
data DEnergyIdx = DEnergyIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)

-- | Power variables.
data PowerIdx = PowerIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)
data DPowerIdx = DPowerIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)

-- | Eta variables.
--data EtaIdx = EtaIdx !Int !Int !Int !Int deriving (Show)
data FEtaIdx = FEtaIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)
data DEtaIdx = DEtaIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)

-- | Splitting factors.
data XIdx = XIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)

-- | Delta time variables, depending solely on their section and record number.
data DTimeIdx = DTimeIdx !Int !Int deriving (Show, Ord, Eq)


-- | Section number, record number, storage number.
data StorageIdx = StorageIdx !Int !Int !Int deriving (Show, Ord, Eq)

-- | This variable type can be used to express arbitrary relations.
-- You can variables also make dependent on section and record.
-- ATTENTION: Some of them are used for equation generation for
-- performance issues. You have to make sure yourself if your
-- variable is unique in the equational system.
data VarIdx = VarIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)

{-
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
-}





-- Environments
type EnergyMap a = M.Map EnergyIdx a
type DEnergyMap a = M.Map DEnergyIdx a

type PowerMap a = M.Map PowerIdx a
type DPowerMap a = M.Map DPowerIdx a

--type EtaMap a = M.Map EtaIdx a
type FEtaMap a = M.Map FEtaIdx (a -> a)
type DEtaMap a = M.Map DEtaIdx a

type DTimeMap a = M.Map DTimeIdx a

type XMap a = M.Map XIdx a
type VarMap a = M.Map VarIdx a
type StorageMap a = M.Map StorageIdx a



data Envs a = Envs { recordNumber :: RecordNumber,
                     energyMap :: EnergyMap a,
                     denergyMap :: DEnergyMap a,
                     powerMap :: PowerMap a,
                     dpowerMap :: DPowerMap a,
                     fetaMap :: FEtaMap a,
                     detaMap :: DEtaMap a,
                     dtimeMap :: DTimeMap a,
                     xMap :: XMap a,
                     varMap :: VarMap a,
                     storageMap :: StorageMap a } deriving (Show)


instance (Show a) => Show (a -> a) where
         show _ = "<function (a -> a)>"

instance Eq (a -> a) where
         _ == _ = True

instance Ord (a -> a) where
         compare _ _ = EQ

emptyEnv :: Envs a
emptyEnv = Envs NoRecord M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty


data RecordNumber = NoRecord
                  | SingleRecord Int
                  | MixedRecord [Int] deriving (Eq, Ord, Show)

isSingleRecord :: RecordNumber -> Bool
isSingleRecord (SingleRecord _) = True
isSingleRecord _ = False

fromSingleRecord :: RecordNumber -> Int
fromSingleRecord (SingleRecord x) = x
fromSingleRecord x = error $ "fromSingleRecord: not a single record: " ++ show x

uniteRecordNumbers :: [RecordNumber] -> RecordNumber
uniteRecordNumbers [] = NoRecord
uniteRecordNumbers rs = L.foldl' f (MixedRecord []) rs
  where f NoRecord _ = NoRecord
        f _ NoRecord = NoRecord
        f (MixedRecord xs) (SingleRecord x) = MixedRecord (xs ++ [x])
        f (MixedRecord xs) (MixedRecord ys) = MixedRecord (xs ++ ys) 

envUnion :: [Envs a] -> Envs a
envUnion envs = Envs { recordNumber = uniteRecordNumbers (map recordNumber envs),
                       energyMap = M.unions $ map energyMap envs,
                       denergyMap = M.unions $ map denergyMap envs,
                       powerMap = M.unions $ map powerMap envs,
                       dpowerMap = M.unions $ map dpowerMap envs,
                       fetaMap = M.unions $ map fetaMap envs,
                       detaMap = M.unions $ map detaMap envs,
                       dtimeMap = M.unions $ map dtimeMap envs,
                       xMap = M.unions $ map xMap envs,
                       varMap = M.unions $ map varMap envs,
                       storageMap = M.unions $ map storageMap envs }