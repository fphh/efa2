{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module EFA2.Interpreter.Env where

import qualified Data.Map as M
import qualified Data.List as L

import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Data as D
import qualified EFA2.Signal.Vector as SV
import EFA2.Signal.Signal (TC, (.-))
import EFA2.Signal.Data (Data, Zip)
import EFA2.Signal.Typ (TSum)
import EFA2.Signal.Base (BSum)

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
data FEtaIdx = FEtaIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)
data DEtaIdx = DEtaIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)

-- | Splitting factors.
data XIdx = XIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)
data DXIdx = DXIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)

-- | Delta time variables, depending solely on their section and record number.
data DTimeIdx = DTimeIdx !Int !Int deriving (Show, Ord, Eq)


-- | Section number, record number, storage number.
data StorageIdx = StorageIdx !Int !Int !Int deriving (Show, Ord, Eq)

-- | This variable type can be used to express arbitrary relations.
-- You can variables also make dependent on section and record.
-- ATTENTION: Some of them are used for equation generation for
-- performance issues. You have to make sure yourself if your
-- variable is unique in the equational system.
--data VarIdx = VarIdx !Int !Int !Int !Int deriving (Show, Ord, Eq)

data Use = InSum
         | OutSum
         | InDiffSum
         | OutDiffSum
         | St deriving (Show, Eq, Ord)

toDiffUse :: Use -> Use
toDiffUse InSum = InDiffSum
toDiffUse OutSum = OutDiffSum

data VarIdx = VarIdx !Int !Int Use !Int deriving (Show, Ord, Eq)


data Index =
            Energy EnergyIdx
          | DEnergy DEnergyIdx
          | Power PowerIdx
          | DPower DPowerIdx
          | FEta FEtaIdx
          | DEta DEtaIdx
          | DTime DTimeIdx
          | X XIdx
          | DX DXIdx
          | Var VarIdx
          | Store StorageIdx
             deriving (Show, Eq, Ord)


class IdxRecNum a where
      getIdxRecNum :: a -> Int

instance IdxRecNum EnergyIdx where
         getIdxRecNum (EnergyIdx _ r _ _) = r

instance IdxRecNum DEnergyIdx where
         getIdxRecNum (DEnergyIdx _ r _ _) = r

instance IdxRecNum PowerIdx where
         getIdxRecNum (PowerIdx _ r _ _) = r

instance IdxRecNum DPowerIdx where
         getIdxRecNum (DPowerIdx _ r _ _) = r

instance IdxRecNum FEtaIdx where
         getIdxRecNum (FEtaIdx _ r _ _) = r

instance IdxRecNum DEtaIdx where
         getIdxRecNum (DEtaIdx _ r _ _) = r

instance IdxRecNum XIdx where
         getIdxRecNum (XIdx _ r _ _) = r

instance IdxRecNum DXIdx where
         getIdxRecNum (DXIdx _ r _ _) = r

instance IdxRecNum DTimeIdx where
         getIdxRecNum (DTimeIdx _ r) = r

instance IdxRecNum StorageIdx where
         getIdxRecNum (StorageIdx _ r _) = r

instance IdxRecNum VarIdx where
         getIdxRecNum (VarIdx _ r _ _) = r

class IdxEq a where
      ignoreRecEq :: a -> a -> Bool

instance IdxEq PowerIdx where
         ignoreRecEq (PowerIdx a _ b c) (PowerIdx x _ y z) = a == x && b == y && c == z

instance IdxEq EnergyIdx where
         ignoreRecEq (EnergyIdx a _ b c) (EnergyIdx x _ y z) = a == x && b == y && c == z

instance IdxEq FEtaIdx where
         ignoreRecEq (FEtaIdx a _ b c) (FEtaIdx x _ y z) = a == x && b == y && c == z

instance IdxEq XIdx where
         ignoreRecEq (XIdx a _ b c) (XIdx x _ y z) = a == x && b == y && c == z

instance IdxEq StorageIdx where
         ignoreRecEq (StorageIdx a _ b) (StorageIdx x _ y) = a == x && b == y

-- Environments
type EnergyMap a = M.Map EnergyIdx a
type DEnergyMap a = M.Map DEnergyIdx a

type PowerMap a = M.Map PowerIdx a
type DPowerMap a = M.Map DPowerIdx a

type FEtaMap a = M.Map FEtaIdx (a -> a)
type DEtaMap a = M.Map DEtaIdx (a -> a)

type DTimeMap a = M.Map DTimeIdx a

type XMap a = M.Map XIdx a
type DXMap a = M.Map DXIdx a

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
                     dxMap :: DXMap a,
                     varMap :: VarMap a,
                     storageMap :: StorageMap a } deriving (Show)


instance (Show a) => Show (a -> a) where
         show _ = "<function (a -> a)>"

instance Eq (a -> a) where
         _ == _ = True

instance Ord (a -> a) where
         compare _ _ = EQ

emptyEnv :: Envs a
emptyEnv = Envs NoRecord M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty


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
                       dxMap = M.unions $ map dxMap envs,
                       varMap = M.unions $ map varMap envs,
                       storageMap = M.unions $ map storageMap envs }


separateEnvs :: Envs a -> [Envs a]
separateEnvs envs =
   case recordNumber envs of
      MixedRecord lst -> map f (L.sort lst)
      _ -> error "separateEnvs: no mixed env"
  where p n k _ = n == getIdxRecNum k
        f n = emptyEnv { recordNumber = SingleRecord n,
                         energyMap = M.filterWithKey (p n) (energyMap envs),
                         denergyMap = M.filterWithKey (p n) (denergyMap envs),
                         powerMap = M.filterWithKey (p n) (powerMap envs),
                         dpowerMap = M.filterWithKey (p n) (dpowerMap envs),
                         fetaMap = M.filterWithKey (p n) (fetaMap envs),
                         detaMap = M.filterWithKey (p n) (detaMap envs),
                         dtimeMap = M.filterWithKey (p n) (dtimeMap envs),
                         xMap = M.filterWithKey (p n) (xMap envs),
                         dxMap = M.filterWithKey (p n) (dxMap envs),
                         varMap = M.filterWithKey (p n) (varMap envs),
                         storageMap = M.filterWithKey (p n) (storageMap envs) }

checkEnvsForDelta :: Envs a -> Envs a -> Bool
checkEnvsForDelta env fnv =
   and [ check env fnv energyMap,
         check env fnv powerMap,
         check env fnv fetaMap,
         check env fnv xMap,
         check env fnv storageMap ]
   where check :: IdxEq idx => Envs a -> Envs a -> (Envs a -> M.Map idx c) -> Bool
         check x y f = SV.equalBy ignoreRecEq (M.keys $ f x) (M.keys $ f y)

minusEnv ::
   (S.Arith s s ~ s, TSum t t t, D.ZipWith c, D.Storage c a, BSum a) =>
   Envs (TC s t (Data c a)) ->
   Envs (TC s t (Data c a)) ->
   Envs (TC s t (Data c a))
minusEnv laterEnv formerEnv | checkEnvsForDelta laterEnv formerEnv = gnv
  where minus x y = M.fromList $ zipWith minush (M.toList x) (M.toList y)
        minush (k0, x) (k1, y) =
           (if ignoreRecEq k0 k1 then k0 else error "minush", x .- y)

        fminus x y = M.fromList $ zipWith fminush (M.toList x) (M.toList y)
        fminush (k0, fx) (k1, fy) =
           (if ignoreRecEq k0 k1 then k0 else error "fminush", \z -> fx z .- fy z)
{-
  where minus x y = M.intersectionWith (.-) x y
        fminus = M.intersectionWith (\fx fy z -> fx z .- fy z)
-}

        edk (EnergyIdx a b c d) = DEnergyIdx a b c d
        pdk (PowerIdx a b c d) = DPowerIdx a b c d
        etadk (FEtaIdx a b c d) = DEtaIdx a b c d
        xdk (XIdx a b c d) = DXIdx a b c d

        gnv = laterEnv { denergyMap = M.mapKeys edk $ energyMap laterEnv `minus` energyMap formerEnv,
                         dpowerMap = M.mapKeys pdk $ powerMap laterEnv `minus` powerMap formerEnv,
                         dxMap = M.mapKeys xdk $ xMap laterEnv `minus` xMap formerEnv,
                         detaMap = M.mapKeys etadk $ fetaMap laterEnv `fminus` fetaMap formerEnv }



mapEnv ::
   (D.Map c, D.Storage c a, D.Storage c b) =>
   (a -> b) -> Envs (TC s t (Data c a)) -> Envs (TC s t (Data c b))
mapEnv f env = emptyEnv { recordNumber = recordNumber env,
                          energyMap = M.map (S.map f) (energyMap env),
                          denergyMap = M.map (S.map f) (denergyMap env),
                          powerMap = M.map (S.map f) (powerMap env),
                          dpowerMap = M.map (S.map f) (dpowerMap env),
                          --fetaMap = M.map (S.map f .) (fetaMap env),
                          --detaMap = M.map (S.map f .) (detaMap env),
                          dtimeMap = M.map (S.map f) (dtimeMap env),
                          xMap = M.map (S.map f) (xMap env),
                          dxMap = M.map (S.map f) (dxMap env),
                          varMap = M.map (S.map f) (varMap env),
                          storageMap = M.map (S.map f) (storageMap env) }

