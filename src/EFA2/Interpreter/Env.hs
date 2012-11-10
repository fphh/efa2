{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module EFA2.Interpreter.Env where

import qualified Data.Map as M
import qualified Data.List as L

import qualified EFA2.Signal.Index as Idx
import qualified EFA2.Signal.Signal as S
import qualified EFA2.Signal.Data as D
import qualified EFA2.Signal.Vector as SV
import EFA2.Signal.Signal (TC, (.-))
import EFA2.Signal.Data (Data)
import EFA2.Signal.Typ (TSum)
import EFA2.Signal.Base (BSum)


data Index =
            Energy Idx.Energy
          | DEnergy Idx.DEnergy
          | Power Idx.Power
          | DPower Idx.DPower
          | FEta Idx.FEta
          | DEta Idx.DEta
          | DTime Idx.DTime
          | X Idx.X
          | DX Idx.DX
          | Var Idx.Var
          | Store Idx.Storage
             deriving (Show, Eq, Ord)


class IdxRecNum a where
      getIdxRecNum :: a -> Idx.Record
      setIdxRecNum :: Idx.Record -> a -> a

instance IdxRecNum Idx.Energy where
         getIdxRecNum (Idx.Energy _ r _ _) = r
         setIdxRecNum rec (Idx.Energy s _ f t) = Idx.Energy s rec f t

instance IdxRecNum Idx.DEnergy where
         getIdxRecNum (Idx.DEnergy _ r _ _) = r
         setIdxRecNum rec (Idx.DEnergy s _ f t) = Idx.DEnergy s rec f t

instance IdxRecNum Idx.Power where
         getIdxRecNum (Idx.Power _ r _ _) = r
         setIdxRecNum rec (Idx.Power s _ f t) = Idx.Power s rec f t

instance IdxRecNum Idx.DPower where
         getIdxRecNum (Idx.DPower _ r _ _) = r
         setIdxRecNum rec (Idx.DPower s _ f t) = Idx.DPower s rec f t

instance IdxRecNum Idx.FEta where
         getIdxRecNum (Idx.FEta _ r _ _) = r
         setIdxRecNum rec (Idx.FEta s _ f t) = Idx.FEta s rec f t

instance IdxRecNum Idx.DEta where
         getIdxRecNum (Idx.DEta _ r _ _) = r
         setIdxRecNum rec (Idx.DEta s _ f t) = Idx.DEta s rec f t

instance IdxRecNum Idx.X where
         getIdxRecNum (Idx.X _ r _ _) = r
         setIdxRecNum rec (Idx.X s _ f t) = Idx.X s rec f t

instance IdxRecNum Idx.DX where
         getIdxRecNum (Idx.DX _ r _ _) = r
         setIdxRecNum rec (Idx.DX s _ f t) = Idx.DX s rec f t

instance IdxRecNum Idx.DTime where
         getIdxRecNum (Idx.DTime _ r) = r
         setIdxRecNum rec (Idx.DTime s _) = Idx.DTime s rec

instance IdxRecNum Idx.Storage where
         getIdxRecNum (Idx.Storage _ r _) = r
         setIdxRecNum rec (Idx.Storage s _ sto) = Idx.Storage s rec sto

instance IdxRecNum Idx.Var where
         getIdxRecNum (Idx.Var _ r _ _) = r
         setIdxRecNum rec (Idx.Var s _ use t) = Idx.Var s rec use t


class IdxEq a where
      ignoreRecEq :: a -> a -> Bool

instance IdxEq Idx.Power where
         ignoreRecEq (Idx.Power a _ b c) (Idx.Power x _ y z) = a == x && b == y && c == z

instance IdxEq Idx.Energy where
         ignoreRecEq (Idx.Energy a _ b c) (Idx.Energy x _ y z) = a == x && b == y && c == z

instance IdxEq Idx.FEta where
         ignoreRecEq (Idx.FEta a _ b c) (Idx.FEta x _ y z) = a == x && b == y && c == z

instance IdxEq Idx.X where
         ignoreRecEq (Idx.X a _ b c) (Idx.X x _ y z) = a == x && b == y && c == z

instance IdxEq Idx.Storage where
         ignoreRecEq (Idx.Storage a _ b) (Idx.Storage x _ y) = a == x && b == y

-- Environments
type EnergyMap a = M.Map Idx.Energy a
type DEnergyMap a = M.Map Idx.DEnergy a

type PowerMap a = M.Map Idx.Power a
type DPowerMap a = M.Map Idx.DPower a

type FEtaMap a = M.Map Idx.FEta (a -> a)
type DEtaMap a = M.Map Idx.DEta (a -> a)

type DTimeMap a = M.Map Idx.DTime a

type XMap a = M.Map Idx.X a
type DXMap a = M.Map Idx.DX a

type VarMap a = M.Map Idx.Var a
type StorageMap a = M.Map Idx.Storage a



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
                  | SingleRecord Idx.Record
                  | MixedRecord [Idx.Record] deriving (Eq, Ord, Show)

isSingleRecord :: RecordNumber -> Bool
isSingleRecord (SingleRecord _) = True
isSingleRecord _ = False

fromSingleRecord :: RecordNumber -> Idx.Record
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

        edk (Idx.Energy a b c d) = Idx.DEnergy a b c d
        pdk (Idx.Power a b c d) = Idx.DPower a b c d
        etadk (Idx.FEta a b c d) = Idx.DEta a b c d
        xdk (Idx.X a b c d) = Idx.DX a b c d

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

