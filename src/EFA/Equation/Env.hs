{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module EFA.Equation.Env where

import qualified EFA.Graph.Topology.Index as Idx

import qualified Data.Map as M

import qualified Data.Accessor.Basic as Accessor
import Control.Applicative (Applicative, pure, (<*>))
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)


-- Environments
type EnergyMap nty a = M.Map (Idx.Energy nty) a
type DEnergyMap nty a = M.Map (Idx.DEnergy nty) a

type MaxEnergyMap nty a = M.Map (Idx.MaxEnergy nty) a
type DMaxEnergyMap nty a = M.Map (Idx.DMaxEnergy nty) a

type PowerMap nty a = M.Map (Idx.Power nty) a
type DPowerMap nty a = M.Map (Idx.DPower nty) a

type EtaMap nty a = M.Map (Idx.Eta nty) a
type DEtaMap nty a = M.Map (Idx.DEta nty) a

type DTimeMap nty a = M.Map (Idx.DTime nty) a

type XMap nty a = M.Map (Idx.X nty) a
type DXMap nty a = M.Map (Idx.DX nty) a

type YMap nty a = M.Map (Idx.Y nty) a
type DYMap nty a = M.Map (Idx.DY nty) a

type VarMap nty a = M.Map (Idx.Var nty) a

type StorageMap nty a = M.Map (Idx.Storage nty) a



data Env nty rec a =
               Env { recordNumber :: rec,
                     energyMap :: EnergyMap nty a,
                     denergyMap :: DEnergyMap nty a,
                     maxenergyMap :: MaxEnergyMap nty a,
                     dmaxenergyMap :: DMaxEnergyMap nty a,

                     powerMap :: PowerMap nty a,
                     dpowerMap :: DPowerMap nty a,
                     etaMap :: EtaMap nty a,
                     detaMap :: DEtaMap nty a,
                     dtimeMap :: DTimeMap nty a,
                     xMap :: XMap nty a,
                     dxMap :: DXMap nty a,
                     yMap :: YMap nty a,
                     dyMap :: DYMap nty a,
                     varMap :: VarMap nty a,
                     storageMap :: StorageMap nty a } deriving (Show)


class AccessMap idx where
   accessMap :: Accessor.T (Env nty rec a) (M.Map (idx nty) a)

instance AccessMap Idx.Energy where
   accessMap =
      Accessor.fromSetGet (\x c -> c{energyMap = x}) energyMap


instance AccessMap Idx.DEnergy where
   accessMap =
      Accessor.fromSetGet (\x c -> c{denergyMap = x}) denergyMap

instance AccessMap Idx.MaxEnergy where
   accessMap =
      Accessor.fromSetGet (\x c -> c{maxenergyMap = x}) maxenergyMap

instance AccessMap Idx.DMaxEnergy where
   accessMap =
      Accessor.fromSetGet (\x c -> c{dmaxenergyMap = x}) dmaxenergyMap

instance AccessMap Idx.Power where
   accessMap =
      Accessor.fromSetGet (\x c -> c{powerMap = x}) powerMap

instance AccessMap Idx.DPower where
   accessMap =
      Accessor.fromSetGet (\x c -> c{dpowerMap = x}) dpowerMap

instance AccessMap Idx.Eta where
   accessMap =
      Accessor.fromSetGet (\x c -> c{etaMap = x}) etaMap

instance AccessMap Idx.DEta where
   accessMap =
      Accessor.fromSetGet (\x c -> c{detaMap = x}) detaMap

instance AccessMap Idx.DTime where
   accessMap =
      Accessor.fromSetGet (\x c -> c{dtimeMap = x}) dtimeMap

instance AccessMap Idx.X where
   accessMap =
      Accessor.fromSetGet (\x c -> c{xMap = x}) xMap

instance AccessMap Idx.DX where
   accessMap =
      Accessor.fromSetGet (\x c -> c{dxMap = x}) dxMap

instance AccessMap Idx.Y where
   accessMap =
      Accessor.fromSetGet (\x c -> c{yMap = x}) yMap

instance AccessMap Idx.DY where
   accessMap =
      Accessor.fromSetGet (\x c -> c{dyMap = x}) dyMap

instance AccessMap Idx.Var where
   accessMap =
      Accessor.fromSetGet (\x c -> c{varMap = x}) varMap

instance AccessMap Idx.Storage where
   accessMap =
      Accessor.fromSetGet (\x c -> c{storageMap = x}) storageMap


instance Functor (Env nty rec) where
         fmap f (Env rec e de me dme p dp n dn dt x dx y dy v st) =
           Env rec (fmap f e) (fmap f de) (fmap f me) (fmap f dme) (fmap f p) (fmap f dp) (fmap f n) (fmap f dn) (fmap f dt) (fmap f x) (fmap f dx) (fmap f y) (fmap f dy) (fmap f v) (fmap f st)

instance Foldable (Env nty rec) where
   foldMap = foldMapDefault

instance Traversable (Env nty rec) where
   sequenceA (Env rec e de  me dme p dp n dn dt x dx y dy v st) =
      pure (Env rec) <?> e <?> de <?> me <?> dme <?> p <?> dp <?> n <?> dn <?> dt <?> x <?> dx <?> y <?> dy <?> v <?> st

infixl 4 <?>
(<?>) ::
   (Applicative f, Traversable map) =>
   f (map a -> b) -> map (f a) -> f b
f <?> x = f <*> sequenceA x


empty :: rec -> Env nty rec a
empty rec = Env rec M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty


data NoRecord = NoRecord deriving (Eq, Ord, Show)
newtype SingleRecord = SingleRecord {fromSingleRecord :: Idx.Record} deriving (Eq, Ord, Show)
newtype MixedRecord = MixedRecord {fromMixedRecord :: [Idx.Record]} deriving (Eq, Ord, Show)


class RecordNumber rec where
   uniteRecordNumbers :: [rec] -> MixedRecord

instance RecordNumber SingleRecord where
   uniteRecordNumbers =
      MixedRecord . map fromSingleRecord

instance RecordNumber MixedRecord where
   uniteRecordNumbers =
      MixedRecord . concatMap fromMixedRecord
