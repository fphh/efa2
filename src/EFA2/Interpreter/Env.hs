module EFA2.Interpreter.Env where

import qualified EFA2.Signal.Index as Idx

import qualified Data.Map as M

import qualified Data.Accessor.Basic as Accessor
import Control.Applicative (Applicative, pure, (<*>))
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)


-- Environments
type EnergyMap a = M.Map Idx.Energy a
type DEnergyMap a = M.Map Idx.DEnergy a

type MaxEnergyMap a = M.Map Idx.MaxEnergy a
type DMaxEnergyMap a = M.Map Idx.DMaxEnergy a

type PowerMap a = M.Map Idx.Power a
type DPowerMap a = M.Map Idx.DPower a

type EtaMap a = M.Map Idx.Eta a
type DEtaMap a = M.Map Idx.DEta a

type DTimeMap a = M.Map Idx.DTime a

type XMap a = M.Map Idx.X a
type DXMap a = M.Map Idx.DX a

type YMap a = M.Map Idx.Y a
type DYMap a = M.Map Idx.DY a

type VarMap a = M.Map Idx.Var a

type StorageMap a = M.Map Idx.Storage a



data Env rec a =
               Env { recordNumber :: rec,
                     energyMap :: EnergyMap a,
                     denergyMap :: DEnergyMap a,
                     maxenergyMap :: MaxEnergyMap a,
                     dmaxenergyMap :: DMaxEnergyMap a,

                     powerMap :: PowerMap a,
                     dpowerMap :: DPowerMap a,
                     etaMap :: EtaMap a,
                     detaMap :: DEtaMap a,
                     dtimeMap :: DTimeMap a,
                     xMap :: XMap a,
                     dxMap :: DXMap a,
                     yMap :: YMap a,
                     dyMap :: DYMap a,
                     varMap :: VarMap a,
                     storageMap :: StorageMap a } deriving (Show)


class Ord idx => AccessMap idx where
   accessMap :: Accessor.T (Env rec a) (M.Map idx a)

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


instance Functor (Env rec) where
         fmap f (Env rec e de me dme p dp n dn dt x dx y dy v st) =
           Env rec (fmap f e) (fmap f de) (fmap f me) (fmap f dme) (fmap f p) (fmap f dp) (fmap f n) (fmap f dn) (fmap f dt) (fmap f x) (fmap f dx) (fmap f y) (fmap f dy) (fmap f v) (fmap f st)

instance Foldable (Env rec) where
   foldMap = foldMapDefault

instance Traversable (Env rec) where
   sequenceA (Env rec e de me dme p dp n dn dt x dx y dy v st) =
      pure (Env rec) <?> e <?> de <?> me <?> dme <?> p <?> dp <?> n <?> dn <?> dt <?> x <?> dx <?> y <?> dy <?> v <?> st

infixl 4 <?>
(<?>) ::
   (Applicative f, Traversable map) =>
   f (map a -> b) -> map (f a) -> f b
f <?> x = f <*> sequenceA x


empty :: rec -> Env rec a
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
