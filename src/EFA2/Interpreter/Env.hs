module EFA2.Interpreter.Env where

import qualified EFA2.Signal.Index as Idx

import qualified Data.Map as M

import qualified Data.Accessor.Basic as Accessor
import Control.Applicative (Applicative, pure, (<*>))
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)


data Index =
            Energy Idx.Energy
          | DEnergy Idx.DEnergy
          | MaxEnergy Idx.MaxEnergy
          | DMaxEnergy Idx.DMaxEnergy
          | Power Idx.Power
          | DPower Idx.DPower
          | Eta Idx.Eta
          | DEta Idx.DEta
          | DTime Idx.DTime
          | X Idx.X
          | DX Idx.DX
          | Y Idx.Y
          | DY Idx.DY
          | Var Idx.Var
          | Store Idx.Storage
            deriving (Show, Eq, Ord)


class IdxRecNum a where
   getIdxRecNum :: a -> Idx.Record
   setIdxRecNum :: Idx.Record -> a -> a

instance IdxRecNum Idx.Energy where
   getIdxRecNum (Idx.Energy r _ _) = r
   setIdxRecNum rec (Idx.Energy _ f t) = Idx.Energy rec f t

instance IdxRecNum Idx.DEnergy where
   getIdxRecNum (Idx.DEnergy r _ _) = r
   setIdxRecNum rec (Idx.DEnergy _ f t) = Idx.DEnergy rec f t

instance IdxRecNum Idx.Power where
   getIdxRecNum (Idx.Power r _ _) = r
   setIdxRecNum rec (Idx.Power _ f t) = Idx.Power rec f t

instance IdxRecNum Idx.DPower where
   getIdxRecNum (Idx.DPower r _ _) = r
   setIdxRecNum rec (Idx.DPower _ f t) = Idx.DPower rec f t

instance IdxRecNum Idx.Eta where
   getIdxRecNum (Idx.Eta r _ _) = r
   setIdxRecNum rec (Idx.Eta _ f t) = Idx.Eta rec f t

instance IdxRecNum Idx.DEta where
   getIdxRecNum (Idx.DEta r _ _) = r
   setIdxRecNum rec (Idx.DEta _ f t) = Idx.DEta rec f t

instance IdxRecNum Idx.X where
   getIdxRecNum (Idx.X r _ _) = r
   setIdxRecNum rec (Idx.X _ f t) = Idx.X rec f t

instance IdxRecNum Idx.DX where
   getIdxRecNum (Idx.DX r _ _) = r
   setIdxRecNum rec (Idx.DX _ f t) = Idx.DX rec f t

instance IdxRecNum Idx.DTime where
   getIdxRecNum (Idx.DTime r _) = r
   setIdxRecNum rec (Idx.DTime _ s) = Idx.DTime rec s

instance IdxRecNum Idx.Storage where
   getIdxRecNum (Idx.Storage r _) = r
   setIdxRecNum rec (Idx.Storage _ n) = Idx.Storage rec n

instance IdxRecNum Idx.Var where
   getIdxRecNum (Idx.Var r _ _) = r
   setIdxRecNum rec (Idx.Var _ use t) = Idx.Var rec use t


class IdxEq a where
   ignoreRecEq :: a -> a -> Bool

instance IdxEq Idx.Power where
   ignoreRecEq (Idx.Power _ a b) (Idx.Power _ x y) = a == x && b == y

instance IdxEq Idx.Energy where
   ignoreRecEq (Idx.Energy _ a b) (Idx.Energy _ x y) = a == x && b == y

instance IdxEq Idx.Eta where
   ignoreRecEq (Idx.Eta _ a b) (Idx.Eta _ x y) = a == x && b == y

instance IdxEq Idx.X where
   ignoreRecEq (Idx.X _ a b) (Idx.X _ x y) = a == x && b == y

instance IdxEq Idx.Storage where
   ignoreRecEq (Idx.Storage _ a) (Idx.Storage _ x) = a == x

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
