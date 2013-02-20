module EFA.Equation.Env where

import qualified EFA.Graph.Topology.Index as Idx

import qualified Data.Map as M

import qualified Data.Accessor.Basic as Accessor
import Control.Applicative (Applicative, pure, (<*>))
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid, mempty, mappend)


-- Environments
type EnergyMap node a = M.Map (Idx.Energy node) a
type DEnergyMap node a = M.Map (Idx.DEnergy node) a

type MaxEnergyMap node a = M.Map (Idx.MaxEnergy node) a
type DMaxEnergyMap node a = M.Map (Idx.DMaxEnergy node) a

type PowerMap node a = M.Map (Idx.Power node) a
type DPowerMap node a = M.Map (Idx.DPower node) a

type EtaMap node a = M.Map (Idx.Eta node) a
type DEtaMap node a = M.Map (Idx.DEta node) a

type DTimeMap node a = M.Map (Idx.DTime node) a

type XMap node a = M.Map (Idx.X node) a
type DXMap node a = M.Map (Idx.DX node) a

type YMap node a = M.Map (Idx.Y node) a
type DYMap node a = M.Map (Idx.DY node) a

type SumMap node a = M.Map (Idx.Sum node) a

type StorageMap node a = M.Map (Idx.Storage node) a



data Env node a =
               Env { energyMap :: EnergyMap node a,
                     denergyMap :: DEnergyMap node a,
                     maxenergyMap :: MaxEnergyMap node a,
                     dmaxenergyMap :: DMaxEnergyMap node a,

                     powerMap :: PowerMap node a,
                     dpowerMap :: DPowerMap node a,
                     etaMap :: EtaMap node a,
                     detaMap :: DEtaMap node a,
                     dtimeMap :: DTimeMap node a,
                     xMap :: XMap node a,
                     dxMap :: DXMap node a,
                     yMap :: YMap node a,
                     dyMap :: DYMap node a,
                     sumMap :: SumMap node a,
                     storageMap :: StorageMap node a } deriving (Show)


class AccessMap idx where
   accessMap :: Accessor.T (Env node a) (M.Map (idx node) a)

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

instance AccessMap Idx.Sum where
   accessMap =
      Accessor.fromSetGet (\x c -> c{sumMap = x}) sumMap

instance AccessMap Idx.Storage where
   accessMap =
      Accessor.fromSetGet (\x c -> c{storageMap = x}) storageMap


instance Functor (Env node) where
   fmap f (Env e de me dme p dp n dn dt x dx y dy v st) =
      Env (fmap f e) (fmap f de) (fmap f me) (fmap f dme) (fmap f p) (fmap f dp) (fmap f n) (fmap f dn) (fmap f dt) (fmap f x) (fmap f dx) (fmap f y) (fmap f dy) (fmap f v) (fmap f st)

instance Foldable (Env node) where
   foldMap = foldMapDefault

instance Traversable (Env node) where
   sequenceA (Env e de me dme p dp n dn dt x dx y dy v st) =
      pure Env <?> e <?> de <?> me <?> dme <?> p <?> dp <?> n <?> dn <?> dt <?> x <?> dx <?> y <?> dy <?> v <?> st

infixl 4 <?>
(<?>) ::
   (Applicative f, Traversable map) =>
   f (map a -> b) -> map (f a) -> f b
f <?> x = f <*> sequenceA x



instance (Ord node) => Monoid (Env node a) where
   mempty = Env M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty
   mappend
         (Env e de me dme p dp n dn dt x dx y dy v st)
         (Env e' de' me' dme' p' dp' n' dn' dt' x' dx' y' dy' v' st') =
       Env (M.union e e') (M.union de de') (M.union me me') (M.union dme dme')
           (M.union p p') (M.union dp dp') (M.union n n')   (M.union dn dn')
           (M.union dt dt') (M.union x x') (M.union dx dx') (M.union y y')
           (M.union dy dy') (M.union v v') (M.union st st')
