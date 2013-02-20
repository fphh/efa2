module EFA.Equation.Env where

import qualified EFA.Graph.Topology.Index as Idx

import qualified Data.Map as M

import qualified Data.Accessor.Basic as Accessor
import Control.Applicative (Applicative, pure, (<*>))
import Data.Traversable (Traversable, sequenceA, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Monoid (Monoid, mempty, mappend)


-- Environments
type EnergyMap rec node a = M.Map (Idx.Energy rec node) a
type DEnergyMap rec node a = M.Map (Idx.DEnergy rec node) a

type MaxEnergyMap rec node a = M.Map (Idx.MaxEnergy rec node) a
type DMaxEnergyMap rec node a = M.Map (Idx.DMaxEnergy rec node) a

type PowerMap rec node a = M.Map (Idx.Power rec node) a
type DPowerMap rec node a = M.Map (Idx.DPower rec node) a

type EtaMap rec node a = M.Map (Idx.Eta rec node) a
type DEtaMap rec node a = M.Map (Idx.DEta rec node) a

type DTimeMap rec node a = M.Map (Idx.DTime rec node) a

type XMap rec node a = M.Map (Idx.X rec node) a
type DXMap rec node a = M.Map (Idx.DX rec node) a

type YMap rec node a = M.Map (Idx.Y rec node) a
type DYMap rec node a = M.Map (Idx.DY rec node) a

type SumMap rec node a = M.Map (Idx.Sum rec node) a

type StorageMap rec node a = M.Map (Idx.Storage rec node) a



data Env rec node a =
               Env { energyMap :: EnergyMap rec node a,
                     denergyMap :: DEnergyMap rec node a,
                     maxenergyMap :: MaxEnergyMap rec node a,
                     dmaxenergyMap :: DMaxEnergyMap rec node a,

                     powerMap :: PowerMap rec node a,
                     dpowerMap :: DPowerMap rec node a,
                     etaMap :: EtaMap rec node a,
                     detaMap :: DEtaMap rec node a,
                     dtimeMap :: DTimeMap rec node a,
                     xMap :: XMap rec node a,
                     dxMap :: DXMap rec node a,
                     yMap :: YMap rec node a,
                     dyMap :: DYMap rec node a,
                     sumMap :: SumMap rec node a,
                     {-
                     If 'a' is a signal type,
                     then storages must still be scalar values.
                     Maybe we should move the storageMap to another data type
                     in order to maintain the Functor instances.
                     -}
                     storageMap :: StorageMap rec node a } deriving (Show)


class AccessMap idx where
   accessMap :: Accessor.T (Env rec node a) (M.Map (idx rec node) a)

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


instance Functor (Env rec node) where
   fmap f (Env e de me dme p dp n dn dt x dx y dy v st) =
      Env (fmap f e) (fmap f de) (fmap f me) (fmap f dme) (fmap f p) (fmap f dp) (fmap f n) (fmap f dn) (fmap f dt) (fmap f x) (fmap f dx) (fmap f y) (fmap f dy) (fmap f v) (fmap f st)

instance Foldable (Env rec node) where
   foldMap = foldMapDefault

instance Traversable (Env rec node) where
   sequenceA (Env e de me dme p dp n dn dt x dx y dy v st) =
      pure Env <?> e <?> de <?> me <?> dme <?> p <?> dp <?> n <?> dn <?> dt <?> x <?> dx <?> y <?> dy <?> v <?> st

infixl 4 <?>
(<?>) ::
   (Applicative f, Traversable map) =>
   f (map a -> b) -> map (f a) -> f b
f <?> x = f <*> sequenceA x



instance (Ord node, Ord rec) => Monoid (Env rec node a) where
   mempty = Env M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty
   mappend
         (Env e de me dme p dp n dn dt x dx y dy v st)
         (Env e' de' me' dme' p' dp' n' dn' dt' x' dx' y' dy' v' st') =
       Env (M.union e e') (M.union de de') (M.union me me') (M.union dme dme')
           (M.union p p') (M.union dp dp') (M.union n n')   (M.union dn dn')
           (M.union dt dt') (M.union x x') (M.union dx dx') (M.union y y')
           (M.union dy dy') (M.union v v') (M.union st st')
