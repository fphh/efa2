module EFA.Flow.Part.Map where

import qualified EFA.Flow.Storage.Index as StorageIdx
import qualified EFA.Flow.Part.Index as PartIdx
import qualified EFA.Graph.Topology as Topo

import qualified EFA.Flow.SequenceState.Variable as Var
import qualified EFA.Flow.SequenceState.Index as Idx
import EFA.Flow.SequenceState.Variable ((<#>))

import qualified EFA.Utility.Map as MapU
import EFA.Utility.Map (Caller)

import Control.Applicative (Applicative, pure, (<*>))

import qualified Data.Map as Map
import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable, foldMap)
import Data.Map (Map)


data PartMap part a =
   PartMap {
      init, exit :: a,
      parts :: Map part a
   }
   deriving (Eq)

constant ::
   (Ord part) =>
   a -> [part] -> PartMap part a
constant a ps =
   PartMap a a $ Map.fromList $ map (flip (,) a) ps

lookup :: (Ord part) => part -> PartMap part a -> Maybe a
lookup p = Map.lookup p . parts

checkedZipWith ::
   (Ord part) =>
   Caller ->
   (a -> b -> c) ->
   PartMap part a -> PartMap part b -> PartMap part c
checkedZipWith caller f (PartMap ia ea psa) (PartMap ib eb psb) =
   PartMap (f ia ib) (f ea eb) (MapU.checkedZipWith caller f psa psb)

mapKeysWith ::
   (Ord part1) =>
   (a -> a -> a) ->
   (part0 -> part1) ->
   PartMap part0 a -> PartMap part1 a
mapKeysWith f g (PartMap i e ps) =
   PartMap i e $ Map.mapKeysWith f g ps


instance (Ord part) => Functor (PartMap part) where
   fmap f (PartMap i e ps) = PartMap (f i) (f e) (fmap f ps)

instance (Ord part) => Foldable (PartMap part) where
   foldMap = foldMapDefault

instance (Ord part) => Traversable (PartMap part) where
   traverse f (PartMap i e ps) =
      pure PartMap <*> f i <*> f e <*> traverse f ps



mapWithVar ::
   (PartIdx.Format sec,Show sec, Show node) =>
   (Idx.PartNode sec node -> Maybe Topo.StoreDir) ->
   (Var.Scalar sec node -> a0 -> a1) ->
   node ->
   PartMap sec a0 ->
   PartMap sec a1
mapWithVar lookupDir f node (PartMap i e ps) =
   PartMap
      (f (StorageIdx.OutSum Idx.Init <#> node) i)
      (f (StorageIdx.InSum  Idx.Exit <#> node) e)
      (Map.mapWithKey
          (\part a ->
             case lookupDir (Idx.PartNode part node) of
                Nothing -> error $ "PartMap.mapWithVar - inactive Part: " ++ show part ++ " Node: "++ show node  
                Just Topo.In  -> f (StorageIdx.OutSum (Idx.NoInit part) <#> node) a
                Just Topo.Out -> f (StorageIdx.InSum  (Idx.NoExit part) <#> node) a)
          ps)
