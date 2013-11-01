{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Storage.Quantity where

import qualified EFA.Flow.Storage.Variable as StorageVar
import qualified EFA.Flow.Storage as Storage

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.PartMap as PartMap

import qualified EFA.Equation.Variable as Var
import EFA.Equation.Unknown (Unknown(unknown))

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Report.Format as Format

import qualified Data.Traversable as Trav
import qualified Data.Map as Map ; import Data.Map (Map)

import Control.Applicative (Applicative, pure, liftA2, (<*>), (<$>))
import Data.Foldable (Foldable)


type Graph carry a = Storage.Graph (CarryPart carry) a (carry a)

class (Applicative f, Foldable f) => Carry f where
   carryEnergy, carryXOut, carryXIn :: f a -> a

   type CarryPart f :: *
   carryVars ::
      (CarryPart f ~ part) =>
      f (Idx.CarryEdge part -> StorageVar.Scalar part)


mapGraphWithVar ::
   (Carry carry, CarryPart carry ~ part, Format.Part part) =>
   (Idx.PartNode part node -> Maybe (FlowTopo.Sums v)) ->
   (Var.ForStorageScalar part node -> a0 -> a1) ->
   node ->
   Graph carry a0 ->
   Graph carry a1
mapGraphWithVar lookupSums f node (Storage.Graph partMap edges) =
   Storage.Graph
      (PartMap.mapWithVar
          (maybe
              (error "mapStoragesWithVar: missing corresponding sum")
              FlowTopo.dirFromSums .
           lookupSums)
          f node partMap)
      (Map.mapWithKey (mapCarryWithVar f node) edges)

mapCarryWithVar ::
   (Carry carry, CarryPart carry ~ part) =>
   (Var.ForStorageScalar part node -> a0 -> a1) ->
   node -> Idx.CarryEdge part -> carry a0 -> carry a1
mapCarryWithVar f node edge =
   liftA2 f (Idx.ForStorage <$> (carryVars <*> pure edge) <*> pure node)


mapGraph ::
   (Functor carry, CarryPart carry ~ part, Ord part) =>
   (a0 -> a1) ->
   Graph carry a0 -> Graph carry a1
mapGraph f =
   Storage.mapNode f . Storage.mapEdge (fmap f)

traverseGraph ::
   (Applicative f, Trav.Traversable carry, CarryPart carry ~ part, Ord part) =>
   (a0 -> f a1) ->
   Graph carry a0 -> f (Graph carry a1)
traverseGraph f =
   Storage.traverse f (Trav.traverse f)


forwardEdgesFromSums ::
   (Ord part) =>
   Map part (FlowTopo.Sums v) -> [Idx.CarryEdge part]
forwardEdgesFromSums stores = do
   let ins  = Map.mapMaybe FlowTopo.sumIn stores
   let outs = Map.mapMaybe FlowTopo.sumOut stores
   secin <- Idx.Init : map Idx.NoInit (Map.keys ins)
   secout <-
      (++[Idx.Exit]) $ map Idx.NoExit $ Map.keys $
      case secin of
         Idx.Init -> outs
         Idx.NoInit s -> snd $ Map.split s outs
   return $ Idx.CarryEdge secin secout

allEdgesFromSums ::
   (Ord part) =>
   Map part (FlowTopo.Sums a) -> [Idx.CarryEdge part]
allEdgesFromSums stores =
   liftA2 Idx.CarryEdge
      (Idx.Init : map Idx.NoInit (Map.keys (Map.mapMaybe FlowTopo.sumIn stores)))
      (Idx.Exit : map Idx.NoExit (Map.keys (Map.mapMaybe FlowTopo.sumOut stores)))

graphFromList ::
   (Carry carry, CarryPart carry ~ part, Ord part, Unknown a) =>
   [part] ->
   [Idx.CarryEdge part] ->
   Graph carry a
graphFromList sts edges =
   Storage.Graph
      (PartMap.constant unknown sts)
      (Map.fromListWith (error "duplicate storage edge") $
       map (flip (,) (pure unknown)) edges)
