{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Storage.Quantity where

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


class (Applicative f, Foldable f) => Carry f where
   carryEnergy, carryXOut, carryXIn :: f a -> a

   type CarryPart f :: *
   carryVars ::
      (CarryPart f ~ part) =>
      f (Idx.StorageEdge part node -> Var.Scalar part node)


mapGraphWithVar ::
   (Ord node, Carry carry, CarryPart carry ~ part, Format.Part part) =>
   (Idx.PartNode part node -> Maybe (FlowTopo.Sums v)) ->
   (Var.ForNodeScalar part node -> a0 -> a1) ->
   node ->
   Storage.Graph part node a0 (carry a0) ->
   Storage.Graph part node a1 (carry a1)
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
   (Var.ForNodeScalar part node -> a0 -> a1) ->
   node -> Idx.StorageEdge part node -> carry a0 -> carry a1
mapCarryWithVar f node edge =
   liftA2 f (Idx.ForNode <$> (carryVars <*> pure edge) <*> pure node)


mapGraph ::
   (Ord part, Functor carry) =>
   (a -> b) ->
   Storage.Graph part node a (carry a) ->
   Storage.Graph part node b (carry b)
mapGraph f =
   Storage.mapNode f . Storage.mapEdge (fmap f)

traverseGraph ::
   (Ord part, Applicative f, Trav.Traversable carry) =>
   (a -> f b) ->
   Storage.Graph part node a (carry a) ->
   f (Storage.Graph part node b (carry b))
traverseGraph f =
   Storage.traverse f (Trav.traverse f)


forwardEdgesFromSums ::
   (Ord part) =>
   Map part (FlowTopo.Sums v) -> [Idx.StorageEdge part node]
forwardEdgesFromSums stores = do
   let ins  = Map.mapMaybe FlowTopo.sumIn stores
   let outs = Map.mapMaybe FlowTopo.sumOut stores
   secin <- Idx.Init : map Idx.NoInit (Map.keys ins)
   secout <-
      (++[Idx.Exit]) $ map Idx.NoExit $ Map.keys $
      case secin of
         Idx.Init -> outs
         Idx.NoInit s -> snd $ Map.split s outs
   return $ Idx.StorageEdge secin secout

allEdgesFromSums ::
   (Ord part) =>
   Map part (FlowTopo.Sums a) -> [Idx.StorageEdge part node]
allEdgesFromSums stores =
   liftA2 Idx.StorageEdge
      (Idx.Init : map Idx.NoInit (Map.keys (Map.mapMaybe FlowTopo.sumIn stores)))
      (Idx.Exit : map Idx.NoExit (Map.keys (Map.mapMaybe FlowTopo.sumOut stores)))

graphFromList ::
   (Ord part, Ord node, Carry carry, Unknown a) =>
   [part] ->
   [Idx.StorageEdge part node] ->
   Storage.Graph part node a (carry a)
graphFromList sts edges =
   Storage.Graph
      (PartMap.constant unknown sts)
      (Map.fromListWith (error "duplicate storage edge") $
       map (flip (,) (pure unknown)) edges)
