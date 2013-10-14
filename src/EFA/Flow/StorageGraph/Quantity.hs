{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.StorageGraph.Quantity where

import EFA.Flow.StorageGraph (StorageGraph(StorageGraph))

import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.PartMap as PartMap

import qualified EFA.Equation.Variable as Var

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Report.Format as Format

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
   StorageGraph part node a0 (carry a0) ->
   StorageGraph part node a1 (carry a1)
mapGraphWithVar lookupSums f node (StorageGraph partMap edges) =
   StorageGraph
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
