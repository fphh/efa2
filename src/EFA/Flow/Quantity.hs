{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Quantity where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Graph; import EFA.Graph (Graph)

import qualified EFA.Equation.Variable as Var

import Control.Applicative (Applicative, pure, liftA2, (<*>), (<$>))

import qualified Data.Foldable as Fold

import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable)
import Data.Maybe (fromMaybe)

import Prelude hiding (lookup, init, sin, sum)


type
   DirTopology node a v =
      Graph node Graph.DirEdge (Sums v) (Flow v)

type
   Topology node v =
      Graph node Graph.EitherEdge (Sums v) (Maybe (Flow v))

data Flow v =
   Flow {
      flowPowerOut, flowEnergyOut, flowXOut,
      flowEta,
      flowXIn, flowEnergyIn, flowPowerIn :: v
   }
   deriving (Eq)

data Sums v =
   Sums { sumIn, sumOut :: Maybe v }
   deriving (Eq)


instance Functor Flow where
   fmap f (Flow pout eout xout eta xin ein pin) =
      Flow (f pout) (f eout) (f xout) (f eta) (f xin) (f ein) (f pin)

instance Foldable Flow where
   foldMap = foldMapDefault

instance Traversable Flow where
   traverse f (Flow pout eout xout eta xin ein pin) =
      pure Flow <*> f pout <*> f eout <*> f xout <*> f eta <*> f xin <*> f ein <*> f pin

instance Applicative Flow where
   pure a = Flow a a a a a a a
   Flow fpout feout fxout feta fxin fein fpin
         <*> Flow pout eout xout eta xin ein pin =
      Flow
         (fpout pout) (feout eout) (fxout xout)
         (feta eta) (fxin xin) (fein ein) (fpin pin)


instance Functor Sums where
   fmap f (Sums i o) = Sums (fmap f i) (fmap f o)

instance Foldable Sums where
   foldMap = foldMapDefault

instance Traversable Sums where
   traverse f (Sums i o) = liftA2 Sums (traverse f i) (traverse f o)

instance Applicative Sums where
   pure a = Sums (Just a) (Just a)
   (Sums fi fo) <*> (Sums i o) = Sums (fi <*> i) (fo <*> o)


mapSums ::
   (v0 -> v1) ->
   Sums v0 -> Sums v1
mapSums f s =
   Sums {
      sumIn  = fmap f $ sumIn  s,
      sumOut = fmap f $ sumOut s
   }


zipWithSums ::
   (v0 -> v1 -> v2) ->
   Sums v0 -> Sums v1 -> Sums v2
zipWithSums f s0 s1 =
   Sums {
      sumIn  = liftA2 f (sumIn  s0) (sumIn  s1),
      sumOut = liftA2 f (sumOut s0) (sumOut s1)
   }


traverseSums ::
   (Applicative f) =>
   (v0 -> f v1) ->
   Sums v0 -> f (Sums v1)
traverseSums f (Sums i o) =
   liftA2 Sums
      (traverse f i)
      (traverse f o)


liftEdgeFlow ::
   (Graph.DirEdge node -> flow0 -> flow1) ->
   Graph.EitherEdge node -> Maybe flow0 -> Maybe flow1
liftEdgeFlow f =
   switchEdgeFlow (const Nothing) (\edge flow -> Just $ f edge flow)

switchEdgeFlow ::
   (Graph.UnDirEdge node -> a) ->
   (Graph.DirEdge node -> flow -> a) ->
   Graph.EitherEdge node -> Maybe flow -> a
switchEdgeFlow f _ (Graph.EUnDirEdge edge) Nothing = f edge
switchEdgeFlow _ f (Graph.EDirEdge edge) (Just flow) = f edge flow
switchEdgeFlow _ _ _ _ =
   error $
      "switchEdgeFlow: undirEdge's flow must be Nothing," ++
      " dirEdge's flow must be Just"

mapFlowWithVar ::
   (Idx.InPart part Var.Signal node -> v0 -> v1) ->
   part -> Graph.DirEdge node -> Flow v0 -> Flow v1
mapFlowWithVar f part e =
   liftA2 f
      (Idx.InPart part <$>
       (flowVars <*> pure (Topo.structureEdgeFromDirEdge e)))

flowVars :: Flow (Idx.StructureEdge node -> Var.Signal node)
flowVars =
   Flow {
      flowPowerOut = Var.signalIndex . Idx.Power,
      flowPowerIn = Var.signalIndex . Idx.Power . Idx.flip,
      flowEnergyOut = Var.signalIndex . Idx.Energy,
      flowEnergyIn = Var.signalIndex . Idx.Energy . Idx.flip,
      flowXOut = Var.signalIndex . Idx.X,
      flowXIn = Var.signalIndex . Idx.X . Idx.flip,
      flowEta = Var.signalIndex . Idx.Eta
   }


lookupEdge ::
   Ord n =>
   (el -> a) ->
   Idx.StructureEdge n ->
   Graph n Graph.EitherEdge nl (Maybe el) ->
   Maybe a
lookupEdge f se =
   fmap (maybe (error "lookupEdge: directed edge must have Just label") f) .
   Graph.lookupEdge (Graph.EDirEdge $ Topo.dirEdgeFromStructureEdge se)


dirFromGraph ::
   (Ord n) =>
   Graph n Graph.EitherEdge nl el -> Graph n Graph.DirEdge nl el
dirFromGraph =
   Graph.mapEdgesMaybe $ \ee ->
      case ee of
         Graph.EDirEdge de -> Just de
         Graph.EUnDirEdge _ -> Nothing

dirFromFlowGraph ::
   (Ord n) =>
   Graph n Graph.EitherEdge nl (Maybe el) -> Graph n Graph.DirEdge nl el
dirFromFlowGraph =
   Graph.mapEdge
      (fromMaybe (error "dirFromFlowGraph: directed edge must have Just label"))
   .
   dirFromGraph


dirFromSums :: Sums v -> Maybe Topo.StoreDir
dirFromSums sums =
   case (sumIn sums, sumOut sums) of
      (Nothing, Nothing) -> Nothing
      (Just _, Nothing) -> Just Topo.In
      (Nothing, Just _) -> Just Topo.Out
      (Just _, Just _) -> error "storage cannot be both In and Out"
