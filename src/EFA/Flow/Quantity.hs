{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.Quantity where

import qualified EFA.Graph.Topology.Index as Idx
import qualified EFA.Graph.Topology as Topo
import qualified EFA.Graph as Gr

import qualified EFA.Equation.Variable as Var

import Control.Applicative (Applicative, pure, liftA2, (<*>), (<$>))

import qualified Data.Foldable as Fold

import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable)
import Data.Maybe (fromMaybe)

import Prelude hiding (lookup, init, sin, sum)


type
   DirTopology node a v =
      Gr.Graph node Gr.DirEdge (Sums a v) (Flow v)

type
   Topology node a v =
      Gr.Graph node Gr.EitherEdge (Sums a v) (Maybe (Flow v))

data Flow v =
   Flow {
      flowPowerOut, flowEnergyOut, flowXOut,
      flowEta,
      flowXIn, flowEnergyIn, flowPowerIn :: v
   }
   deriving (Eq)

data Sums a v =
   Sums { sumIn, sumOut :: Maybe (Sum a v) }
   deriving (Eq)

data Sum a v =
   Sum { carrySum :: a, flowSum :: v }
   deriving (Eq)


class Functor f => Carry f where
   carryEnergy, carryXOut, carryXIn :: f a -> a


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



mapSums ::
   (a0 -> a1) ->
   (v0 -> v1) ->
   Sums a0 v0 -> Sums a1 v1
mapSums f g s =
   Sums {
      sumIn  = fmap (mapSum f g) $ sumIn  s,
      sumOut = fmap (mapSum f g) $ sumOut s
   }

mapSum ::
   (a0 -> a1) ->
   (v0 -> v1) ->
   Sum a0 v0 -> Sum a1 v1
mapSum f g s =
   Sum {
      carrySum = f $ carrySum s,
      flowSum  = g $ flowSum  s
   }


traverseSums ::
   (Applicative f) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Sums a0 v0 -> f (Sums a1 v1)
traverseSums f g (Sums i o) =
   liftA2 Sums
      (traverse (traverseSum f g) i)
      (traverse (traverseSum f g) o)

traverseSum ::
   (Applicative f) =>
   (a0 -> f a1) ->
   (v0 -> f v1) ->
   Sum a0 v0 -> f (Sum a1 v1)
traverseSum f g (Sum cs fs) =
   liftA2 Sum (f cs) (g fs)


mapFlowTopologyWithVar ::
   (Ord node) =>
   (Var.ForNodeScalar part node -> a0 -> a1) ->
   (Var.InPartSignal part node -> v0 -> v1) ->
   part ->
   (v0, Gr.Graph node Gr.EitherEdge (Sums a0 v0) (Maybe (Flow v0))) ->
   (v1, Gr.Graph node Gr.EitherEdge (Sums a1 v1) (Maybe (Flow v1)))
mapFlowTopologyWithVar f g part (dtime, gr) =
   (g (part <~> Idx.DTime) dtime,
    Gr.mapNodeWithKey
       (\n (Sums {sumIn = sin, sumOut = sout}) ->
          Sums {
             sumIn =
                flip fmap sin $ \(Sum {carrySum = cs, flowSum = fs}) ->
                   Sum
                      (f (Idx.StOutSum (Idx.NoInit part) <#> n) cs)
                      (g (part <~> Idx.Sum Idx.In n) fs),
             sumOut =
                flip fmap sout $ \(Sum {carrySum = cs, flowSum = fs}) ->
                   Sum
                      (f (Idx.StInSum (Idx.NoExit part) <#> n) cs)
                      (g (part <~> Idx.Sum Idx.Out n) fs)
          }) $
    Gr.mapEdgeWithKey (liftEdgeFlow $ mapFlowWithVar g part) gr)

liftEdgeFlow ::
   (Gr.DirEdge node -> flow0 -> flow1) ->
   Gr.EitherEdge node -> Maybe flow0 -> Maybe flow1
liftEdgeFlow f =
   switchEdgeFlow (const Nothing) (\edge flow -> Just $ f edge flow)

switchEdgeFlow ::
   (Gr.UnDirEdge node -> a) ->
   (Gr.DirEdge node -> flow -> a) ->
   Gr.EitherEdge node -> Maybe flow -> a
switchEdgeFlow f _ (Gr.EUnDirEdge edge) Nothing = f edge
switchEdgeFlow _ f (Gr.EDirEdge edge) (Just flow) = f edge flow
switchEdgeFlow _ _ _ _ =
   error $
      "switchEdgeFlow: undirEdge's flow must be Nothing," ++
      " dirEdge's flow must be Just"

mapFlowWithVar ::
   (Idx.InPart part Var.Signal node -> v0 -> v1) ->
   part -> Gr.DirEdge node -> Flow v0 -> Flow v1
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
   Gr.Graph n Gr.EitherEdge nl (Maybe el) ->
   Maybe a
lookupEdge f se =
   fmap (maybe (error "lookupEdge: directed edge must have Just label") f) .
   Gr.lookupEdge (Gr.EDirEdge $ Topo.dirEdgeFromStructureEdge se)


dirFromGraph ::
   (Ord n) =>
   Gr.Graph n Gr.EitherEdge nl el -> Gr.Graph n Gr.DirEdge nl el
dirFromGraph =
   Gr.mapEdgesMaybe $ \ee ->
      case ee of
         Gr.EDirEdge de -> Just de
         Gr.EUnDirEdge _ -> Nothing

dirFromFlowGraph ::
   (Ord n) =>
   Gr.Graph n Gr.EitherEdge nl (Maybe el) -> Gr.Graph n Gr.DirEdge nl el
dirFromFlowGraph =
   Gr.mapEdge
      (fromMaybe (error "dirFromFlowGraph: directed edge must have Just label"))
   .
   dirFromGraph


(<#>) ::
   (Var.ScalarIndex idx, Var.ScalarPart idx ~ part) =>
   idx node -> node -> Var.ForNodeScalar part node
(<#>) idx node = Idx.ForNode (Var.scalarIndex idx) node

(<~>) ::
   (Var.SignalIndex idx) =>
   part -> idx node -> Var.InPartSignal part node
(<~>) part idx = Idx.InPart part $ Var.signalIndex idx
