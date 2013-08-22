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

import Prelude hiding (lookup, init, sin, sum)


type
   Topology node a v =
      Gr.Graph node Gr.DirEdge (Sums a v) (Flow v)

data Flow v =
   Flow {
      flowPowerOut, flowEnergyOut, flowXOut,
      flowEta,
      flowXIn, flowEnergyIn, flowPowerIn :: v
   }

data Sums a v =
   Sums { sumIn, sumOut :: Maybe (Sum a v) }

data Sum a v =
   Sum { carrySum :: a, flowSum :: v }


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
   (v0, Gr.Graph node Gr.DirEdge (Sums a0 v0) (Flow v0)) ->
   (v1, Gr.Graph node Gr.DirEdge (Sums a1 v1) (Flow v1))
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
    Gr.mapEdgeWithKey
       (\e ->
          liftA2 g
             (Idx.InPart part <$>
              (flowVars <*> pure (Topo.structureEdgeFromDirEdge e))))
       gr)


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


(<#>) ::
   (Var.ScalarIndex idx, Var.ScalarPart idx ~ part) =>
   idx node -> node -> Var.ForNodeScalar part node
(<#>) idx node = Idx.ForNode (Var.scalarIndex idx) node

(<~>) ::
   (Var.SignalIndex idx) =>
   part -> idx node -> Var.InPartSignal part node
(<~>) part idx = Idx.InPart part $ Var.signalIndex idx
