module EFA.Flow.Quantity where

import qualified EFA.Graph as Gr

import Control.Applicative (Applicative, pure, liftA2, (<*>))

import qualified Data.Foldable as Fold

import Data.Traversable (Traversable, traverse, foldMapDefault)
import Data.Foldable (Foldable)


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
