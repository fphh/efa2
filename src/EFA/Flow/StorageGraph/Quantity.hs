{-# LANGUAGE TypeFamilies #-}
module EFA.Flow.StorageGraph.Quantity where

import qualified EFA.Equation.Variable as Var

import qualified EFA.Graph.Topology.Index as Idx

import Control.Applicative (Applicative, pure, liftA2, (<*>), (<$>))
import Data.Foldable (Foldable)


class (Applicative f, Foldable f) => Carry f where
   carryEnergy, carryXOut, carryXIn :: f a -> a

   type CarryPart f :: *
   carryVars ::
      (CarryPart f ~ part) =>
      f (Idx.StorageEdge part node -> Var.Scalar part node)


mapCarryWithVar ::
   (Carry carry, CarryPart carry ~ part) =>
   (Var.ForNodeScalar part node -> a0 -> a1) ->
   node -> Idx.StorageEdge part node -> carry a0 -> carry a1
mapCarryWithVar f node edge =
   liftA2 f (Idx.ForNode <$> (carryVars <*> pure edge) <*> pure node)
