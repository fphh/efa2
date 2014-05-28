
module EFA.Action.Flow where


import qualified EFA.Flow.Topology.Quantity as FlowTopo

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~/))
import EFA.Equation.Result (Result)

import qualified EFA.Graph as Graph

import qualified Data.Map as Map

import Control.Applicative (liftA2)

import Data.Foldable (Foldable, foldMap)

-- | TODO: include storages with a reuse efficiency -- deliver two values -- with & without reuse efficiency
etaSys ::
   (Node.C n, Graph.Edge e, Ord (e n),
    Arith.Sum v, Arith.Product v,
    Functor f, Foldable f) =>
   f (Graph.Graph n e (FlowTopo.Sums (Result v)) el) -> Result v
etaSys sq =
   let nodes = fmap Graph.nodeLabels sq
       sinks =
          fmap
             (Map.mapMaybe FlowTopo.sumIn .
              Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node)) nodes
       sources =
          fmap
             (Map.mapMaybe FlowTopo.sumOut .
              Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node)) nodes
       sumRes =
          foldl1 (liftA2 (~+)) . foldMap Map.elems

   in liftA2 (~/) (sumRes sinks) (sumRes sources)

-- | TODO: write function for overall system loss -- deliver two values -- with & without reuse efficiency