
module EFA.Flow.SystemEta where


import qualified EFA.Flow.Topology.Quantity as FlowTopo

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~/))
import EFA.Equation.Result (Result(Determined))

import qualified EFA.Graph as Graph

import qualified Data.Map as Map

import Control.Applicative (liftA2)

import Data.Foldable (Foldable, foldMap)


etaSys ::
   (Node.C n, Graph.Edge e, Ord (e n), Arith.Constant v,
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
          foldl (liftA2 (~+)) (Determined Arith.zero) . foldMap Map.elems

   in  liftA2 (~/) (sumRes sinks) (sumRes sources)
