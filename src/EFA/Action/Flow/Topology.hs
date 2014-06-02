module EFA.Action.Flow.Topology where

import qualified EFA.Action.Flow as ActFlow
import qualified EFA.Graph.Topology.Node as Node
import EFA.Equation.Result (Result)
import qualified EFA.Equation.Arithmetic as Arith

import qualified EFA.Flow.Topology.Quantity as TopoQty

-- TODO:: Check function with Values
etaSys ::
   (Node.C node, Arith.Product v) =>
   TopoQty.Section node (Result v) -> Result v
etaSys =
   ActFlow.etaSys . fmap TopoQty.topology . Just
