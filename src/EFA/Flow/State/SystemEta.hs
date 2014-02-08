

module EFA.Flow.State.SystemEta where

import EFA.Application.Utility (checkDetermined)
import qualified EFA.Application.Sweep as Sweep

import qualified EFA.Flow.State.Quantity as StateQty
import qualified EFA.Flow.SystemEta as SystemEta
import qualified EFA.Flow.Topology.Quantity as FlowTopo
import qualified EFA.Flow.Storage as Storage
import qualified EFA.Flow.Part.Map as PartMap

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+))
import EFA.Equation.Result (Result(Determined, Undetermined))

import EFA.Utility.Map (Caller)

import qualified Data.Map as Map

import Data.Maybe.HT (toMaybe)
import qualified EFA.Graph as Graph


import Control.Applicative (liftA2)

import Data.Foldable (Foldable, foldMap)


etaSys ::
   (Node.C node, Arith.Product v) =>
   StateQty.Graph node a (Result v) -> Result v
etaSys =
   SystemEta.etaSys . fmap FlowTopo.topology . StateQty.states



etaSys2 ::
  (Ord node, Node.C node,
   Arith.Constant a,
   Arith.Product (sweep vec a),
   Sweep.SweepClass sweep vec a) =>
  StateQty.Graph node (Result (sweep vec a)) (Result (sweep vec a)) ->
  Result (sweep vec a)
etaSys2 sq =
   let es = fmap FlowTopo.topology $ StateQty.states sq

       x = Map.elems $ fmap (PartMap.init . Storage.nodes) (StateQty.storages sq)
       y = Map.elems $ fmap (PartMap.exit . Storage.nodes) (StateQty.storages sq)

       err str = error ("EFA.Flow.State.SystemEta.etaSys2: " ++ str)

       (s, t) = case zipWith (liftA2 (Arith.~-)) x y of
                     [] -> err "empty list"
                     Undetermined:_ -> err "Undetermined"
                     s0@(Determined t0):_ -> (s0, t0)

       w = liftA2 (Arith.~*) 
             (Determined $ Sweep.replicate t (Arith.fromRational 20)) s

       nodes = fmap Graph.nodeLabels es

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

   in liftA2 (Arith.~/) (sumRes sinks) (liftA2 (Arith.~+) w (sumRes sources))



detEtaSys ::
   (Node.C node, Arith.Product v) =>
   Caller ->
   StateQty.Graph node a (Result v) -> v
detEtaSys caller =
   checkDetermined (caller ++ ".detEtaSys") . etaSys


type Condition node a v = StateQty.Graph node a (Result v) -> Bool

type Forcing node a v = StateQty.Graph node a (Result v) -> v

objectiveFunction ::
   (Node.C node, Arith.Product v) =>
   Condition node a v ->
   Forcing node a v ->
   StateQty.Graph node a (Result v) ->
   Maybe (v, v)
objectiveFunction cond forcing env =
   let eta = detEtaSys "objectiveFunction" env
   in  toMaybe (cond env) $ (eta ~+ forcing env, eta)
