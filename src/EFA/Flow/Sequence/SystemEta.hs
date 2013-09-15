
module EFA.Flow.Sequence.SystemEta where

import EFA.Application.Utility (checkDetermined)

import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.SystemEta as SystemEta

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+))
import EFA.Equation.Result (Result(..))

import EFA.Utility.Map (Caller)

import Data.Maybe.HT (toMaybe)


etaSys ::
   (Node.C node, Arith.Constant v) =>
   SeqFlow.Graph node a (Result v) -> Result v
etaSys =
   SystemEta.etaSys . fmap (snd . snd) . SeqFlow.sequence


detEtaSys ::
   (Node.C node, Arith.Constant v) =>
   Caller ->
   SeqFlow.Graph node a (Result v) -> v
detEtaSys caller =
   checkDetermined (caller ++ ".detEtaSys") . etaSys


type Condition node a v = SeqFlow.Graph node a (Result v) -> Bool

type Forcing node a v = SeqFlow.Graph node a (Result v) -> v


objectiveFunction ::
   (Node.C node, Arith.Constant v) =>
   Condition node a v ->
   Forcing node a v ->
   SeqFlow.Graph node a (Result v) ->
   Maybe v
objectiveFunction cond forcing env =
   toMaybe (cond env) $ detEtaSys "objectiveFunction" env ~+ forcing env
