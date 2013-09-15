
module EFA.Flow.SystemEta where

import EFA.Application.Utility (checkDetermined)

import qualified EFA.Flow.Sequence.Quantity as SeqFlow

import qualified EFA.Graph.Topology.Node as Node

import qualified EFA.Equation.Arithmetic as Arith
import EFA.Equation.Arithmetic ((~+), (~/))
import EFA.Equation.Result (Result(..))

import qualified EFA.Graph as Graph

import qualified Data.Set as Set
import qualified Data.Map as Map


import Control.Applicative (liftA2)

import Data.Foldable (foldMap)

import Data.Maybe.HT (toMaybe)


hasStructureEdge :: Set.Set a -> Bool
hasStructureEdge = not . Set.null


etaSys ::
   (Node.C node, Arith.Product v) =>
   SeqFlow.Graph node a (Result v) -> Result v
etaSys gr =
   let sq = fmap (Graph.nodeLabels . snd . snd) $ SeqFlow.sequence gr
       sinks =
          fmap
             (Map.mapMaybe SeqFlow.sumIn .
              Map.filterWithKey (\node _ -> Node.isSink $ Node.typ node)) sq
       sources =
          fmap
             (Map.mapMaybe SeqFlow.sumOut .
              Map.filterWithKey (\node _ -> Node.isSource $ Node.typ node)) sq
       sumRes =
          foldl1 (liftA2 (~+)) . map SeqFlow.flowSum . foldMap Map.elems

   in  liftA2 (~/) (sumRes sinks) (sumRes sources)


detEtaSys ::
   (Node.C node, Arith.Product v) =>
   SeqFlow.Graph node a (Result v) -> v
detEtaSys =
   checkDetermined "detEtaSys" . etaSys


type Condition node a v = SeqFlow.Graph node a (Result v) -> Bool

type Forcing node a v = SeqFlow.Graph node a (Result v) -> v


objectiveFunction ::
   (Node.C node, Arith.Product v) =>
   Condition node a v ->
   Forcing node a v ->
   SeqFlow.Graph node a (Result v) ->
   Maybe v
objectiveFunction cond forcing env =
   toMaybe (cond env) $ detEtaSys env ~+ forcing env
