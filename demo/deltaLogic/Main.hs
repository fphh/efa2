{-# LANGUAGE Rank2Types #-}
module Main where

import qualified EFA.Example.Topology.LinearOne as LinearOne
import EFA.Example.Topology.LinearOne (Node(Source, Sink))

import qualified EFA.Application.Symbolic as Symbolic
import EFA.Application.Symbolic ((=<>))
import EFA.Application.Utility (seqFlowGraphFromTopology)

import qualified EFA.Flow.Sequence.AssignMap as AssignMap
import qualified EFA.Flow.Sequence.EquationSystem as EqSys
import qualified EFA.Flow.Sequence.Quantity as SeqFlow
import qualified EFA.Flow.Sequence.Index as XIdx
import EFA.Flow.Sequence.EquationSystem ((=%=))

import qualified EFA.Symbolic.SumProduct as SumProduct

import qualified EFA.Equation.Record as Record
import EFA.Equation.Arithmetic ((~*))

import qualified EFA.Graph.Topology.Index as Idx

import qualified EFA.Report.Format as Format

import qualified System.IO as IO
import Data.Monoid (mempty, (<>))


sec0 :: Idx.Section
sec0 = Idx.Section 0


{-
Use SumProduct.Term here since it simplifies automatically.
-}
given, sys ::
   Symbolic.EquationSystem Symbolic.Ignore
      Record.Delta Node s SumProduct.Term
given =
   Idx.before (XIdx.power sec0 Source Sink) =<>
   Idx.before (XIdx.eta sec0 Source Sink) =<>

   Idx.after (XIdx.eta sec0 Source Sink) =<>

   Idx.delta (XIdx.power sec0 Source Sink) =<>

   mempty

sys =
   EqSys.variableRecord (XIdx.power sec0 Sink Source) =%=
      EqSys.variableRecord (XIdx.eta sec0 Source Sink) ~*
      EqSys.variableRecord (XIdx.power sec0 Source Sink)

run ::
   (forall s.
    Symbolic.EquationSystem Symbolic.Ignore
       Record.Delta Node s SumProduct.Term) ->
   IO ()
run x =
   putStrLn $ Format.unUnicode $ Format.lines $
   AssignMap.format $ SeqFlow.toAssignMap $
   EqSys.solve (seqFlowGraphFromTopology LinearOne.topology) x


main :: IO ()
main = do
   IO.hSetEncoding IO.stdout IO.utf8

   run given
   run (given <> sys)
   run (sys <> given)
